# cia-data-requests — design and implementation plan

A local Shiny app for building, previewing, packaging and delivering aggregated
data requests from the CIA (Catch In Areas) database, with the confidentiality
rule enforced automatically.

## Goals

- Specify a data request as **filters** + **grouping** per dataset, without
  hand-writing SQL.
- Apportion haul-level catch onto a spatial grid and apply the confidentiality
  rule (currently: each output record aggregates >= 3 distinct vessels) in
  every query, with no way to skip it.
- Preview results and a suppression report before approving.
- Package approved datasets into a single GeoPackage, write the SQL and a
  README, and upload to Google Drive — replacing the existing CLI pipeline.
- Keep a durable history of every request and run.

Non-goals (for now): statistical disclosure control beyond the vessel rule
(e.g. noise injection), dominance or ownership rules, automated Drive folder
creation/sharing, long-form code descriptions.

## Source data

All names lowercase.

| Table | Key | Relevant columns |
|---|---|---|
| `hauls` | `haul_id` | `vessel_id`, `haul_polygon`, `haul_type`, `year`, `gear_code`, `management_program_code`, `trip_target_code`, `harvest_sector`, `processing_sector` |
| `haul_catch` | `haul_id` (FK, many per haul) | `species_code`, `species_group_code`, `weight` (metric tons), `count` |
| `data.hex_8km`, `data.hex_20km` | `hexid` | `shape` (EPSG:3338) |

`haul_polygon` is the haul's footprint in EPSG:3338: a 10 m buffer of the GPS
track for GPS-derived hauls, or the whole stat area for `haul_type` = area hauls.

> The schema of `hauls` / `haul_catch` is configurable; it defaults to `data`.

## Query semantics

```
hauls ── haul-level filters
  └─ apportion: haul_polygon ∩ grid cell
       p_raw = area(intersection) / area(haul_polygon)
       drop overlaps with p_raw < 5% (GPS hauls only; AREA hauls exempt;
         a haul with no overlap >= 5% keeps all of its overlaps)
       p     = p_raw / sum(p_raw) over haul      -- renormalise (trimming, grid edge, fp error)
         └─ join haul_catch ── catch-level filters (species_code, species_group_code)
              └─ catch_weight_mt = weight * p, catch_count = count * p
                   └─ GROUP BY grid cell + grouping fields
                      HAVING <confidentiality rules>    -- counted after the catch join
```

- The grid cell (`grid_id`, `grid_shape`) is always part of the grouping.
- Grid columns are renamed when queried: `<id> AS grid_id`, `<geom> AS grid_shape`,
  so any grid with an id and polygon column works.
- Filters: AND across fields; within a field, any of the selected values
  (`IN (...)`); `year` is a range. Empty filter = no restriction.
- A vessel counts toward every cell its haul receives catch in: for GPS hauls,
  cells covering at least 5% of the haul (`apportionment.min_share` in
  `schema.yaml`); for area hauls, every overlapping cell.
- Hauls entirely off the grid lose their catch; this is reported in the
  suppression report as off-grid catch, separately from confidentiality
  suppression.
- Records with a missing species or species group code are never selected by
  a filter on that field (there is nothing to select on), which is intended. This satisfies current policy; a future change (e.g. ignore area
  hauls when counting) is a one-line rule change because `haul_type` is
  available.
- The vessel count is used only in `HAVING`; it is never in the output.
- Output stays in EPSG:3338.

Each dataset produces two queries built from the same CTE chain:

- **deliver** — gated, with geometry; this is what is exported and what is
  written to the `.sql` file given to the recipient.
- **diagnostic** — no geometry, no `HAVING`; returns kept vs dropped totals
  (rows, weight, count) for the suppression report.

SQL is generated from a readable, commented CTE template using `glue_sql` for
safe quoting (not dbplyr), because recipients read these files.

## Configuration

Environment variables (`.env`, gitignored; see `.env.example`):

| Variable | Purpose |
|---|---|
| `CIA_PG_HOST`, `CIA_PG_PORT`, `CIA_PG_DB`, `CIA_PG_USER`, `CIA_PG_PASSWORD` | External Postgres server |
| `CIA_PG_SCHEMA` | Schema for `hauls` / `haul_catch` (default `data`) |
| `CIA_DATA_DIR` | Host folder mounted at `/data` (default `%LOCALAPPDATA%\cia-data-requests`) |
| `CIA_REQUESTS_DB` | SQLite path inside the container (`/data/requests.sqlite`) |
| `CIA_OUTPUT_DIR` | Run outputs inside the container (`/data/outputs`) |
| `CIA_RCLONE_REMOTE` | rclone remote, e.g. `gdrive:` |

Config files (`inst/config/`, no secrets):

- `schema.yaml` — physical table/column names and the field catalogue
  (which fields are filterable/groupable, which table they live on, set vs range).
- `grids.yaml` — grid registry: table, id column, geometry column, SRID,
  optional precomputed allocation table.
- `rules.yaml` — confidentiality rules, structured rather than raw SQL
  (`type: min_distinct`, `column`, `min`, optional `filter` such as
  `{column: haul_type, op: not_in, values: [AREA]}`), with human-readable
  descriptions (also written into the SQL header and README).

All configured identifiers are validated as plain lowercase SQL names so they
can be written unquoted (readable SQL); filter values are always quoted as
literals.

## Storage

**SQLite is the record; the output directory holds artefacts.**

```
requests      id, requester, contact, drive_path, notes, created_at
datasets      id, request_id, name, position, spec_json
runs          id, request_id, kind (preview|package), started_at, finished_at,
              status, error, app_version, gpkg_path, upload_status
run_datasets  run_id, dataset_id, sql_text, duration_s,
              rows_kept, rows_dropped, weight_kept, weight_dropped,
              count_kept, count_dropped
```

- The exact SQL executed is stored per run.
- SQLite holds only specs and aggregate stats — no haul-level data.
- Default (rollback) journal mode, set explicitly: WAL is unreliable over
  Docker Desktop's Windows bind mounts.
- Outputs are kept: `/data/outputs/<request_id>/<run_id>/` containing
  `<request>.gpkg` (one layer per dataset), one `.sql` per dataset, `README.md`.

## Pipeline

1. Execute each dataset's deliver query with `sf::st_read(con, query = ...)`
   (no temp tables), plus its diagnostic query.
2. Write all layers with `sf::st_write()` to a temp GeoPackage; rename into
   place only when every layer succeeds.
3. Write `.sql` files and `README.md` (request summary, filters/grouping per
   dataset, suppression table, units, CRS, confidentiality rule text).
4. `rclone copy` the run folder to the request's Drive path (via `processx`).
   Drive folder creation and sharing stay manual.
5. Each step updates the `runs` row; an interrupted run shows as failed.
   Packaging runs in a background R process so the UI stays responsive.

## Guardrails

- Specs are validated against the config (fields, values, grid).
- **Package is enabled only after a successful preview of the exact current
  specs.** Approval records a hash of the generated deliver SQL (not just the
  spec), so a change to the spec, the config or the query template all
  invalidate it.
- The query builder always appends the rules; specs have no way to disable them.

## App flow

1. **Requests list** — search, last run, status, suppression summary.
2. **Request editor** — metadata, datasets (add / duplicate / reorder / delete).
3. **Dataset editor** — name (layer name), grid, filters, grouping.
4. **Preview** — table + map per dataset, suppression report, generated SQL;
   **Approve**.
5. **Package** — background run with progress.
6. **Run history** — timing, stats, errors, exact SQL; rerun.

## Deployment

Docker Desktop (WSL2) on Windows, via `docker compose`.

- `app` — built on `rocker/geospatial:4.4.2` (R, sf, GDAL, PROJ) + rclone;
  bound to `127.0.0.1:3838`. Volumes:
  - `${CIA_DATA_DIR:-${LOCALAPPDATA}/cia-data-requests}` → `/data`
  - rclone config (to be finalised when the upload stage is tested; likely
    `%APPDATA%\rclone` → `/config/rclone`, writable for token refresh)
- `postgis` — test/dev sidecar (`profiles: [test, dev]`), seeded with a
  synthetic schema identical to production.
- `tests` — runs the test suite against the sidecar (`profiles: [test]`); CI
  runs the same command.
- `app-dev` — the app against the sidecar with source mounted live
  (`profiles: [dev]`, port 3839).

Package versions: versioned rocker images pin CRAN to a dated Posit Package
Manager snapshot, so packages installed in the Dockerfile resolve to fixed
versions. This replaces renv.

## Testing

Synthetic data in the PostGIS sidecar: line hauls buffered 10 m plus area hauls
covering whole stat-area squares, a mix of vessels so some cells fall below the
threshold, and a generated hex grid in EPSG:3338.

- **Conservation:** ungated apportioned weight/count summed per haul equals the
  original.
- **Gate correctness:** an independent `sf` computation confirms no delivered
  row aggregates < 3 vessels, and the dropped set is exactly right.
- **Gate non-bypassable:** no spec produces SQL without the rules.
- **SQL snapshots** for representative specs.
- **Store:** CRUD; editing a spec invalidates approval.
- **End-to-end:** `package_request()` with rclone pointed at a local folder;
  the GeoPackage opens with the right layers in EPSG:3338.

## Build order

1. ~~Repo scaffold, Docker/compose, `.env.example`, synthetic PostGIS seed, CI.~~ Done.
2. ~~Config + spec + SQL builder, with conservation and gate tests.~~ Done.
3. ~~Execution + suppression stats.~~ Done.
4. SQLite store.
5. Packager, README, rclone upload — full pipeline usable from the R console.
6. Shiny UI.
7. Benchmark on-the-fly join vs precomputed allocation on real data; polish.

## Future

- Long-form code descriptions from lookup tables.
- Ownership-based rule (e.g. >= 2 distinct owners) and/or excluding area hauls
  from vessel counts.
- Statistical disclosure control (noise) for overlapping requests.
- Automated Drive folder creation and sharing.
