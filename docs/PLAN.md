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
requests      id, title, requester, contact, drive_path, notes, archived,
              created_at, updated_at
datasets      id, request_id, name, position, spec_json,
              approved_sql_hash, approved_run_id, approved_at, created_at, updated_at
runs          id, request_id, kind (preview|package), status (running|succeeded|failed),
              started_at, finished_at, error, app_version, output_dir, gpkg_path,
              upload_status, upload_error, uploaded_at
run_datasets  run_id, dataset_id, dataset_name, position, spec_json, sql_text,
              sql_hash, error, duration_s, rows_kept, rows_dropped, weight_kept,
              weight_dropped, count_kept, count_dropped, weight_off_grid, count_off_grid
```

- Schema changes are numbered migrations tracked with `PRAGMA user_version`;
  an app older than its store refuses to open it.
- `run_datasets` copies each dataset's name, spec and SQL, so history stays
  intact if a dataset is later edited or deleted.
- Requests that have been packaged cannot be deleted, only archived.
- On app start-up, runs left `running` are marked failed ("interrupted").
- Previewing fails a dataset whose filters use values absent from the
  database (almost always a typo); the run then cannot be approved.

- The exact SQL executed is stored per run.
- SQLite holds only specs and aggregate stats — no haul-level data.
- Default (rollback) journal mode, set explicitly: WAL is unreliable over
  Docker Desktop's Windows bind mounts.
- Outputs are kept: `/data/outputs/<request_id>/<run_id>/` containing
  `<request>.gpkg` (one layer per dataset), one `.sql` per dataset, `README.md`.

## Pipeline

`package_request()` (requires every dataset approved against its current SQL):

1. Re-run each dataset (deliver + diagnostic); refuse if its SQL no longer
   matches the approved hash. Results recorded as a `package` run.
2. Write into `run_<id>.partial/`: `<request>.gpkg` (one layer per dataset,
   via `sf::st_write`), `NN_<dataset>.sql`, `README.md`. Rename to `run_<id>/`
   only when everything is written; on failure the partial folder is removed
   and the run marked failed.
3. Upload with `rclone copy` to `<CIA_RCLONE_REMOTE>/<request drive folder>`
   (fixed root; the per-request folder cannot contain `:`, `..` or a leading
   `/`). Same-named files are overwritten. Files this app uploaded in the
   request's previous delivery to the same folder that are no longer produced
   are deleted; nothing else in the folder is touched. Drive folder creation
   and sharing stay manual.
4. Upload status is tracked separately from the build; `upload_run()` retries
   a failed upload without rebuilding.

**README suppression report.** Rows withheld are always reported. Withheld
catch weight/count (and percentages) are reported only if the withheld
records, taken together, satisfy the confidentiality rules (computed in the
diagnostic query); otherwise publishing them would reveal the catch of the
suppressed groups, since the recipient can sum the delivered rows. Off-grid
catch is recorded internally only.

## Guardrails

- Specs are validated against the config (fields, values, grid).
- **Package is enabled only after a successful preview of the exact current
  specs.** Approval records a hash of the generated deliver SQL (not just the
  spec), so a change to the spec, the config or the query template all
  invalidate it.
- The query builder always appends the rules; specs have no way to disable them.

## App flow

Sidebar: request list (newest first, last run status) and **New request**.
Per request, five tabs:

1. **Details**: title, requester, contact, Drive folder (below the fixed
   upload root), internal notes; archive / delete.
2. **Datasets**: table with filters, grouping and approval status; add, edit,
   duplicate, reorder, delete. The editor offers filter values loaded from the
   database, so codes cannot be mistyped; validation errors stay in the editor.
3. **Preview**: runs every dataset in a background R process (live progress
   log). Per dataset: suppression table, whether the README will report the
   withheld totals, off-grid catch, map of delivered cells, first 500 rows,
   and the SQL. **Approve these results** appears after a successful preview.
4. **Package**: approval checklist, destination, **Package and upload**
   (background, live log), last package run, **Retry upload** on failure.
5. **History**: every run with per-dataset statistics, errors, exact SQL and
   the upload log.

Previews and packaging run in a separate R process (`callr`) so the UI stays
responsive; only one job runs at a time.

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
4. ~~SQLite store.~~ Done.
5. ~~Packager, README, rclone upload — full pipeline usable from the R console.~~ Done.
6. ~~Shiny UI.~~ Done.
7. Performance on real data (in progress):
   - ~~Overlay: compute each haul/cell intersection once (it was computed twice),
     skip it when a haul lies inside one cell or a cell inside a haul, and
     compute haul areas once. ~2.5x faster on the synthetic data.~~
   - ~~Cancel button for running jobs (also cancels the Postgres query).~~
   - Diagnose slow spatially filtered queries on the real tables (EXPLAIN).
   - Benchmark on-the-fly overlay vs precomputed allocation tables.

## Spatial filters

Requests often limit a dataset to (or, rarely, exclude) one or more
management areas.

- **One areas view** (`data.spatial_areas`, a dbt model maintained outside
  this repo) unions every area source: `source`, `area_code`, `shape`
  (EPSG:3338). Business rules live in the view (e.g. only current ADF&G stat
  area definitions). The app reads only those three columns, configured in
  `inst/config/areas.yaml`, which also gives display names and order; sources
  added to the view appear in the app without code changes.
- **Spec:** `spatial_filters`, a list of
  `{mode: include | exclude, source: <source>, codes: [...]}`. Within a filter
  the areas are combined (any of them); separate filters are combined with
  AND, like attribute filters. Datasets reference `(source, code)`, never the
  view's generated `area_id`. Specs without spatial filters keep their
  previous JSON and SQL, so existing approvals stay valid.
- **Applied up front**, in `hauls_f`, before apportionment: include keeps
  hauls that overlap the areas; exclude drops them. Overlap means a shared
  area greater than zero (`ST_Intersects AND NOT ST_Touches`), so hauls that
  only touch a boundary do not count. Hauls are kept or dropped whole (fuzzy
  edges at grid resolution are accepted).
- Each check is written `(EXISTS (...)) IS TRUE`: PostgreSQL badly
  underestimates spatial semi-joins (2 of 2,250 hauls in testing), and the bad
  estimate turned the rest of the plan into nested loops (6 s instead of 2 s).
  Evaluated per haul, each check still uses the area table's spatial index.
- Unknown area codes fail the preview, like unknown attribute codes. The
  README and SQL header describe the filter; the preview map outlines the
  areas (included blue, excluded red).

**Later: custom areas (WKT).** A polygon drawn for a specific request
(exported from ArcMap etc. as WKT). It is defined **once per request** and
stored with the request (name + WKT, validated as a polygon/multipolygon in
EPSG:3338). Datasets reference it by name, as another kind of source:
`source: {request_area: <name>}`. The SQL builder writes the WKT into each
dataset's SQL (`ST_GeomFromText(..., 3338)`), so every `.sql` file still runs
on its own, and editing the shape changes the SQL hash, which withdraws
existing approvals. The include/exclude and filter-list structure above is
designed so this is an addition, not a change.

## Future

- Code labels: each set field has a lookup table (code + readable label).
  Show "code: label" in the editor dropdowns (searchable by either), and in
  the README's filter descriptions. Configure the lookup table per field in
  `schema.yaml`.
- Custom spatial filter areas (WKT), defined once per request (see above).
- Ownership-based rule (e.g. >= 2 distinct owners) and/or excluding area hauls
  from vessel counts.
- Statistical disclosure control (noise) for overlapping requests.
- Automated Drive folder creation and sharing.
