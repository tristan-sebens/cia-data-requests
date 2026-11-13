# cia-data-requests

A local Shiny app for building, previewing and delivering aggregated data
requests from the CIA (Catch In Areas) database. Haul-level catch is apportioned
onto a spatial grid, the confidentiality rule (records must aggregate at least
3 distinct vessels) is applied automatically, and approved datasets are
packaged into a GeoPackage and uploaded to Google Drive.

**Status:** feature-complete for a first trial (build steps 1–6 of 7 done). See
[docs/PLAN.md](docs/PLAN.md) for the design and build plan.

No data or credentials are stored in this repository.

## Requirements

Docker Desktop (WSL2 backend on Windows).

## Running

1. Copy `.env.example` to `.env` and fill in the database connection and the
   upload root (`CIA_RCLONE_REMOTE`). Check where your rclone config lives with
   `rclone config file`; if it is not `%APPDATA%\rclone`, set `RCLONE_CONFIG_DIR`.
2. Build and start the app:

   ```bash
   docker compose up -d --build app
   ```

3. Open <http://127.0.0.1:3838>.

## Logs and troubleshooting

The app logs every action, database query and background job, with timings, to
the console:

```bash
docker compose logs -f app
```

Each line is tagged with the browser session (`[session 1a2b3c]`) or the
background job (`[job preview 12]`) it came from. Anything slower than expected
is logged as `WARN ... (slow)`. For more detail (every read of the request
store, and call stacks for errors), set `CIA_LOG_LEVEL=DEBUG` in `.env` and
restart with `docker compose up -d app`. Logs contain names, ids, counts and
timings only, never catch data or passwords.

At start-up the log shows the settings in use and checks the request store
and the Postgres connection. Filter values for the dataset editor are loaded
from Postgres the first time the editor is opened after start-up (this can
take a while on the full tables) and then kept until the app restarts.

## Workflow

1. **New request**: title, requester, and the Drive folder (create and share
   it in Drive first).
2. **Datasets**: add one per layer the recipient wants (filters + grouping);
   duplicate to make variants.
3. **Preview**: check records, suppression, the map and the SQL; **Approve**.
4. **Package**: builds the GeoPackage, SQL files and README, and uploads them.

Any edit after approval requires a new preview and approval before packaging.

The SQLite request database and all outputs are stored in
`%LOCALAPPDATA%\cia-data-requests` (override with `CIA_DATA_DIR` in `.env`).

## Development

Tests run against a PostGIS sidecar seeded with synthetic data
(`docker/postgis-init/`), never against the real database:

```bash
docker compose --profile test run --rm tests
```

Run the app against the synthetic data, with source mounted live, at
<http://127.0.0.1:3839> ("uploads" go to a local folder inside the dev volume,
not Google Drive):

```bash
docker compose --profile dev up
```

The synthetic database is also reachable from pgAdmin at `127.0.0.1:55432`
(database `cia_test`, user/password `cia`).

If your network uses a TLS-inspecting proxy, pass its CA certificate to the build:

```bash
docker build --secret id=extra_ca,src=path/to/ca.crt -t cia-data-requests:latest .
```
