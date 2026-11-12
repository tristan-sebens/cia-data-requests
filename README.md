# cia-data-requests

A local Shiny app for building, previewing and delivering aggregated data
requests from the CIA (Catch In Areas) database. Haul-level catch is apportioned
onto a spatial grid, the confidentiality rule (records must aggregate at least
3 distinct vessels) is applied automatically, and approved datasets are
packaged into a GeoPackage and uploaded to Google Drive.

**Status:** early development (build step 1 of 7 done). See
[docs/PLAN.md](docs/PLAN.md) for the design and build plan.

No data or credentials are stored in this repository.

## Requirements

Docker Desktop (WSL2 backend on Windows).

## Running

1. Copy `.env.example` to `.env` and fill in the database connection.
2. Build and start the app:

   ```bash
   docker compose up -d --build app
   ```

3. Open <http://127.0.0.1:3838>.

The SQLite request database and all outputs are stored in
`%LOCALAPPDATA%\cia-data-requests` (override with `CIA_DATA_DIR` in `.env`).

## Development

Tests run against a PostGIS sidecar seeded with synthetic data
(`docker/postgis-init/`), never against the real database:

```bash
docker compose --profile test run --rm tests
```

Run the app against the synthetic data, with source mounted live, at
<http://127.0.0.1:3839>:

```bash
docker compose --profile dev up
```

The synthetic database is also reachable from pgAdmin at `127.0.0.1:55432`
(database `cia_test`, user/password `cia`).

If your network uses a TLS-inspecting proxy, pass its CA certificate to the build:

```bash
docker build --secret id=extra_ca,src=path/to/ca.crt -t cia-data-requests:latest .
```
