# cia-data-requests

A local Shiny app for building, previewing and delivering aggregated data
requests from the CIA (Catch In Areas) database. Haul-level catch is apportioned
onto a spatial grid, the confidentiality rule (records must aggregate at least
3 distinct vessels) is applied automatically, and approved datasets are
packaged into a GeoPackage and uploaded to Google Drive.

**Status:** early development. See [docs/PLAN.md](docs/PLAN.md) for the design
and build plan.

## Setup (planned)

1. Copy `.env.example` to `.env` and fill in the database connection.
2. `docker compose up -d app`
3. Open <http://127.0.0.1:3838>.

No data or credentials are stored in this repository.
