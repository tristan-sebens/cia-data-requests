# Tests that need the database only run against the synthetic PostGIS sidecar.
# The guard checks CIA_PG_DB so they can never run against production by accident.
skip_if_no_test_db <- function() {
  if (!identical(Sys.getenv("CIA_PG_DB"), "cia_test")) {
    skip("Synthetic test database not configured (CIA_PG_DB != 'cia_test').")
  }
}

# Connection that closes automatically when the calling test finishes.
test_con <- function(env = parent.frame()) {
  skip_if_no_test_db()
  con <- cia_connect()
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

q1 <- function(con, sql) DBI::dbGetQuery(con, sql)[[1]]
