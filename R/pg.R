#' Connect to the CIA Postgres database
#'
#' Connection settings come from environment variables (see `.env.example`):
#' `CIA_PG_HOST`, `CIA_PG_PORT`, `CIA_PG_DB`, `CIA_PG_USER`, `CIA_PG_PASSWORD`.
#'
#' @return A DBI connection.
#' @export
cia_connect <- function() {
  host <- require_env("CIA_PG_HOST")
  port <- as.integer(Sys.getenv("CIA_PG_PORT", "5432"))
  dbname <- require_env("CIA_PG_DB")
  user <- require_env("CIA_PG_USER")
  log_timed(sprintf("Connect to Postgres %s@%s:%d/%s", user, host, port, dbname), level = "INFO",
    DBI::dbConnect(
      RPostgres::Postgres(),
      host     = host,
      port     = port,
      dbname   = dbname,
      user     = user,
      password = Sys.getenv("CIA_PG_PASSWORD"),
      # Fail rather than hang if the server is unreachable.
      connect_timeout = as.integer(Sys.getenv("CIA_PG_CONNECT_TIMEOUT", "15")),
      application_name = "cia-data-requests"
    ))
}

#' Schema holding `hauls` and `haul_catch`
#' @noRd
cia_schema <- function() {
  Sys.getenv("CIA_PG_SCHEMA", "data")
}

#' Read a required environment variable, failing clearly if it is unset
#' @noRd
require_env <- function(name) {
  value <- Sys.getenv(name)
  if (!nzchar(value)) {
    stop("Environment variable ", name, " is not set. See .env.example.", call. = FALSE)
  }
  value
}
