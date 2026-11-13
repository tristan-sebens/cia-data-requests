# SQLite store: the record of requests, datasets, runs and approvals.
#
# Holds only specs, SQL text and aggregate statistics; never haul-level data.

# ---------------------------------------------------------------------------
# Connection and migrations
# ---------------------------------------------------------------------------

#' Open the request store
#'
#' Creates the database file if needed and applies any pending schema
#' migrations.
#'
#' @param path SQLite file. Defaults to the `CIA_REQUESTS_DB` environment variable.
#' @return A DBI connection to the store.
#' @export
store_connect <- function(path = Sys.getenv("CIA_REQUESTS_DB")) {
  if (!nzchar(path)) {
    stop("No request store path: set CIA_REQUESTS_DB. See .env.example.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  t0 <- Sys.time()
  store <- DBI::dbConnect(RSQLite::SQLite(), path)
  # Rollback journal, not WAL: WAL's shared-memory file is unreliable on
  # Docker Desktop's Windows bind mounts.
  tryCatch({
    DBI::dbExecute(store, "PRAGMA journal_mode = DELETE")
    DBI::dbExecute(store, "PRAGMA foreign_keys = ON")
    DBI::dbExecute(store, "PRAGMA busy_timeout = 10000")
    store_migrate(store)
  }, error = function(e) {
    log_error("Could not open the request store at ", path, ": ", conditionMessage(e))
    DBI::dbDisconnect(store)
    stop(e)
  })
  log_debug("Opened request store ", path, " in ", fmt_ms(t0))
  store
}

#' Schema migrations, applied in order. Never edit a released migration; add a new one.
#' @noRd
store_migrations <- list(
  # 1: initial schema
  c(
    "CREATE TABLE requests (
       id          INTEGER PRIMARY KEY,
       title       TEXT NOT NULL,
       requester   TEXT NOT NULL,
       contact     TEXT NOT NULL DEFAULT '',
       drive_path  TEXT NOT NULL DEFAULT '',
       notes       TEXT NOT NULL DEFAULT '',
       archived    INTEGER NOT NULL DEFAULT 0,
       created_at  TEXT NOT NULL,
       updated_at  TEXT NOT NULL
     )",
    "CREATE TABLE runs (
       id            INTEGER PRIMARY KEY,
       request_id    INTEGER NOT NULL REFERENCES requests (id) ON DELETE CASCADE,
       kind          TEXT NOT NULL CHECK (kind IN ('preview', 'package')),
       status        TEXT NOT NULL CHECK (status IN ('running', 'succeeded', 'failed')),
       started_at    TEXT NOT NULL,
       finished_at   TEXT,
       error         TEXT,
       app_version   TEXT NOT NULL,
       output_dir    TEXT,
       gpkg_path     TEXT,
       upload_status TEXT CHECK (upload_status IN ('pending', 'running', 'succeeded', 'failed', 'skipped')),
       upload_error  TEXT,
       uploaded_at   TEXT
     )",
    "CREATE INDEX runs_request_idx ON runs (request_id, id)",
    "CREATE TABLE datasets (
       id                INTEGER PRIMARY KEY,
       request_id        INTEGER NOT NULL REFERENCES requests (id) ON DELETE CASCADE,
       name              TEXT NOT NULL,
       position          INTEGER NOT NULL,
       spec_json         TEXT NOT NULL,
       approved_sql_hash TEXT,
       approved_run_id   INTEGER REFERENCES runs (id) ON DELETE SET NULL,
       approved_at       TEXT,
       created_at        TEXT NOT NULL,
       updated_at        TEXT NOT NULL,
       UNIQUE (request_id, name)
     )",
    # One row per dataset per run. Name, spec and SQL are copied so history is
    # intact even if the dataset is later edited or deleted.
    "CREATE TABLE run_datasets (
       run_id          INTEGER NOT NULL REFERENCES runs (id) ON DELETE CASCADE,
       dataset_id      INTEGER REFERENCES datasets (id) ON DELETE SET NULL,
       dataset_name    TEXT NOT NULL,
       position        INTEGER NOT NULL,
       spec_json       TEXT NOT NULL,
       sql_text        TEXT NOT NULL,
       sql_hash        TEXT NOT NULL,
       error           TEXT,
       duration_s      REAL,
       rows_kept       INTEGER,
       rows_dropped    INTEGER,
       weight_kept     REAL,
       weight_dropped  REAL,
       count_kept      REAL,
       count_dropped   REAL,
       weight_off_grid REAL,
       count_off_grid  REAL,
       PRIMARY KEY (run_id, dataset_name)
     )"
  ),
  # 2: packaging
  c(
    "ALTER TABLE run_datasets ADD COLUMN dropped_disclosable INTEGER",
    "ALTER TABLE runs ADD COLUMN upload_dest TEXT",
    "ALTER TABLE runs ADD COLUMN upload_log TEXT"
  )
)

#' @noRd
store_migrate <- function(store) {
  current <- DBI::dbGetQuery(store, "PRAGMA user_version")[[1]]
  target <- length(store_migrations)
  if (current > target) {
    stop("The request store (schema version ", current, ") is newer than this app ",
         "(version ", target, "). Update the app.", call. = FALSE)
  }
  for (v in seq_len(target)[seq_len(target) > current]) {
    DBI::dbWithTransaction(store, {
      for (stmt in store_migrations[[v]]) DBI::dbExecute(store, stmt)
      DBI::dbExecute(store, paste0("PRAGMA user_version = ", v))
    })
  }
  invisible(store)
}

#' Mark runs left in 'running' state as failed
#'
#' Call once at app start-up: any run still 'running' then was interrupted
#' (app stopped, machine shut down). Do not call while a run may be in progress.
#' @return Number of runs marked failed.
#' @export
recover_interrupted_runs <- function(store, request_id = NULL,
                                     reason = "Interrupted: the app stopped while this run was in progress.") {
  if (is.null(request_id)) {
    DBI::dbExecute(store, "UPDATE runs SET status = 'failed', finished_at = ?, error = ?
                           WHERE status = 'running'", params = list(now_utc(), reason))
  } else {
    DBI::dbExecute(store, "UPDATE runs SET status = 'failed', finished_at = ?, error = ?
                           WHERE status = 'running' AND request_id = ?",
                   params = list(now_utc(), reason, request_id))
  }
}

# ---------------------------------------------------------------------------
# Requests
# ---------------------------------------------------------------------------

#' Create, read, update and list requests
#'
#' @param store Connection from [store_connect()].
#' @return `create_request()` returns the new request id.
#' @export
create_request <- function(store, title, requester, contact = "", drive_path = "", notes = "") {
  check_text(title, "title", required = TRUE)
  check_text(requester, "requester", required = TRUE)
  for (x in list(contact, drive_path, notes)) check_text(x, "field")
  t <- now_utc()
  DBI::dbExecute(store, "
    INSERT INTO requests (title, requester, contact, drive_path, notes, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(title, requester, contact, drive_path, notes, t, t))
  last_id(store)
}

#' @rdname create_request
#' @param ... Fields to change: `title`, `requester`, `contact`, `drive_path`,
#'   `notes`, `archived`.
#' @export
update_request <- function(store, request_id, ...) {
  changes <- list(...)
  allowed <- c("title", "requester", "contact", "drive_path", "notes", "archived")
  bad <- setdiff(names(changes), allowed)
  if (length(bad)) stop("Cannot update request field(s): ", paste(bad, collapse = ", "), call. = FALSE)
  if (!length(changes)) return(invisible(FALSE))
  for (f in setdiff(names(changes), "archived")) {
    check_text(changes[[f]], f, required = f %in% c("title", "requester"))
  }
  if (!is.null(changes$archived)) changes$archived <- as.integer(isTRUE(changes$archived))
  get_request(store, request_id)  # errors if missing
  sets <- paste0(names(changes), " = ?", collapse = ", ")
  DBI::dbExecute(store, paste0("UPDATE requests SET ", sets, ", updated_at = ? WHERE id = ?"),
                 params = c(unname(changes), list(now_utc(), request_id)))
  invisible(TRUE)
}

#' @rdname create_request
#' @export
get_request <- function(store, request_id) {
  r <- DBI::dbGetQuery(store, "SELECT * FROM requests WHERE id = ?", params = list(request_id))
  if (!nrow(r)) stop("No request with id ", request_id, call. = FALSE)
  r$archived <- as.logical(r$archived)
  as.list(r)
}

#' @rdname create_request
#' @param include_archived Include archived requests?
#' @return `list_requests()`: a data frame, newest first, with the number of
#'   datasets and the latest run's kind, status and time.
#' @export
list_requests <- function(store, include_archived = FALSE) {
  r <- DBI::dbGetQuery(store, paste0("
    SELECT r.id, r.title, r.requester, r.archived, r.created_at, r.updated_at,
           (SELECT COUNT(*) FROM datasets d WHERE d.request_id = r.id) AS n_datasets,
           lr.kind AS last_run_kind, lr.status AS last_run_status,
           lr.started_at AS last_run_at, lr.upload_status AS last_upload_status
    FROM requests r
    LEFT JOIN runs lr ON lr.id = (SELECT MAX(id) FROM runs WHERE request_id = r.id)",
    if (!include_archived) " WHERE r.archived = 0",
    " ORDER BY r.id DESC"))
  r$archived <- as.logical(r$archived)
  r
}

#' Delete a request
#'
#' Only allowed if the request was never packaged; delivered requests are part
#' of the record and should be archived instead.
#' @export
delete_request <- function(store, request_id) {
  get_request(store, request_id)
  n <- DBI::dbGetQuery(store, "SELECT COUNT(*) FROM runs WHERE request_id = ? AND kind = 'package'",
                       params = list(request_id))[[1]]
  if (n > 0) {
    stop("Request ", request_id, " has been packaged and cannot be deleted; archive it instead.",
         call. = FALSE)
  }
  DBI::dbExecute(store, "DELETE FROM requests WHERE id = ?", params = list(request_id))
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Datasets
# ---------------------------------------------------------------------------

#' Add, update, reorder, duplicate and delete datasets
#'
#' Specs are validated before they are stored. Changing a dataset's spec
#' withdraws its approval.
#'
#' @return `add_dataset()` and `duplicate_dataset()` return the new dataset id.
#' @export
add_dataset <- function(store, request_id, spec, cfg) {
  get_request(store, request_id)
  spec <- validate_spec(spec, cfg)
  t <- now_utc()
  DBI::dbWithTransaction(store, {
    pos <- DBI::dbGetQuery(store, "SELECT COALESCE(MAX(position), 0) + 1 FROM datasets WHERE request_id = ?",
                           params = list(request_id))[[1]]
    with_unique_name(spec$name, DBI::dbExecute(store, "
      INSERT INTO datasets (request_id, name, position, spec_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?)",
      params = list(request_id, spec$name, pos, as.character(spec_to_json(spec)), t, t)))
  })
  touch_request(store, request_id)
  last_id(store)
}

#' @rdname add_dataset
#' @export
update_dataset <- function(store, dataset_id, spec, cfg) {
  old <- get_dataset(store, dataset_id)
  spec <- validate_spec(spec, cfg)
  json <- as.character(spec_to_json(spec))
  if (identical(json, old$spec_json)) return(invisible(FALSE))
  with_unique_name(spec$name, DBI::dbExecute(store, "
    UPDATE datasets SET name = ?, spec_json = ?, updated_at = ?,
                        approved_sql_hash = NULL, approved_run_id = NULL, approved_at = NULL
    WHERE id = ?",
    params = list(spec$name, json, now_utc(), dataset_id)))
  touch_request(store, old$request_id)
  invisible(TRUE)
}

#' @rdname add_dataset
#' @param new_name Name for the copy.
#' @export
duplicate_dataset <- function(store, dataset_id, new_name, cfg) {
  old <- get_dataset(store, dataset_id)
  spec <- old$spec
  spec$name <- new_name
  add_dataset(store, old$request_id, spec, cfg)
}

#' @rdname add_dataset
#' @export
delete_dataset <- function(store, dataset_id) {
  old <- get_dataset(store, dataset_id)
  DBI::dbWithTransaction(store, {
    DBI::dbExecute(store, "DELETE FROM datasets WHERE id = ?", params = list(dataset_id))
    renumber_datasets(store, old$request_id)
  })
  touch_request(store, old$request_id)
  invisible(TRUE)
}

#' @rdname add_dataset
#' @param dataset_ids All of the request's dataset ids, in the new order.
#' @export
reorder_datasets <- function(store, request_id, dataset_ids) {
  current <- get_datasets(store, request_id)$id
  if (!setequal(current, dataset_ids) || length(current) != length(dataset_ids)) {
    stop("reorder_datasets() needs every dataset of the request exactly once.", call. = FALSE)
  }
  DBI::dbWithTransaction(store, {
    for (i in seq_along(dataset_ids)) {
      DBI::dbExecute(store, "UPDATE datasets SET position = ? WHERE id = ?",
                     params = list(i, dataset_ids[[i]]))
    }
  })
  touch_request(store, request_id)
  invisible(TRUE)
}

#' @rdname add_dataset
#' @return `get_dataset()`: a list including the parsed `spec`.
#' @export
get_dataset <- function(store, dataset_id) {
  d <- DBI::dbGetQuery(store, "SELECT * FROM datasets WHERE id = ?", params = list(dataset_id))
  if (!nrow(d)) stop("No dataset with id ", dataset_id, call. = FALSE)
  d <- as.list(d)
  d$spec <- spec_from_json(d$spec_json)
  d
}

#' @rdname add_dataset
#' @return `get_datasets()`: a data frame in position order, with a list
#'   column `spec`.
#' @export
get_datasets <- function(store, request_id) {
  d <- DBI::dbGetQuery(store, "SELECT * FROM datasets WHERE request_id = ? ORDER BY position",
                       params = list(request_id))
  d$spec <- lapply(d$spec_json, spec_from_json)
  d
}

# ---------------------------------------------------------------------------
# Runs
# ---------------------------------------------------------------------------

#' Record runs and their per-dataset results
#'
#' @param kind `"preview"` or `"package"`.
#' @return `start_run()` returns the run id.
#' @export
start_run <- function(store, request_id, kind = c("preview", "package")) {
  kind <- match.arg(kind)
  get_request(store, request_id)
  DBI::dbExecute(store, "
    INSERT INTO runs (request_id, kind, status, started_at, app_version, upload_status)
    VALUES (?, ?, 'running', ?, ?, ?)",
    params = list(request_id, kind, now_utc(), app_version(),
                  if (kind == "package") "pending" else NA_character_))
  last_id(store)
}

#' @rdname start_run
#' @param result A `cia_dataset_result` from [run_dataset()], or `NULL` if the
#'   dataset failed (then give `spec`, `sql` and `error`).
#' @export
record_run_dataset <- function(store, run_id, dataset_id, position, result = NULL,
                               spec = result$spec, sql = result$sql, error = NULL) {
  s <- if (!is.null(result)) result$stats else NULL
  val <- function(x) if (is.null(x)) NA else x
  DBI::dbExecute(store, "
    INSERT INTO run_datasets (run_id, dataset_id, dataset_name, position, spec_json, sql_text,
                              sql_hash, error, duration_s, rows_kept, rows_dropped,
                              weight_kept, weight_dropped, count_kept, count_dropped,
                              weight_off_grid, count_off_grid, dropped_disclosable)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(run_id, val(dataset_id), spec$name, position,
                  as.character(spec_to_json(spec)), sql, sql_hash(sql),
                  if (is.null(error)) NA_character_ else error,
                  val(result$duration_s), val(s$rows_kept), val(s$rows_dropped),
                  val(s$weight_kept), val(s$weight_dropped), val(s$count_kept),
                  val(s$count_dropped), val(s$weight_off_grid), val(s$count_off_grid),
                  if (is.null(s)) NA_integer_ else as.integer(s$dropped_disclosable)))
  invisible(TRUE)
}

#' @rdname start_run
#' @param status `"succeeded"` or `"failed"`.
#' @param ... Other run fields to set: `output_dir`, `gpkg_path`,
#'   `upload_status`, `upload_error`, `uploaded_at`, `upload_dest`, `upload_log`.
#' @export
finish_run <- function(store, run_id, status = c("succeeded", "failed"), error = NULL, ...) {
  status <- match.arg(status)
  DBI::dbExecute(store, "UPDATE runs SET status = ?, finished_at = ?, error = ? WHERE id = ?",
                 params = list(status, now_utc(), if (is.null(error)) NA_character_ else error, run_id))
  if (length(list(...))) update_run(store, run_id, ...)
  invisible(TRUE)
}

#' @rdname start_run
#' @export
update_run <- function(store, run_id, ...) {
  changes <- list(...)
  allowed <- c("output_dir", "gpkg_path", "upload_status", "upload_error", "uploaded_at",
               "upload_dest", "upload_log")
  bad <- setdiff(names(changes), allowed)
  if (length(bad)) stop("Cannot update run field(s): ", paste(bad, collapse = ", "), call. = FALSE)
  if (!length(changes)) return(invisible(FALSE))
  DBI::dbExecute(store, paste0("UPDATE runs SET ", paste0(names(changes), " = ?", collapse = ", "),
                               " WHERE id = ?"),
                 params = c(unname(changes), list(run_id)))
  invisible(TRUE)
}

#' @rdname start_run
#' @export
get_run <- function(store, run_id) {
  r <- DBI::dbGetQuery(store, "SELECT * FROM runs WHERE id = ?", params = list(run_id))
  if (!nrow(r)) stop("No run with id ", run_id, call. = FALSE)
  as.list(r)
}

#' @rdname start_run
#' @export
list_runs <- function(store, request_id) {
  DBI::dbGetQuery(store, "SELECT * FROM runs WHERE request_id = ? ORDER BY id DESC",
                  params = list(request_id))
}

#' @rdname start_run
#' @export
get_run_datasets <- function(store, run_id) {
  DBI::dbGetQuery(store, "SELECT * FROM run_datasets WHERE run_id = ? ORDER BY position",
                  params = list(run_id))
}

# ---------------------------------------------------------------------------
# Approval
# ---------------------------------------------------------------------------

#' Approve a preview run's datasets
#'
#' Approval is tied to the exact SQL that was previewed. A dataset is approved
#' only if the preview succeeded and the SQL generated from its current spec
#' and the current configuration is identical to the SQL that was run.
#'
#' @return Invisibly, the ids of the datasets approved.
#' @export
approve_run <- function(store, run_id, cfg) {
  run <- get_run(store, run_id)
  if (run$kind != "preview") stop("Only preview runs can be approved.", call. = FALSE)
  if (run$status != "succeeded") stop("Run ", run_id, " did not succeed; it cannot be approved.", call. = FALSE)

  rds <- get_run_datasets(store, run_id)
  current <- get_datasets(store, run$request_id)
  if (!setequal(rds$dataset_id, current$id) || anyNA(rds$dataset_id)) {
    stop("The request's datasets have changed since run ", run_id, ". Preview again.", call. = FALSE)
  }
  for (i in seq_len(nrow(rds))) {
    d <- current[current$id == rds$dataset_id[i], ]
    now_hash <- sql_hash(build_sql(d$spec[[1]], cfg))
    if (!identical(now_hash, rds$sql_hash[i])) {
      stop("Dataset '", d$name, "' has changed since run ", run_id, ". Preview again.", call. = FALSE)
    }
  }
  t <- now_utc()
  DBI::dbWithTransaction(store, {
    for (i in seq_len(nrow(rds))) {
      DBI::dbExecute(store, "
        UPDATE datasets SET approved_sql_hash = ?, approved_run_id = ?, approved_at = ?
        WHERE id = ?", params = list(rds$sql_hash[i], run_id, t, rds$dataset_id[i]))
    }
  })
  invisible(rds$dataset_id)
}

#' Approval status of a request's datasets
#'
#' @return A data frame with `id`, `name` and `status`: `"approved"`,
#'   `"unapproved"` (never approved, or edited since), or `"stale"` (approved,
#'   but the SQL it would now generate differs, e.g. after a configuration
#'   change).
#' @export
approval_status <- function(store, request_id, cfg) {
  d <- get_datasets(store, request_id)
  status <- vapply(seq_len(nrow(d)), function(i) {
    if (is.na(d$approved_sql_hash[i])) return("unapproved")
    now_hash <- tryCatch(sql_hash(build_sql(d$spec[[i]], cfg)), error = function(e) NA)
    if (identical(now_hash, d$approved_sql_hash[i])) "approved" else "stale"
  }, "")
  data.frame(id = d$id, name = d$name, status = status)
}

#' Can this request be packaged?
#'
#' True only if it has at least one dataset and every dataset is approved
#' against the current SQL.
#' @export
ready_to_package <- function(store, request_id, cfg) {
  s <- approval_status(store, request_id, cfg)
  nrow(s) > 0 && all(s$status == "approved")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' @noRd
now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

#' @noRd
app_version <- function() as.character(utils::packageVersion("ciarequests"))

#' @noRd
last_id <- function(store) DBI::dbGetQuery(store, "SELECT last_insert_rowid()")[[1]]

#' @noRd
touch_request <- function(store, request_id) {
  DBI::dbExecute(store, "UPDATE requests SET updated_at = ? WHERE id = ?",
                 params = list(now_utc(), request_id))
}

#' @noRd
renumber_datasets <- function(store, request_id) {
  ids <- DBI::dbGetQuery(store, "SELECT id FROM datasets WHERE request_id = ? ORDER BY position",
                         params = list(request_id))$id
  for (i in seq_along(ids)) {
    DBI::dbExecute(store, "UPDATE datasets SET position = ? WHERE id = ?", params = list(i, ids[i]))
  }
}

#' Turn a UNIQUE(request_id, name) violation into a readable error
#' @noRd
with_unique_name <- function(name, expr) {
  tryCatch(expr, error = function(e) {
    if (grepl("UNIQUE constraint failed: datasets.request_id, datasets.name", conditionMessage(e))) {
      stop("This request already has a dataset named '", name, "'.", call. = FALSE)
    }
    stop(e)
  })
}

#' @noRd
check_text <- function(x, what, required = FALSE) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("'", what, "' must be a single string.", call. = FALSE)
  }
  if (required && !nzchar(trimws(x))) stop("'", what, "' is required.", call. = FALSE)
  invisible(x)
}
