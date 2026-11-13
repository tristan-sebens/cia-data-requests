# Background jobs for the app. Previews and packaging can take minutes on the
# real database, so they run in a separate R process; the app polls the
# process and a progress file.

#' Start a background job
#'
#' @param kind `"preview"`, `"package"` or `"upload"`.
#' @param id Request id (preview, package) or run id (upload).
#' @return A list with the `callr` process, `kind`, `id` and `log_file`.
#' @noRd
start_job <- function(kind, id) {
  log_file <- tempfile(paste0("cia_", kind, "_"), fileext = ".log")
  file.create(log_file)
  proc <- callr::r_bg(
    job_main,
    args = list(kind = kind, id = id, log_file = log_file,
                dev_pkg = Sys.getenv("CIA_DEV_PKG")),
    supervise = TRUE,
    package = FALSE,
    # Inherit the app's stdout/stderr so job logging shows in the console.
    # (Unread pipes would also block the job once their buffer filled.)
    stdout = "", stderr = "",
    env = c(callr::rcmd_safe_env(), CIA_LOG_CONTEXT = sprintf("job %s %s", kind, id))
  )
  log_info("Started background ", kind, " job for ", if (kind == "upload") "run " else "request ", id,
           " (pid ", proc$get_pid(), ")")
  list(proc = proc, kind = kind, id = id, log_file = log_file, started = Sys.time(), cancelled_at = NULL)
}

# Files a job shares with the app, next to its log.
job_cancel_file <- function(log_file) paste0(log_file, ".cancel")
job_pgpid_file  <- function(log_file) paste0(log_file, ".pgpid")

#' Ask a running job to stop
#'
#' Leaves a flag the job checks between datasets, and cancels the job's
#' current Postgres query (`pg_cancel_backend`), which otherwise keeps running
#' on the server even if the job's R process is killed. The job then fails
#' cleanly, recording its run as failed.
#'
#' @param con A connection to the CIA database (same user as the job).
#' @return The job, with `cancelled_at` set.
#' @noRd
cancel_job <- function(job, con) {
  file.create(job_cancel_file(job$log_file))
  pid_file <- job_pgpid_file(job$log_file)
  if (file.exists(pid_file)) {
    pid <- as.integer(readLines(pid_file, warn = FALSE)[1])
    ok <- tryCatch(DBI::dbGetQuery(con, "SELECT pg_cancel_backend($1) AS ok", params = list(pid))$ok,
                   error = function(e) {
                     log_warn("Could not cancel Postgres backend ", pid, ": ", conditionMessage(e))
                     FALSE
                   })
    log_info("Cancel ", job$kind, " job for ", job$id, ": Postgres backend ", pid,
             if (isTRUE(ok)) " cancelled" else " not cancelled (no query running?)")
  } else {
    log_info("Cancel ", job$kind, " job for ", job$id, ": not connected to Postgres yet")
  }
  job$cancelled_at <- Sys.time()
  job
}

#' Body of a background job (runs in the child process)
#' @noRd
job_main <- function(kind, id, log_file, dev_pkg) {
  # In development the package is loaded from source; otherwise the installed copy.
  if (nzchar(dev_pkg)) pkgload::load_all(dev_pkg, quiet = TRUE, export_all = FALSE)
  else library(ciarequests)
  ns <- asNamespace("ciarequests")
  options(cia.log_context = Sys.getenv("CIA_LOG_CONTEXT", paste("job", kind, id)))
  say <- function(...) {
    cat(format(Sys.time(), "%H:%M:%S"), " ", paste0(...), "\n", sep = "", file = log_file, append = TRUE)
    ns$log_info(...)
  }
  t0 <- Sys.time()
  on.exit(ns$log_info("Job finished after ", ns$fmt_ms(t0)), add = TRUE)
  withCallingHandlers(ns$job_body(ns, kind, id, say, log_file), error = function(e) {
    say("ERROR: ", conditionMessage(e))
  })
}

#' @noRd
job_body <- function(ns, kind, id, say, log_file) {
  cancel_file <- ns$job_cancel_file(log_file)
  check_cancel <- function() {
    if (file.exists(cancel_file)) stop("Cancelled by user.", call. = FALSE)
  }

  cfg <- ns$load_config()
  store <- ns$store_connect()
  on.exit(DBI::dbDisconnect(store), add = TRUE)

  if (kind == "upload") {
    say("Uploading run ", id, " ...")
    status <- ns$upload_run(store, id, ns$rclone_uploader())
    say("Upload ", status, ".")
    return(list(status = status, run_id = id))
  }

  say("Connecting to the CIA database ...")
  con <- ns$cia_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # So the app can cancel this job's running query.
  writeLines(as.character(DBI::dbGetQuery(con, "SELECT pg_backend_pid()")[[1]]),
             ns$job_pgpid_file(log_file))
  check_cancel()

  if (kind == "preview") {
    res <- ns$preview_request(store, con, id, cfg, progress = function(i, n, name) {
      check_cancel()
      say("Running dataset ", i, " of ", n, ": ", name)
    })
    if (file.exists(cancel_file)) say("Cancelled by user.")
    say("Preview ", res$status, ".")
    return(res)
  }

  if (kind == "package") {
    labels <- c(query = "Running", write = "Writing layer", upload = "Uploading to")
    res <- ns$package_request(store, con, id, cfg, uploader = ns$rclone_uploader(),
                              progress = function(stage, detail) {
                                # Not once uploading: the package itself is complete by then.
                                if (stage != "upload") check_cancel()
                                say(labels[[stage]], " ", detail)
                              })
    say("Package ", res$status, if (!is.null(res$upload_status)) paste0(", upload ", res$upload_status), ".")
    res$results <- NULL  # not needed by the app; keeps the hand-back small
    return(res)
  }
  stop("Unknown job kind: ", kind)
}

#' Poll a job: its log so far, and its result once finished
#' @return A list with `done`, `log`, and when done either `result` or `error`.
#' @noRd
poll_job <- function(job) {
  log <- if (file.exists(job$log_file)) readLines(job$log_file, warn = FALSE) else character()
  if (job$proc$is_alive()) return(list(done = FALSE, log = log))
  out <- tryCatch(list(result = job$proc$get_result()), error = function(e) {
    msg <- conditionMessage(e)
    parent <- e$parent
    if (!is.null(parent)) msg <- conditionMessage(parent)
    list(error = msg)
  })
  c(list(done = TRUE, log = log), out)
}
