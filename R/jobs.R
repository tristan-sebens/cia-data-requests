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
  list(proc = proc, kind = kind, id = id, log_file = log_file, started = Sys.time())
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
  withCallingHandlers(ns$job_body(ns, kind, id, say), error = function(e) {
    say("ERROR: ", conditionMessage(e))
  })
}

#' @noRd
job_body <- function(ns, kind, id, say) {
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

  if (kind == "preview") {
    res <- ns$preview_request(store, con, id, cfg, progress = function(i, n, name) {
      say("Running dataset ", i, " of ", n, ": ", name)
    })
    say("Preview ", res$status, ".")
    return(res)
  }

  if (kind == "package") {
    labels <- c(query = "Running", write = "Writing layer", upload = "Uploading to")
    res <- ns$package_request(store, con, id, cfg, uploader = ns$rclone_uploader(),
                              progress = function(stage, detail) say(labels[[stage]], " ", detail))
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
