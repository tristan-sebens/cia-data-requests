#' Package a request: run, write the GeoPackage, SQL and README, and upload
#'
#' Requires every dataset to be approved against its current SQL
#' ([ready_to_package()]). Each dataset is run again from its approved SQL; the
#' results are written to a staging folder that is renamed into place only
#' when every file has been written, so an interruption never leaves a
#' half-written package. The run folder is then uploaded (see [upload_run()]).
#'
#' Local layout: `<output_root>/request_<id>/run_<run id>/` containing
#' `<request>.gpkg` (one layer per dataset), `NN_<dataset>.sql` and `README.md`.
#'
#' @param store Connection from [store_connect()].
#' @param con Connection to the CIA database from [cia_connect()].
#' @param uploader A [rclone_uploader()], or `NULL` to package without uploading.
#' @param output_root Defaults to the `CIA_OUTPUT_DIR` environment variable.
#' @param progress Optional callback `function(stage, detail)`.
#' @return A list with `run_id`, `status`, `upload_status`, `output_dir`, `results`.
#' @export
package_request <- function(store, con, request_id, cfg, uploader = rclone_uploader(),
                            output_root = Sys.getenv("CIA_OUTPUT_DIR"), progress = NULL) {
  say <- function(stage, detail = "") if (is.function(progress)) progress(stage, detail)
  if (!nzchar(output_root)) stop("No output folder: set CIA_OUTPUT_DIR.", call. = FALSE)

  request <- get_request(store, request_id)
  if (!ready_to_package(store, request_id, cfg)) {
    stop("Request ", request_id, " is not ready to package: every dataset must be previewed ",
         "and approved with its current definition.", call. = FALSE)
  }
  if (!is.null(uploader)) uploader$destination(request$drive_path)  # fail early on a bad folder

  datasets <- get_datasets(store, request_id)
  run_id <- start_run(store, request_id, "package")
  request_dir <- file.path(output_root, paste0("request_", request_id))
  final_dir <- file.path(request_dir, paste0("run_", run_id))
  staging_dir <- paste0(final_dir, ".partial")
  results <- list()

  built <- tryCatch({
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    gpkg_file <- paste0(file_slug(request$title, fallback = paste0("request_", request_id)), ".gpkg")
    entries <- list()

    for (i in seq_len(nrow(datasets))) {
      spec <- datasets$spec[[i]]
      say("query", sprintf("%d/%d %s", i, nrow(datasets), spec$name))
      r <- run_dataset(con, spec, cfg)
      if (!identical(r$sql_hash, datasets$approved_sql_hash[i])) {
        stop("Dataset '", spec$name, "' no longer matches its approved SQL.", call. = FALSE)
      }
      record_run_dataset(store, run_id, datasets$id[i], i, result = r)
      results[[spec$name]] <- r

      say("write", spec$name)
      log_timed(paste0("Write layer '", spec$name, "' (", nrow(r$data), " records)"), level = "INFO",
        sf::st_write(r$data, file.path(staging_dir, gpkg_file), layer = spec$name,
                   driver = "GPKG", quiet = TRUE))
      sql_file <- sprintf("%02d_%s.sql", i, spec$name)
      cat(r$sql, file = file.path(staging_dir, sql_file), sep = "")
      entries[[i]] <- list(spec = r$spec, stats = r$stats, sql_file = sql_file)
    }

    writeLines(render_readme(request, entries, gpkg_file, cfg), file.path(staging_dir, "README.md"),
               useBytes = TRUE)
    if (dir.exists(final_dir)) unlink(final_dir, recursive = TRUE)
    if (!file.rename(staging_dir, final_dir)) stop("Could not move the package into ", final_dir, call. = FALSE)
    finish_run(store, run_id, "succeeded", output_dir = final_dir,
               gpkg_path = file.path(final_dir, gpkg_file),
               upload_status = if (is.null(uploader)) "skipped" else "pending")
    TRUE
  }, error = function(e) {
    unlink(staging_dir, recursive = TRUE)
    finish_run(store, run_id, "failed", error = conditionMessage(e), upload_status = "skipped")
    e
  })
  if (inherits(built, "error")) {
    return(list(run_id = run_id, status = "failed", upload_status = "skipped",
                output_dir = NA_character_, results = results, error = conditionMessage(built)))
  }

  upload_status <- "skipped"
  if (!is.null(uploader)) {
    say("upload", request$drive_path)
    upload_status <- upload_run(store, run_id, uploader)
  }
  list(run_id = run_id, status = "succeeded", upload_status = upload_status,
       output_dir = final_dir, results = results)
}

#' Upload (or re-upload) a packaged run
#'
#' Copies the run folder into the request's Drive folder, replacing files of
#' the same name. Files this app uploaded for the request's previous delivery
#' to the same folder that are not part of this run (e.g. the SQL of a dataset
#' since deleted) are then removed. Nothing else in the folder is touched.
#'
#' @return The upload status, `"succeeded"` or `"failed"`; details are recorded
#'   on the run (`upload_error`, `upload_log`).
#' @export
upload_run <- function(store, run_id, uploader) {
  run <- get_run(store, run_id)
  if (run$kind != "package" || run$status != "succeeded" || is.na(run$output_dir) ||
      !dir.exists(run$output_dir)) {
    stop("Run ", run_id, " is not a completed package with its files on disk.", call. = FALSE)
  }
  request <- get_request(store, run$request_id)
  dest <- uploader$destination(request$drive_path)
  update_run(store, run_id, upload_status = "running", upload_dest = dest,
             upload_error = NA_character_)

  log <- character()
  fail <- function(msg) {
    update_run(store, run_id, upload_status = "failed", upload_error = msg,
               upload_log = paste(log, collapse = "\n"))
    "failed"
  }

  res <- uploader$copy(run$output_dir, dest)
  log <- c(log, paste("$ copy", run$output_dir, dest), res$log)
  if (!res$ok) return(fail(paste("Upload to", dest, "failed. See the upload log.")))

  # Remove files from this app's previous delivery to the same folder that this
  # run no longer contains.
  prev <- DBI::dbGetQuery(store, "
    SELECT output_dir FROM runs
    WHERE request_id = ? AND kind = 'package' AND upload_status = 'succeeded'
      AND upload_dest = ? AND id <> ?
    ORDER BY id DESC LIMIT 1", params = list(run$request_id, dest, run_id))
  if (nrow(prev) && dir.exists(prev$output_dir)) {
    stale <- setdiff(list.files(prev$output_dir), list.files(run$output_dir))
    for (f in stale) {
      res <- uploader$delete(dest, f)
      log <- c(log, paste("$ delete", f), res$log)
      if (!res$ok) return(fail(paste("Removing the outdated file", f, "from", dest, "failed.")))
    }
  }

  update_run(store, run_id, upload_status = "succeeded", uploaded_at = now_utc(),
             upload_log = paste(log, collapse = "\n"))
  "succeeded"
}

#' File-name-safe version of a title
#' @noRd
file_slug <- function(x, fallback = "request") {
  s <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  s <- substr(s, 1, 60)
  if (!nzchar(s) || is.na(s)) fallback else s
}
