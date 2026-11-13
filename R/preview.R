#' Preview a request: run every dataset and record the results
#'
#' Runs each dataset's deliver and diagnostic queries, recording the SQL and
#' suppression statistics as a `preview` run. A dataset fails if its filters
#' use values that do not exist in the database (almost always a typo) or if
#' its query errors; the other datasets still run. The run succeeds only if
#' every dataset succeeds, and only a successful run can be approved with
#' [approve_run()].
#'
#' @param store Connection from [store_connect()].
#' @param con Connection to the CIA database from [cia_connect()].
#' @param progress Optional callback `function(i, n, name)` called before each dataset.
#' @return A list with `run_id`, `status`, and `results`: one entry per
#'   dataset, either a `cia_dataset_result` or a `cia_dataset_error`.
#' @export
preview_request <- function(store, con, request_id, cfg, progress = NULL) {
  datasets <- get_datasets(store, request_id)
  if (!nrow(datasets)) stop("Request ", request_id, " has no datasets to preview.", call. = FALSE)

  run_id <- start_run(store, request_id, "preview")
  results <- vector("list", nrow(datasets))
  names(results) <- datasets$name

  outcome <- tryCatch({
    for (i in seq_len(nrow(datasets))) {
      spec <- datasets$spec[[i]]
      if (is.function(progress)) progress(i, nrow(datasets), spec$name)
      results[[i]] <- run_dataset_checked(con, spec, cfg)
      r <- results[[i]]
      if (inherits(r, "cia_dataset_error")) {
        record_run_dataset(store, run_id, datasets$id[i], i, spec = r$spec, sql = r$sql, error = r$error)
      } else {
        record_run_dataset(store, run_id, datasets$id[i], i, result = r)
      }
    }
    failed <- vapply(results, inherits, TRUE, "cia_dataset_error")
    if (any(failed)) {
      finish_run(store, run_id, "failed",
                 error = paste0("Failed dataset(s): ", paste(names(results)[failed], collapse = ", ")))
      "failed"
    } else {
      finish_run(store, run_id, "succeeded")
      "succeeded"
    }
  }, error = function(e) {
    finish_run(store, run_id, "failed", error = conditionMessage(e))
    stop(e)
  })

  list(run_id = run_id, status = outcome, results = results)
}

#' Run one dataset, returning an error object instead of throwing
#' @noRd
run_dataset_checked <- function(con, spec, cfg) {
  sql <- tryCatch(build_sql(spec, cfg), error = function(e) NA_character_)
  fail <- function(msg) {
    structure(list(spec = spec, sql = if (is.na(sql)) "" else sql, error = msg),
              class = "cia_dataset_error")
  }
  if (is.na(sql)) return(fail(tryCatch(build_sql(spec, cfg), error = conditionMessage)))

  unknown <- tryCatch(unknown_filter_values(con, spec, cfg), error = function(e) e)
  if (inherits(unknown, "error")) return(fail(conditionMessage(unknown)))
  if (nrow(unknown)) {
    by_field <- tapply(unknown$value, unknown$field, paste, collapse = ", ")
    return(fail(paste0("Filter values not found in the database: ",
                       paste0(names(by_field), " = ", by_field, collapse = "; "))))
  }
  res <- tryCatch(run_dataset(con, spec, cfg), error = function(e) fail(conditionMessage(e)))
  if (inherits(res, "cia_dataset_result")) {
    # For the preview map only; a failure here should not fail the dataset.
    res$areas <- tryCatch(area_outlines(con, res$spec, cfg), error = function(e) {
      log_warn("Could not load spatial filter outlines for '", spec$name, "': ", conditionMessage(e))
      NULL
    })
  }
  res
}

#' @export
print.cia_dataset_error <- function(x, ...) {
  cat("<cia_dataset_error> ", x$spec$name, ": ", x$error, "\n", sep = "")
  invisible(x)
}
