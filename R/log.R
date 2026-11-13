# Console logging. Everything goes to stderr, so it shows up in
# `docker compose logs -f app`. The level is set with CIA_LOG_LEVEL
# (DEBUG, INFO, WARN, ERROR, or OFF; default INFO).
#
# Log lines never contain catch data or credentials: only names, ids,
# counts and timings.

log_levels <- c(DEBUG = 1L, INFO = 2L, WARN = 3L, ERROR = 4L, OFF = 5L)

#' @noRd
log_threshold <- function() {
  lvl <- toupper(Sys.getenv("CIA_LOG_LEVEL", "INFO"))
  log_levels[[if (lvl %in% names(log_levels)) lvl else "INFO"]]
}

#' Write a log line
#'
#' @param level One of `"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"`.
#' @param ... Pasted together (no separator) to form the message.
#' @noRd
cia_log <- function(level, ...) {
  if (log_levels[[level]] < log_threshold()) return(invisible())
  ctx <- getOption("cia.log_context", "")
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), " ", formatC(level, width = -5), " ",
                 if (nzchar(ctx)) paste0("[", ctx, "] "), paste0(..., collapse = ""))
  cat(line, "\n", sep = "", file = stderr())
  invisible()
}
log_debug <- function(...) cia_log("DEBUG", ...)
log_info  <- function(...) cia_log("INFO", ...)
log_warn  <- function(...) cia_log("WARN", ...)
log_error <- function(...) cia_log("ERROR", ...)

#' Elapsed milliseconds since `t0`, formatted
#' @noRd
fmt_ms <- function(t0) {
  ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  if (ms < 1000) sprintf("%d ms", round(ms)) else sprintf("%.1f s", ms / 1000)
}

#' Evaluate `expr`, logging how long it took
#'
#' Logged at `level` when it finishes; promoted to WARN if it took longer than
#' `slow_ms`. Errors are logged and re-thrown.
#' @noRd
log_timed <- function(label, expr, level = "DEBUG", slow_ms = 1000) {
  t0 <- Sys.time()
  log_debug(label, " ...")
  out <- withCallingHandlers(expr, error = function(e) {
    log_error(label, " failed after ", fmt_ms(t0), ": ", trimws(conditionMessage(e)))
  })
  ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  cia_log(if (ms > slow_ms) "WARN" else level, label, " took ", fmt_ms(t0),
          if (ms > slow_ms) " (slow)")
  out
}

#' Run `expr` with a log context tag (e.g. a session or job id) on each line
#' @noRd
with_log_context <- function(ctx, expr) {
  old <- options(cia.log_context = ctx)
  on.exit(options(old))
  expr
}

#' Short form of a call stack for error logs
#' @noRd
short_trace <- function(calls = sys.calls(), n = 8) {
  txt <- vapply(calls, function(cl) paste(deparse(cl, nlines = 1, width.cutoff = 80), collapse = ""), "")
  txt <- txt[!grepl("^(withCallingHandlers|tryCatch|doTryCatch|tryCatchList|tryCatchOne|withVisible|eval|force|shiny::|\\.handleSimpleError|h\\(|stop\\()", txt)]
  utils::tail(txt, n)
}
