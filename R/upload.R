#' Upload packaged requests with rclone
#'
#' Destinations are `<remote_root>/<folder>`, where `remote_root` is fixed
#' (`CIA_RCLONE_REMOTE`, e.g. `gdrive:Data Requests`) and `folder` is the
#' request's Drive folder name. The folder must already exist and be shared
#' with the recipient; this only copies files into it.
#'
#' @param remote_root rclone remote and base path.
#' @param rclone Path to the rclone binary.
#' @param args Extra arguments passed to every rclone call.
#' @return A `cia_uploader`: a list of functions `destination(folder)`,
#'   `copy(local_dir, dest)` and `delete(dest, file)`, each of the latter two
#'   returning `list(ok, log)`.
#' @export
rclone_uploader <- function(remote_root = Sys.getenv("CIA_RCLONE_REMOTE"), rclone = "rclone",
                            args = character()) {
  if (!nzchar(remote_root)) {
    stop("No upload destination: set CIA_RCLONE_REMOTE (e.g. 'gdrive:Data Requests').", call. = FALSE)
  }
  run <- function(cmd_args) {
    res <- tryCatch(
      processx::run(rclone, c(cmd_args, args), error_on_status = FALSE, timeout = 3600),
      error = function(e) list(status = -1, stdout = "", stderr = conditionMessage(e))
    )
    list(ok = identical(as.integer(res$status), 0L),
         log = trimws(paste(res$stdout, res$stderr, sep = "\n")))
  }
  structure(list(
    name = "rclone",
    destination = function(folder) join_remote(remote_root, check_drive_folder(folder)),
    # copy (not sync): never deletes anything else in the recipient's folder.
    copy = function(local_dir, dest) run(c("copy", local_dir, dest, "--stats-one-line", "-v")),
    delete = function(dest, file) run(c("deletefile", paste0(dest, "/", file)))
  ), class = "cia_uploader")
}

#' Check a request's Drive folder name
#'
#' A folder below the fixed remote root: no remote prefix, no absolute path,
#' no `..` segments, so a request can never write outside the root.
#' @noRd
check_drive_folder <- function(folder) {
  if (!is.character(folder) || length(folder) != 1 || is.na(folder) || !nzchar(trimws(folder))) {
    stop("The request has no Drive folder set.", call. = FALSE)
  }
  f <- trimws(folder)
  parts <- strsplit(f, "/", fixed = TRUE)[[1]]
  if (grepl(":", f, fixed = TRUE) || startsWith(f, "/") || grepl("\\\\", f) ||
      any(parts %in% c("", ".", "..")) || grepl("[[:cntrl:]]", f)) {
    stop("Invalid Drive folder '", folder, "': give a folder name below the upload root ",
         "(no ':' or '\\', no leading '/', no '.' or '..' parts).", call. = FALSE)
  }
  f
}

#' @noRd
join_remote <- function(root, folder) {
  if (endsWith(root, ":") || endsWith(root, "/")) paste0(root, folder) else paste0(root, "/", folder)
}
