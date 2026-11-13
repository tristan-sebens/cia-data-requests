#' Define a dataset within a data request
#'
#' @param name Short name, used as the GeoPackage layer name and file name.
#'   Lowercase letters, digits and underscores, starting with a letter.
#' @param grid Name of a grid in the grid registry (e.g. `"hex_8km"`).
#' @param filters Named list. For `set` fields, a character vector of allowed
#'   values. For `range` fields, `list(min = , max = )` (either may be `NULL`
#'   for an open bound). Fields not listed are unrestricted.
#' @param group_by Character vector of field names to group by, in output
#'   column order. The grid cell is always included and need not be listed.
#' @param description Optional free-text description.
#' @return A `cia_dataset_spec`.
#' @export
dataset_spec <- function(name, grid, filters = list(), group_by = character(),
                         description = "") {
  spec <- list(
    name        = name,
    description = description,
    grid        = grid,
    filters     = filters,
    group_by    = as.character(group_by)
  )
  class(spec) <- "cia_dataset_spec"
  spec
}

#' Check a dataset spec against the configuration
#'
#' Checks structure only (fields exist, values have the right shape). Whether
#' filter values actually occur in the database is checked separately.
#'
#' @return The spec, normalised (set values de-duplicated and sorted, range
#'   bounds as integers, empty filters removed), invisibly. Errors if invalid.
#' @export
validate_spec <- function(spec, cfg) {
  err <- function(...) stop("Invalid dataset '", spec$name, "': ", ..., call. = FALSE)

  if (!is.character(spec$name) || length(spec$name) != 1 ||
      !grepl("^[a-z][a-z0-9_]{0,59}$", spec$name)) {
    stop("Invalid dataset name '", paste(spec$name, collapse = " "), "': use lowercase ",
         "letters, digits and underscores, starting with a letter (max 60 characters).",
         call. = FALSE)
  }
  if (!is.character(spec$description) || length(spec$description) != 1) {
    err("description must be a single string")
  }
  if (!is.character(spec$grid) || length(spec$grid) != 1 || !spec$grid %in% names(cfg$grids)) {
    err("unknown grid '", paste(spec$grid, collapse = " "), "'. Available: ",
        paste(names(cfg$grids), collapse = ", "))
  }

  # Filters
  if (!is.list(spec$filters) || (length(spec$filters) && is.null(names(spec$filters)))) {
    err("filters must be a named list")
  }
  unknown <- setdiff(names(spec$filters), names(cfg$fields))
  if (length(unknown)) err("unknown filter field(s): ", paste(unknown, collapse = ", "))
  if (anyDuplicated(names(spec$filters))) err("a field is filtered more than once")

  norm <- list()
  for (f in names(spec$filters)) {
    v <- spec$filters[[f]]
    if (cfg$fields[[f]]$kind == "set") {
      if (is.null(v) || !length(v)) next
      if (!is.character(v) || anyNA(v) || any(!nzchar(v))) {
        err("filter '", f, "' needs non-empty text values")
      }
      norm[[f]] <- sort(unique(v))
    } else {
      if (is.null(v)) next
      if (!is.list(v) || length(setdiff(names(v), c("min", "max"))) || is.null(names(v))) {
        err("filter '", f, "' must be list(min = , max = )")
      }
      b <- lapply(v[c("min", "max")], function(x) {
        if (is.null(x) || (length(x) == 1 && is.na(x))) return(NULL)
        if (!is.numeric(x) || length(x) != 1 || x != round(x)) {
          err("filter '", f, "' bounds must be whole numbers")
        }
        as.integer(x)
      })
      names(b) <- c("min", "max")
      b <- Filter(Negate(is.null), b)
      if (!length(b)) next
      if (!is.null(b$min) && !is.null(b$max) && b$min > b$max) {
        err("filter '", f, "' has min > max")
      }
      norm[[f]] <- b
    }
  }
  spec$filters <- if (length(norm)) norm[order(names(norm))] else structure(list(), names = character())

  # Grouping
  unknown <- setdiff(spec$group_by, names(cfg$fields))
  if (length(unknown)) err("unknown grouping field(s): ", paste(unknown, collapse = ", "))
  if (anyDuplicated(spec$group_by)) err("a grouping field is listed more than once")

  invisible(spec)
}

#' Convert a dataset spec to and from JSON
#' @export
spec_to_json <- function(spec) {
  x <- unclass(spec)
  x$filters <- if (length(x$filters)) x$filters else structure(list(), names = character())
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = FALSE, digits = NA)
}

#' @rdname spec_to_json
#' @export
spec_from_json <- function(json) {
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  filters <- lapply(x$filters, function(v) {
    if (is.list(v) && !is.null(names(v))) v else as.character(unlist(v))
  })
  dataset_spec(
    name        = x$name,
    grid        = x$grid,
    filters     = filters,
    group_by    = as.character(unlist(x$group_by)),
    description = x$description %||% ""
  )
}

#' Stable hash of a dataset spec
#'
#' Equivalent specs (same filters in a different order, values in a different
#' order) hash identically. The grouping order is significant because it sets
#' the output column order.
#' @export
spec_hash <- function(spec, cfg) {
  spec <- validate_spec(spec, cfg)
  digest::digest(as.character(spec_to_json(spec)), algo = "sha256", serialize = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
