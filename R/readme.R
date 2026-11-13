#' Write the recipient-facing README for a packaged request
#'
#' Describes each dataset (filters, grouping, grid, record count) and how much
#' was withheld under the confidentiality policy. Suppressed catch totals are
#' only reported when the suppressed records, taken together, satisfy the
#' confidentiality rules; otherwise reporting them would reveal the catch of
#' the suppressed groups.
#'
#' @param request A request, from [get_request()].
#' @param entries List of lists with `spec`, `stats`, `sql_file`.
#' @param gpkg_file File name of the GeoPackage.
#' @return The README text (Markdown).
#' @noRd
render_readme <- function(request, entries, gpkg_file, cfg, prepared = Sys.time()) {
  md_text <- function(x) gsub("[[:cntrl:]]+", " ", x)
  rules <- vapply(cfg$rules, function(r) r$description %||% r$name, "")

  dataset_section <- function(i) {
    e <- entries[[i]]
    s <- e$stats
    spec <- e$spec
    grid <- cfg$grids[[spec$grid]]
    filters <- if (length(spec$filters)) {
      vapply(names(spec$filters), function(f) describe_filter(f, spec$filters[[f]]), "")
    } else "none (all records)"

    suppression <- c(
      sprintf("- Records withheld: %s of %s (%s)",
              fmt_n(s$rows_dropped), fmt_n(s$rows_total), fmt_pct(s$pct_rows_dropped)),
      if (s$rows_dropped == 0) {
        "- No records were withheld."
      } else if (isTRUE(s$dropped_disclosable)) {
        c(sprintf("- Catch weight withheld: %s t of %s t (%s)",
                  fmt_n(s$weight_dropped, 1), fmt_n(s$weight_total, 1), fmt_pct(s$pct_weight_dropped)),
          sprintf("- Catch count withheld: %s of %s (%s)",
                  fmt_n(s$count_dropped), fmt_n(s$count_total), fmt_pct(s$pct_count_dropped)))
      } else {
        paste("- Catch withheld: not reported. The withheld records, taken together,",
              "represent too few vessels to report their totals.")
      }
    )

    c(
      paste0("### ", i, ". `", spec$name, "`"),
      "",
      if (nzchar(spec$description)) c(md_text(spec$description), ""),
      paste0("- GeoPackage layer: `", spec$name, "`"),
      paste0("- Query: `", e$sql_file, "`"),
      paste0("- Grid: ", md_text(grid$label %||% spec$grid)),
      paste0("- Grouped by: grid cell", if (length(spec$group_by)) paste0(", ", paste0("`", spec$group_by, "`", collapse = ", "))),
      paste0("- Filters: ", if (length(filters) == 1) md_text(filters) else ""),
      if (length(filters) > 1) paste0("  - ", md_text(filters)),
      paste0("- Records delivered: ", fmt_n(s$rows_kept)),
      "",
      "Confidentiality:",
      "",
      suppression,
      ""
    )
  }

  sql_files <- vapply(entries, `[[`, "", "sql_file")
  layers <- vapply(entries, function(e) e$spec$name, "")
  c(
    paste0("# ", md_text(request$title)),
    "",
    paste0("- Prepared for: ", md_text(request$requester)),
    paste0("- Request: #", request$id),
    paste0("- Prepared: ", format(prepared, "%Y-%m-%d", tz = "UTC")),
    "",
    "## Contents",
    "",
    paste0("- `", gpkg_file, "`: GeoPackage with one layer per dataset below."),
    paste0("- `", sql_files, "`: the query that produced layer `", layers, "`."),
    "- `README.md`: this file.",
    "",
    "## Datasets",
    "",
    unlist(lapply(seq_along(entries), dataset_section)),
    "## Columns",
    "",
    "- `grid_id`: grid cell identifier. Cell polygons are in the layer geometry.",
    "- Grouping columns as listed for each dataset.",
    "- `catch_weight_mt`: catch weight, metric tons.",
    "- `catch_count`: catch count (number of fish). Apportioned counts can be fractional.",
    "",
    "## Method",
    "",
    sub("^Method: e", "E", paste(method_lines(cfg, audience = "recipient"), collapse = " ")),
    "",
    "Coordinate reference system: EPSG:3338 (NAD83 / Alaska Albers).",
    "",
    "## Confidentiality",
    "",
    "Records are withheld unless they meet every rule below:",
    "",
    paste0("- ", md_text(rules)),
    "",
    "Withheld records are removed, not set to zero; their cells do not appear in the layer.",
    ""
  ) |> paste(collapse = "\n")
}

#' @noRd
describe_filter <- function(field, value) {
  if (is.list(value)) {
    if (!is.null(value$min) && !is.null(value$max)) {
      if (value$min == value$max) return(paste0("`", field, "` = ", value$min))
      return(paste0("`", field, "` from ", value$min, " to ", value$max))
    }
    if (!is.null(value$min)) return(paste0("`", field, "` from ", value$min))
    return(paste0("`", field, "` up to ", value$max))
  }
  paste0("`", field, "` is ", paste(value, collapse = ", "))
}

#' @noRd
fmt_pct <- function(x) {
  if (x == 0) return("0%")
  if (x < 0.1) return("<0.1%")
  sprintf("%.1f%%", x)
}
