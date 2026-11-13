#' Run one dataset: delivered data plus suppression statistics
#'
#' Executes the dataset's deliver query (returned as an `sf` object, exactly as
#' it would be exported) and its diagnostic query (kept/dropped totals).
#'
#' @param con A DBI connection to the CIA database.
#' @param spec A [dataset_spec()].
#' @param cfg A [load_config()] result.
#' @return A `cia_dataset_result` list:
#'   * `spec`     the normalised spec
#'   * `sql`      the deliver SQL (what is written to the `.sql` file)
#'   * `sql_hash` SHA-256 of `sql`, used to record approval
#'   * `data`     `sf` data frame: grid_id, grouping fields, catch_weight_mt,
#'                catch_count, grid_shape (EPSG:3338)
#'   * `stats`    one-row data frame of suppression statistics, see
#'                [suppression_stats()]
#'   * `duration_s` wall time for both queries
#' @export
run_dataset <- function(con, spec, cfg) {
  spec <- validate_spec(spec, cfg)
  sql <- build_sql(spec, cfg, "deliver")
  diag_sql <- build_sql(spec, cfg, "diagnostic")

  started <- Sys.time()
  data <- log_timed(paste0("Dataset '", spec$name, "': deliver query"), level = "INFO", slow_ms = Inf,
                    read_deliver(con, sql, spec))
  diag <- log_timed(paste0("Dataset '", spec$name, "': diagnostic query"), level = "INFO", slow_ms = Inf,
                    DBI::dbGetQuery(con, diag_sql))
  log_info("Dataset '", spec$name, "': ", nrow(data), " records delivered, ", diag$rows_dropped, " withheld")
  duration <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  if (nrow(data) != diag$rows_kept) {
    stop("Internal consistency check failed for dataset '", spec$name, "': ",
         nrow(data), " rows delivered but the diagnostic query counted ",
         diag$rows_kept, ". Do not deliver this dataset.", call. = FALSE)
  }

  structure(
    list(spec = spec, sql = sql, sql_hash = sql_hash(sql), data = data,
         stats = suppression_stats(diag), duration_s = duration),
    class = "cia_dataset_result"
  )
}

#' Hash of a deliver query, used to tie an approval to the exact SQL previewed
#' @export
sql_hash <- function(sql) {
  digest::digest(sql, algo = "sha256", serialize = FALSE)
}

#' Suppression statistics from a diagnostic query result
#'
#' @param diag The one-row result of the diagnostic query.
#' @return A one-row data frame with the raw totals plus:
#'   * `rows_total`, `weight_total`, `count_total`: kept + dropped
#'   * `pct_rows_dropped`, `pct_weight_dropped`, `pct_count_dropped`:
#'     share removed by the confidentiality rules (0-100)
#'   * `weight_off_grid`, `count_off_grid`: catch from hauls with no overlap
#'     with the grid (not delivered, not counted as suppression)
#'   * `dropped_disclosable`: whether the suppressed records, taken together,
#'     satisfy the confidentiality rules, so their totals may be reported to
#'     the recipient (TRUE when nothing was suppressed)
#' @export
suppression_stats <- function(diag) {
  d <- as.list(diag[1, , drop = FALSE])
  disclosable <- if (is.null(d$dropped_disclosable)) NA else as.logical(d$dropped_disclosable)
  d <- lapply(d, as.numeric)
  pct <- function(dropped, kept) {
    total <- dropped + kept
    if (total > 0) 100 * dropped / total else 0
  }
  data.frame(
    rows_kept          = as.integer(d$rows_kept),
    rows_dropped       = as.integer(d$rows_dropped),
    rows_total         = as.integer(d$rows_kept + d$rows_dropped),
    pct_rows_dropped   = pct(d$rows_dropped, d$rows_kept),
    weight_kept        = d$weight_kept,
    weight_dropped     = d$weight_dropped,
    weight_total       = d$weight_kept + d$weight_dropped,
    pct_weight_dropped = pct(d$weight_dropped, d$weight_kept),
    count_kept         = d$count_kept,
    count_dropped      = d$count_dropped,
    count_total        = d$count_kept + d$count_dropped,
    pct_count_dropped  = pct(d$count_dropped, d$count_kept),
    weight_source      = d$weight_source,
    count_source       = d$count_source,
    weight_off_grid    = max(0, d$weight_source - d$weight_kept - d$weight_dropped),
    count_off_grid     = max(0, d$count_source - d$count_kept - d$count_dropped),
    dropped_disclosable = d$rows_dropped == 0 || isTRUE(disclosable)
  )
}

#' @export
print.cia_dataset_result <- function(x, ...) {
  s <- x$stats
  cat("<cia_dataset_result> ", x$spec$name, " (", x$spec$grid, ")\n", sep = "")
  cat(sprintf("  rows:   %s delivered, %s suppressed (%.1f%%)\n",
              fmt_n(s$rows_kept), fmt_n(s$rows_dropped), s$pct_rows_dropped))
  cat(sprintf("  weight: %s t delivered, %s t suppressed (%.2f%%)\n",
              fmt_n(s$weight_kept, 1), fmt_n(s$weight_dropped, 1), s$pct_weight_dropped))
  cat(sprintf("  count:  %s delivered, %s suppressed (%.2f%%)\n",
              fmt_n(s$count_kept), fmt_n(s$count_dropped), s$pct_count_dropped))
  if (s$weight_off_grid > 0) {
    cat(sprintf("  off-grid (not delivered, not suppression): %s t\n", fmt_n(s$weight_off_grid, 1)))
  }
  cat(sprintf("  ran in %.1f s\n", x$duration_s))
  invisible(x)
}

#' Run the deliver query and return an sf object, also when it has no rows
#'
#' Runs the SQL exactly as delivered (no wrapping), and decodes the geometry,
#' which RPostgres returns as hex-encoded EWKB.
#' @noRd
read_deliver <- function(con, sql, spec) {
  res <- DBI::dbGetQuery(con, sql)
  wkb <- structure(as.character(res$grid_shape), class = "WKB")
  geom <- if (length(wkb)) sf::st_as_sfc(wkb, EWKB = TRUE) else sf::st_sfc()
  geom <- sf::st_set_crs(geom, 3338)
  res$grid_id <- as.integer(res$grid_id)
  res$grid_shape <- geom
  sf::st_as_sf(res, sf_column_name = "grid_shape")
}

#' Distinct values of a field, for filter pickers
#'
#' @return For `set` fields, a sorted character vector of non-missing values.
#'   For `range` fields, `c(min = , max = )`.
#' @export
field_values <- function(con, cfg, field) {
  def <- cfg$fields[[field]]
  if (is.null(def)) stop("Unknown field '", field, "'", call. = FALSE)
  tbl <- paste0(cfg$schema, ".", cfg[[def$table]]$table)
  if (def$kind == "range") {
    r <- log_timed(paste0("Load range of ", field, " from ", tbl), level = "INFO", slow_ms = 5000,
                   DBI::dbGetQuery(con, paste0("SELECT MIN(", field, ") AS min, MAX(", field, ") AS max FROM ", tbl)))
    return(c(min = as.integer(r$min), max = as.integer(r$max)))
  }
  v <- log_timed(paste0("Load filter values for ", field, " from ", tbl), level = "INFO", slow_ms = 5000,
    DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT ", field, " AS v FROM ", tbl, " WHERE ", field, " IS NOT NULL ORDER BY 1"))$v)
  log_debug(field, ": ", length(v), " distinct values")
  as.character(v)
}

#' Find filter values that do not occur in the database
#'
#' A typo in a code silently filters everything out, so values are checked
#' before a dataset is run.
#'
#' @return A data frame with columns `field`, `value` (zero rows if all known).
#' @export
unknown_filter_values <- function(con, spec, cfg) {
  spec <- validate_spec(spec, cfg)
  out <- data.frame(field = character(), value = character())
  for (f in names(spec$filters)) {
    if (cfg$fields[[f]]$kind != "set") next
    v <- spec$filters[[f]]
    tbl <- paste0(cfg$schema, ".", cfg[[cfg$fields[[f]]$table]]$table)
    found <- log_timed(paste0("Check filter values for ", f), DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT ", f, " AS v FROM ", tbl,
      " WHERE ", f, " IN (", paste(sql_quote(v), collapse = ", "), ")")))$v
    missing <- setdiff(v, found)
    if (length(missing)) out <- rbind(out, data.frame(field = f, value = missing))
  }
  V <- cfg$areas$view
  for (s in spec$spatial_filters) {
    found <- log_timed(paste0("Check area codes for ", s$source), DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT ", V$code, "::text AS v FROM ", V$schema, ".", V$table,
      " WHERE ", V$source, " = ", sql_quote(s$source),
      " AND ", V$code, "::text IN (", paste(sql_quote(s$codes), collapse = ", "), ")")))$v
    missing <- setdiff(s$codes, found)
    if (length(missing)) {
      out <- rbind(out, data.frame(field = paste0("area (", area_source_label(s$source, cfg), ")"),
                                   value = missing))
    }
  }
  out
}

#' Codes of every spatial filter area, by source
#'
#' @return A data frame with columns `source`, `code` (text), ordered by
#'   source then code.
#' @noRd
area_values <- function(con, cfg) {
  V <- cfg$areas$view
  x <- log_timed(paste0("Load spatial filter areas from ", V$schema, ".", V$table), level = "INFO",
                 slow_ms = 5000, DBI::dbGetQuery(con, paste0(
    "SELECT DISTINCT ", V$source, " AS source, ", V$code, "::text AS code FROM ", V$schema, ".", V$table)))
  x <- x[order(x$source, suppressWarnings(as.numeric(x$code)), x$code), , drop = FALSE]
  rownames(x) <- NULL
  log_info("Spatial filter areas: ", paste0(names(table(x$source)), " (", table(x$source), ")", collapse = ", "))
  x
}

#' Area sources in display order: those configured in `areas.yaml` first (in
#' that order), then any others found in the view, alphabetically
#' @noRd
order_area_sources <- function(sources, cfg) {
  known <- intersect(names(cfg$areas$sources), sources)
  c(known, sort(setdiff(sources, known)))
}

#' Outlines of a dataset's spatial filter areas, for the preview map
#'
#' @return An `sf` data frame (EPSG:4326, simplified for display) with columns
#'   `mode`, `source`, `code`; `NULL` if the dataset has no spatial filters.
#' @noRd
area_outlines <- function(con, spec, cfg) {
  if (!length(spec$spatial_filters)) return(NULL)
  V <- cfg$areas$view
  parts <- lapply(spec$spatial_filters, function(s) {
    sql <- paste0(
      "SELECT ", V$source, " AS source, ", V$code, "::text AS code, ",
      "ST_Transform(ST_SimplifyPreserveTopology(", V$geom, ", 250), 4326) AS geom ",
      "FROM ", V$schema, ".", V$table,
      " WHERE ", V$source, " = ", sql_quote(s$source),
      " AND ", V$code, "::text IN (", paste(sql_quote(s$codes), collapse = ", "), ")")
    d <- DBI::dbGetQuery(con, sql)
    if (!nrow(d)) return(NULL)
    sf::st_sf(mode = s$mode, source = d$source, code = d$code,
              geometry = sf::st_set_crs(sf::st_as_sfc(structure(as.character(d$geom), class = "WKB"),
                                                     EWKB = TRUE), 4326))
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  do.call(rbind, parts)
}

#' @noRd
fmt_n <- function(x, digits = 0) formatC(x, format = "f", digits = digits, big.mark = ",")
