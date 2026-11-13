#' Build the SQL for a dataset
#'
#' Produces standalone, commented PostgreSQL/PostGIS SQL from a dataset spec.
#' Every query passes through the same steps:
#'
#' 1. `hauls_f`     hauls matching the haul-level filters
#' 2. `overlay`     intersect haul polygons with grid cells; share of haul area
#' 3. `alloc`       renormalise shares so each haul's shares sum to 1
#' 4. `apportioned` join catch, apply catch-level filters, multiply by share
#' 5. aggregate by grid cell + grouping fields, applying confidentiality rules
#'
#' (With a precomputed `allocation_table` for the grid, steps 2-3 read it instead.)
#'
#' @param spec A [dataset_spec()].
#' @param cfg A [load_config()] result.
#' @param mode `"deliver"`: the gated query with grid geometry, as delivered.
#'   `"diagnostic"`: one row of kept/dropped totals for the suppression report
#'   (no geometry, rules evaluated per group rather than used to filter).
#' @return A single string of SQL.
#' @export
build_sql <- function(spec, cfg, mode = c("deliver", "diagnostic")) {
  mode <- match.arg(mode)
  build_sql_internal(spec, cfg, mode)
}

#' @noRd
build_sql_internal <- function(spec, cfg, mode = c("deliver", "diagnostic", "allocation")) {
  mode <- match.arg(mode)
  spec <- validate_spec(spec, cfg)
  p <- sql_parts(spec, cfg)

  final <- switch(mode,
    deliver    = sql_deliver_tail(p),
    diagnostic = sql_diagnostic_tail(p),
    allocation = "SELECT haul_id, grid_id, p\nFROM alloc\nORDER BY haul_id, grid_id;"
  )

  paste0(
    sql_header(spec, cfg, mode), "\n",
    "WITH\n",
    paste(p$ctes, collapse = ",\n"),
    if (length(p$tail_ctes[[mode]])) paste0(",\n", paste(p$tail_ctes[[mode]], collapse = ",\n")),
    "\n", final, "\n"
  )
}

# ---------------------------------------------------------------------------
# Query parts
# ---------------------------------------------------------------------------

#' @noRd
sql_parts <- function(spec, cfg) {
  H <- cfg$hauls
  C <- cfg$haul_catch
  grid <- cfg$grids[[spec$grid]]
  hauls_tbl <- paste0(cfg$schema, ".", H$table)
  catch_tbl <- paste0(cfg$schema, ".", C$table)
  grid_tbl  <- paste0(grid$schema, ".", grid$table)

  field_table <- vapply(cfg$fields, `[[`, "", "table")
  haul_filters  <- spec$filters[field_table[names(spec$filters)] == "hauls"]
  catch_filters <- spec$filters[field_table[names(spec$filters)] == "haul_catch"]
  haul_groups   <- spec$group_by[field_table[spec$group_by] == "hauls"]
  catch_groups  <- spec$group_by[field_table[spec$group_by] == "haul_catch"]

  # Haul columns needed downstream: vessel, haul-level grouping fields, and any
  # columns the confidentiality rules count or filter on.
  rule_cols <- unlist(lapply(cfg$rules, function(r) c(r$column, r$filter$column)))
  haul_cols <- unique(c(H$vessel, haul_groups, rule_cols))
  haul_cols <- setdiff(haul_cols, c("haul_id", "haul_polygon"))

  use_alloc_table <- !is.null(grid$allocation_table)

  hauls_f <- paste0(
    "-- 1. Hauls matching the haul-level filters\n",
    "hauls_f AS (\n",
    "    SELECT ", col_as("h", H$key, "haul_id"),
    if (!use_alloc_table) paste0(",\n           ", col_as("h", H$geom, "haul_polygon")),
    paste(prefix(",\n           h.", haul_cols), collapse = ""), "\n",
    "    FROM ", hauls_tbl, " h",
    sql_where(haul_filters, cfg, "h"), "\n",
    ")"
  )

  if (use_alloc_table) {
    alloc <- paste0(
      "-- 2-3. Each haul's share of its area in each grid cell (precomputed, renormalised)\n",
      "alloc AS (\n",
      "    SELECT t.haul_id, t.grid_id, t.p\n",
      "    FROM ", grid$schema, ".", grid$allocation_table, " t\n",
      "    JOIN hauls_f f ON f.haul_id = t.haul_id\n",
      "    WHERE t.p > 0\n",
      ")"
    )
    alloc_ctes <- alloc
  } else {
    overlay <- paste0(
      "-- 2. Overlay the grid: share of each haul's area falling in each cell\n",
      "overlay AS (\n",
      "    SELECT f.haul_id,\n",
      "           g.", grid$id, " AS grid_id,\n",
      "           ST_Area(ST_Intersection(f.haul_polygon, g.", grid$geom, ")) / ST_Area(f.haul_polygon) AS p_raw\n",
      "    FROM hauls_f f\n",
      "    JOIN ", grid_tbl, " g ON ST_Intersects(f.haul_polygon, g.", grid$geom, ")\n",
      ")"
    )
    alloc <- paste0(
      "-- 3. Renormalise so each haul's shares sum to 1 (covers hauls extending past\n",
      "--    the grid edge, and rounding). Zero-area overlaps (cells merely touching a\n",
      "--    haul's edge) are dropped so they neither receive catch nor count vessels.\n",
      "alloc AS (\n",
      "    SELECT haul_id, grid_id,\n",
      "           p_raw / SUM(p_raw) OVER (PARTITION BY haul_id) AS p\n",
      "    FROM overlay\n",
      "    WHERE p_raw > 0\n",
      ")"
    )
    alloc_ctes <- c(overlay, alloc)
  }

  apportioned_cols <- c(
    "a.grid_id",
    prefix("f.", haul_cols),
    prefix("c.", catch_groups),
    paste0("c.", C$weight, " * a.p AS weight_mt"),
    paste0("c.", C$count, " * a.p AS count_n")
  )
  apportioned <- paste0(
    "-- 4. Join catch, apply catch-level filters, apportion catch by share\n",
    "apportioned AS (\n",
    "    SELECT ", paste(apportioned_cols, collapse = ",\n           "), "\n",
    "    FROM alloc a\n",
    "    JOIN hauls_f f ON f.haul_id = a.haul_id\n",
    "    JOIN ", catch_tbl, " c ON c.", C$key, " = a.haul_id",
    sql_where(catch_filters, cfg, "c"), "\n",
    ")"
  )

  group_cols <- c("grid_id", spec$group_by)
  rule_exprs <- vapply(cfg$rules, rule_sql, "")
  rule_names <- vapply(cfg$rules, `[[`, "", "name")

  agg_deliver <- paste0(
    "-- 5. Aggregate by grid cell and grouping fields. Confidentiality rules:\n",
    "--    groups that fail any rule are not returned.\n",
    "agg AS (\n",
    "    SELECT ", paste(group_cols, collapse = ", "), ",\n",
    "           SUM(weight_mt) AS catch_weight_mt,\n",
    "           SUM(count_n)   AS catch_count\n",
    "    FROM apportioned\n",
    "    GROUP BY ", paste(group_cols, collapse = ", "), "\n",
    "    HAVING ", paste0(rule_exprs, "  -- rule: ", rule_names, collapse = "\n       AND "), "\n",
    ")"
  )

  groups_diag <- paste0(
    "-- 5. Aggregate by grid cell and grouping fields, evaluating the\n",
    "--    confidentiality rules per group instead of filtering on them.\n",
    "groups AS (\n",
    "    SELECT ", paste(group_cols, collapse = ", "), ",\n",
    "           SUM(weight_mt) AS weight_mt,\n",
    "           SUM(count_n)   AS count_n,\n",
    "           (", paste(rule_exprs, collapse = "\n            AND "), ") AS passes\n",
    "    FROM apportioned\n",
    "    GROUP BY ", paste(group_cols, collapse = ", "), "\n",
    ")"
  )
  source_diag <- paste0(
    "-- Total filtered catch before apportionment (catch from hauls with no\n",
    "-- overlap with the grid at all is the difference).\n",
    "source AS (\n",
    "    SELECT COALESCE(SUM(c.", C$weight, "), 0) AS weight_mt,\n",
    "           COALESCE(SUM(c.", C$count, "), 0)  AS count_n\n",
    "    FROM hauls_f f\n",
    "    JOIN ", catch_tbl, " c ON c.", C$key, " = f.haul_id",
    sql_where(catch_filters, cfg, "c"), "\n",
    ")"
  )

  list(
    ctes = c(hauls_f, alloc_ctes, apportioned),
    tail_ctes = list(deliver = agg_deliver, diagnostic = c(groups_diag, source_diag),
                     allocation = character()),
    group_cols = group_cols,
    grid = grid,
    grid_tbl = grid_tbl
  )
}

#' @noRd
sql_deliver_tail <- function(p) {
  paste0(
    "SELECT ", paste0("a.", p$group_cols, collapse = ", "), ",\n",
    "       a.catch_weight_mt,\n",
    "       a.catch_count,\n",
    "       g.", p$grid$geom, " AS grid_shape\n",
    "FROM agg a\n",
    "JOIN ", p$grid_tbl, " g ON g.", p$grid$id, " = a.grid_id\n",
    "ORDER BY ", paste0("a.", p$group_cols, collapse = ", "), ";"
  )
}

#' @noRd
sql_diagnostic_tail <- function(p) {
  paste0(
    "SELECT (COUNT(*) FILTER (WHERE passes))::integer         AS rows_kept,\n",
    "       (COUNT(*) FILTER (WHERE NOT passes))::integer     AS rows_dropped,\n",
    "       COALESCE(SUM(weight_mt) FILTER (WHERE passes), 0)     AS weight_kept,\n",
    "       COALESCE(SUM(weight_mt) FILTER (WHERE NOT passes), 0) AS weight_dropped,\n",
    "       COALESCE(SUM(count_n) FILTER (WHERE passes), 0)       AS count_kept,\n",
    "       COALESCE(SUM(count_n) FILTER (WHERE NOT passes), 0)   AS count_dropped,\n",
    "       (SELECT weight_mt FROM source)                        AS weight_source,\n",
    "       (SELECT count_n FROM source)                          AS count_source\n",
    "FROM groups;"
  )
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' "alias.col" or "alias.col AS name" when the names differ
#' @noRd
col_as <- function(alias, col, name) {
  if (identical(col, name)) paste0(alias, ".", col) else paste0(alias, ".", col, " AS ", name)
}

#' paste0() that returns nothing for an empty vector (paste0("a", character()) is "a")
#' @noRd
prefix <- function(p, x) if (length(x)) paste0(p, x) else character()

#' SQL for one confidentiality rule, evaluated over `apportioned`
#' @noRd
rule_sql <- function(r) {
  expr <- paste0("COUNT(DISTINCT ", r$column, ")")
  if (!is.null(r$filter)) {
    op <- if (r$filter$op == "in") "IN" else "NOT IN"
    expr <- paste0(expr, " FILTER (WHERE ", r$filter$column, " ", op, " (",
                   paste(sql_quote(as.character(r$filter$values)), collapse = ", "), "))")
  }
  paste0(expr, " >= ", as.integer(r$min))
}

#' WHERE clause for a set of (validated, normalised) filters
#' @noRd
sql_where <- function(filters, cfg, alias) {
  if (!length(filters)) return("")
  clauses <- vapply(names(filters), function(f) {
    filter_sql(f, filters[[f]], cfg$fields[[f]]$kind, alias)
  }, "")
  paste0("\n    WHERE ", paste(clauses, collapse = "\n      AND "))
}

#' @noRd
filter_sql <- function(field, value, kind, alias = NULL) {
  col <- if (is.null(alias)) field else paste0(alias, ".", field)
  if (kind == "set") {
    return(paste0(col, " IN (", paste(sql_quote(value), collapse = ", "), ")"))
  }
  if (!is.null(value$min) && !is.null(value$max)) {
    if (value$min == value$max) return(paste0(col, " = ", value$min))
    return(paste0(col, " BETWEEN ", value$min, " AND ", value$max))
  }
  if (!is.null(value$min)) return(paste0(col, " >= ", value$min))
  paste0(col, " <= ", value$max)
}

#' Quote text values as SQL string literals (standard SQL escaping)
#' @noRd
sql_quote <- function(x) {
  as.character(DBI::dbQuoteString(DBI::ANSI(), x))
}

#' Make arbitrary text safe to place in a `--` comment
#' @noRd
comment_safe <- function(x) gsub("[[:cntrl:]]+", " ", x)

#' @noRd
sql_header <- function(spec, cfg, mode) {
  grid <- cfg$grids[[spec$grid]]
  filters <- if (length(spec$filters)) {
    vapply(names(spec$filters), function(f) {
      filter_sql(f, spec$filters[[f]], cfg$fields[[f]]$kind)
    }, "")
  } else "none (all records)"
  rules <- vapply(cfg$rules, function(r) r$description %||% r$name, "")

  lines <- c(
    paste0("CIA data request dataset: ", spec$name),
    if (nzchar(spec$description)) paste0("Description: ", spec$description),
    if (mode == "diagnostic") "DIAGNOSTIC QUERY: suppression summary, not for delivery.",
    if (mode == "allocation") "ALLOCATION QUERY: internal use only.",
    "",
    paste0("Grid:      ", grid$label %||% spec$grid, " (", grid$schema, ".", grid$table, ")"),
    paste0("Filters:   ", filters[1]),
    if (length(filters) > 1) paste0("           ", filters[-1]),
    paste0("Grouping:  ", paste(c("grid cell", spec$group_by), collapse = ", ")),
    "",
    "Confidentiality:",
    paste0("  - ", rules),
    "",
    "Method: each haul's catch is apportioned across the grid cells its footprint",
    "overlaps, in proportion to the overlapping area (shares renormalised to sum",
    "to 1 per haul). Area-only hauls use the full statistical area as footprint.",
    "",
    "Units:     catch_weight_mt = metric tons; catch_count = number of fish.",
    "CRS:       EPSG:3338 (NAD83 / Alaska Albers).",
    paste0("Generated by ciarequests ", utils::packageVersion("ciarequests"), ".")
  )
  paste0("-- ", comment_safe(lines), collapse = "\n")
}
