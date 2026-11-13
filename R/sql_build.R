#' Build the SQL for a dataset
#'
#' Produces standalone, commented PostgreSQL/PostGIS SQL from a dataset spec.
#' Every query passes through the same steps:
#'
#' 1. `hauls_f`     hauls matching the haul-level filters and spatial filters
#' 2. `overlay`     intersect haul polygons with grid cells; share of haul area
#' 3. `trimmed`     drop small overlaps (`apportionment.min_share`), except exempt haul types
#' 4. `alloc`       renormalise shares so each haul's shares sum to 1
#' 5. `apportioned` join catch, apply catch-level filters, multiply by share
#' 6. aggregate by grid cell + grouping fields, applying confidentiality rules
#'
#' (With a precomputed `allocation_table` for the grid, steps 2-4 read it instead.)
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

  # Small-overlap trimming (see schema.yaml `apportionment`)
  A <- cfg$apportionment
  min_share <- A$min_share %||% 0
  exempt <- as.character(unlist(A$exempt_haul_types))
  trim <- min_share > 0
  use_alloc_table <- !is.null(grid$allocation_table)

  # Haul columns needed downstream: vessel, haul-level grouping fields, any
  # columns the confidentiality rules count or filter on, and haul type if the
  # trimming step exempts some haul types.
  rule_cols <- unlist(lapply(cfg$rules, function(r) c(r$column, r$filter$column)))
  trim_cols <- if (trim && length(exempt) && !use_alloc_table) H$haul_type
  haul_cols <- unique(c(H$vessel, haul_groups, rule_cols, trim_cols))
  haul_cols <- setdiff(haul_cols, c("haul_id", "haul_polygon"))

  hauls_f <- paste0(
    "-- 1. Hauls matching the haul-level filters",
    if (length(spec$spatial_filters)) paste0(
      " and spatial filters.\n",
      "--    A haul matches an area if their footprints share some area (touching\n",
      "--    boundaries does not count); matching hauls are kept or dropped whole.\n",
      "--    The checks are written \"(...) IS TRUE\" so PostgreSQL evaluates them per\n",
      "--    haul instead of as a join, whose row estimates would distort the plan."),
    "\n",
    "hauls_f AS (\n",
    "    SELECT ", col_as("h", H$key, "haul_id"),
    if (!use_alloc_table) paste0(",\n           ", col_as("h", H$geom, "haul_polygon")),
    paste(prefix(",\n           h.", haul_cols), collapse = ""), "\n",
    "    FROM ", hauls_tbl, " h",
    sql_where(haul_filters, cfg, "h",
              extra = vapply(spec$spatial_filters, spatial_filter_sql, "", cfg = cfg,
                             haul_geom = paste0("h.", H$geom))), "\n",
    ")"
  )

  if (use_alloc_table) {
    alloc <- paste0(
      "-- 2-4. Each haul's share of its catch in each grid cell (precomputed:\n",
      "--      overlay, small-overlap trimming and renormalisation already applied)\n",
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
      "-- 2. Overlay the grid: share of each haul's area falling in each cell.\n",
      "--    Zero-area overlaps (cells merely touching a haul's edge) are dropped.\n",
      "overlay AS (\n",
      "    SELECT * FROM (\n",
      "        SELECT f.haul_id,\n",
      if (length(trim_cols)) paste0("               f.", trim_cols, ",\n"),
      "               g.", grid$id, " AS grid_id,\n",
      "               ST_Area(ST_Intersection(f.haul_polygon, g.", grid$geom, ")) / ST_Area(f.haul_polygon) AS p_raw\n",
      "        FROM hauls_f f\n",
      "        JOIN ", grid_tbl, " g ON ST_Intersects(f.haul_polygon, g.", grid$geom, ")\n",
      "    ) o\n",
      "    WHERE p_raw > 0\n",
      ")"
    )
    share_lit <- format(min_share, scientific = FALSE)
    trimmed <- if (trim) paste0(
      "-- 3. Drop overlaps covering less than ", 100 * min_share, "% of a haul's area, so slivers\n",
      "--    neither receive catch nor count the vessel toward that cell.\n",
      if (length(exempt)) paste0("--    Exempt: ", paste(comment_safe(exempt), collapse = ", "),
                                 " hauls (their footprint is a whole statistical area).\n"),
      "--    A haul with no overlap at or above the threshold keeps all of its overlaps.\n",
      "trimmed AS (\n",
      "    SELECT haul_id, grid_id, p_raw\n",
      "    FROM (SELECT o.*, MAX(p_raw) OVER (PARTITION BY haul_id) AS p_max FROM overlay o) o\n",
      "    WHERE p_raw >= ", share_lit, "\n",
      if (length(exempt)) paste0("       OR ", H$haul_type, " IN (", paste(sql_quote(exempt), collapse = ", "), ")\n"),
      "       OR p_max < ", share_lit, "\n",
      ")"
    )
    alloc <- paste0(
      "-- 4. Renormalise so each haul's shares sum to 1 (covers trimmed overlaps,\n",
      "--    hauls extending past the grid edge, and rounding).\n",
      "alloc AS (\n",
      "    SELECT haul_id, grid_id,\n",
      "           p_raw / SUM(p_raw) OVER (PARTITION BY haul_id) AS p\n",
      "    FROM ", if (trim) "trimmed" else "overlay", "\n",
      ")"
    )
    alloc_ctes <- c(overlay, trimmed, alloc)
  }

  apportioned_cols <- c(
    "a.grid_id",
    prefix("f.", haul_cols),
    prefix("c.", catch_groups),
    paste0("c.", C$weight, " * a.p AS weight_mt"),
    paste0("c.", C$count, " * a.p AS count_n")
  )
  apportioned <- paste0(
    "-- 5. Join catch, apply catch-level filters, apportion catch by share\n",
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
    "-- 6. Aggregate by grid cell and grouping fields. Confidentiality rules:\n",
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
    "-- 6. Aggregate by grid cell and grouping fields, evaluating the\n",
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

  match_group <- paste0(
    "g.grid_id = a.grid_id",
    paste(prefix("\n                            AND g.",
                 prefix("", spec$group_by) |> (\(x) if (length(x)) paste0(x, " IS NOT DISTINCT FROM a.", x) else x)()),
          collapse = "")
  )
  dropped_diag <- paste0(
    "-- Can the suppressed records be described in aggregate (e.g. \"x t of catch\n",
    "-- was suppressed\")? Only if, taken together, they satisfy the same rules.\n",
    "dropped AS (\n",
    "    SELECT (", paste(rule_exprs, collapse = "\n            AND "), ") AS disclosable\n",
    "    FROM (\n",
    "        SELECT a.* FROM apportioned a\n",
    "        WHERE EXISTS (SELECT 1 FROM groups g\n",
    "                      WHERE NOT g.passes\n",
    "                        AND ", match_group, ")\n",
    "    ) d\n",
    ")"
  )

  list(
    ctes = c(hauls_f, alloc_ctes, apportioned),
    tail_ctes = list(deliver = agg_deliver, diagnostic = c(groups_diag, source_diag, dropped_diag),
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
    "       (SELECT count_n FROM source)                          AS count_source,\n",
    "       (SELECT disclosable FROM dropped)                     AS dropped_disclosable\n",
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
sql_where <- function(filters, cfg, alias, extra = character()) {
  clauses <- c(
    vapply(names(filters), function(f) filter_sql(f, filters[[f]], cfg$fields[[f]]$kind, alias), ""),
    extra
  )
  if (!length(clauses)) return("")
  paste0("\n    WHERE ", paste(clauses, collapse = "\n      AND "))
}

#' SQL condition for one spatial filter, applied to hauls
#'
#' `[NOT] EXISTS` over the areas view: the haul overlaps at least one of the
#' chosen areas with a shared area greater than zero.
#' @noRd
spatial_filter_sql <- function(s, cfg, haul_geom) {
  V <- cfg$areas$view
  geom <- paste0("sa.", V$geom)
  paste0(
    "(", if (s$mode == "exclude") "NOT ", "EXISTS (  -- ", spatial_filter_text(s, cfg), "\n",
    "          SELECT 1 FROM ", V$schema, ".", V$table, " sa\n",
    "          WHERE sa.", V$source, " = ", sql_quote(s$source), "\n",
    "            AND sa.", V$code, "::text IN (", paste(sql_quote(s$codes), collapse = ", "), ")\n",
    "            AND ST_Intersects(", geom, ", ", haul_geom, ")\n",
    "            AND NOT ST_Touches(", geom, ", ", haul_geom, "))) IS TRUE"
  )
}

#' One-line description of a spatial filter, e.g.
#' "include hauls overlapping NMFS reporting area 610 or 620"
#' @noRd
spatial_filter_text <- function(s, cfg) {
  codes <- s$codes
  listed <- if (length(codes) > 12) {
    paste0(paste(codes[1:10], collapse = ", "), " and ", length(codes) - 10, " more")
  } else if (length(codes) > 1) {
    paste0(paste(codes[-length(codes)], collapse = ", "), " or ", codes[length(codes)])
  } else codes
  comment_safe(paste0(s$mode, " hauls overlapping ", area_source_label(s$source, cfg), " ", listed))
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
method_lines <- function(cfg, audience = c("sql", "recipient")) {
  audience <- match.arg(audience)
  A <- cfg$apportionment
  min_share <- A$min_share %||% 0
  exempt <- as.character(unlist(A$exempt_haul_types))
  c(
    "Method: each haul's catch is apportioned across the grid cells its footprint",
    "overlaps, in proportion to the overlapping area. Area-only hauls use the full",
    "statistical area as their footprint.",
    if (min_share > 0) c(
      paste0("Overlaps covering less than ", 100 * min_share, "% of a haul's area are dropped",
             if (length(exempt)) {
               if (audience == "recipient") " (except area-only hauls)"
               else paste0(" (except ", paste(exempt, collapse = ", "), " hauls)")
             } else "",
             "."),
      "Shares are renormalised to sum to 1 per haul."
    ) else "Shares are renormalised to sum to 1 per haul."
  )
}

#' @noRd
sql_header <- function(spec, cfg, mode) {
  grid <- cfg$grids[[spec$grid]]
  filters <- if (length(spec$filters)) {
    vapply(names(spec$filters), function(f) {
      filter_sql(f, spec$filters[[f]], cfg$fields[[f]]$kind)
    }, "")
  } else "none (all records)"
  rules <- vapply(cfg$rules, function(r) r$description %||% r$name, "")
  spatial <- vapply(spec$spatial_filters, spatial_filter_text, "", cfg = cfg)

  lines <- c(
    paste0("CIA data request dataset: ", spec$name),
    if (nzchar(spec$description)) paste0("Description: ", spec$description),
    if (mode == "diagnostic") "DIAGNOSTIC QUERY: suppression summary, not for delivery.",
    if (mode == "allocation") "ALLOCATION QUERY: internal use only.",
    "",
    paste0("Grid:      ", grid$label %||% spec$grid, " (", grid$schema, ".", grid$table, ")"),
    paste0("Filters:   ", filters[1]),
    if (length(filters) > 1) paste0("           ", filters[-1]),
    if (length(spatial)) paste0(c("Spatial:   ", rep("           ", length(spatial) - 1)), spatial),
    paste0("Grouping:  ", paste(c("grid cell", spec$group_by), collapse = ", ")),
    "",
    "Confidentiality:",
    paste0("  - ", rules),
    "",
    method_lines(cfg),
    "",
    "Units:     catch_weight_mt = metric tons; catch_count = number of fish.",
    "CRS:       EPSG:3338 (NAD83 / Alaska Albers).",
    paste0("Generated by ciarequests ", utils::packageVersion("ciarequests"), ".")
  )
  paste0("-- ", comment_safe(lines), collapse = "\n")
}
