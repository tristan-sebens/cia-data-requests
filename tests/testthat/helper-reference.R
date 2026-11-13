# Independent reference implementation of the apportionment + confidentiality
# gate, written in R with sf rather than SQL. The database tests compare the
# generated SQL's results against this.

ref_hauls <- local({
  cache <- NULL
  function(con) {
    if (is.null(cache)) {
      cache <<- sf::st_read(con, query = "SELECT * FROM data.hauls", quiet = TRUE)
    }
    cache
  }
})

ref_catch <- local({
  cache <- NULL
  function(con) {
    if (is.null(cache)) cache <<- DBI::dbGetQuery(con, "SELECT * FROM data.haul_catch")
    cache
  }
})

ref_grid <- local({
  cache <- list()
  function(con, grid) {
    key <- paste(grid$schema, grid$table)
    if (is.null(cache[[key]])) {
      cache[[key]] <<- sf::st_read(con, quiet = TRUE, query = sprintf(
        "SELECT %s AS grid_id, %s AS geom FROM %s.%s", grid$id, grid$geom, grid$schema, grid$table))
    }
    cache[[key]]
  }
})

ref_apply_filters <- function(df, filters, fields) {
  for (f in intersect(names(filters), names(df))) {
    v <- filters[[f]]
    keep <- if (fields[[f]]$kind == "set") {
      df[[f]] %in% v
    } else {
      (is.null(v$min) | df[[f]] >= (v$min %||% -Inf)) & (df[[f]] <= (v$max %||% Inf))
    }
    df <- df[!is.na(keep) & keep, , drop = FALSE]
  }
  df
}

# Spatial filters: keep (include) or drop (exclude) hauls whose interior
# intersects the interior of any chosen area (DE-9IM "T********"), i.e. a
# shared area, not just a shared boundary.
ref_spatial_filter <- function(con, hauls, spatial_filters) {
  for (s in spatial_filters) {
    areas <- sf::st_read(con, quiet = TRUE, query = sprintf(
      "SELECT shape FROM data.spatial_areas WHERE source = '%s' AND area_code::text IN (%s)",
      s$source, paste0("'", s$codes, "'", collapse = ", ")))
    hit <- lengths(sf::st_relate(hauls, areas, pattern = "T********")) > 0
    hauls <- hauls[if (s$mode == "include") hit else !hit, ]
  }
  hauls
}

# Grouping key that keeps NA as its own group (like SQL GROUP BY).
ref_key <- function(df, cols) {
  parts <- lapply(cols, function(c) {
    x <- df[[c]]
    ifelse(is.na(x), "<NA>", as.character(x))
  })
  do.call(paste, c(parts, sep = "\u001f"))
}

#' Returns a data.frame of delivered groups: group columns, catch_weight_mt,
#' catch_count, n_vessels. With gated = FALSE, every group is returned.
ref_aggregate <- function(con, spec, cfg, gated = TRUE, min_vessels = 3) {
  spec <- validate_spec(spec, cfg)
  grid <- cfg$grids[[spec$grid]]

  hauls <- ref_apply_filters(ref_hauls(con), spec$filters, cfg$fields)
  hauls <- ref_spatial_filter(con, hauls, spec$spatial_filters)
  cells <- ref_grid(con, grid)

  hauls$haul_area <- as.numeric(sf::st_area(hauls))
  inter <- suppressWarnings(sf::st_intersection(hauls[, c("haul_id", "haul_type", "haul_area")], cells))
  inter$p_raw <- as.numeric(sf::st_area(inter)) / inter$haul_area
  alloc <- sf::st_drop_geometry(inter)[inter$p_raw > 0, c("haul_id", "haul_type", "grid_id", "p_raw")]

  # Trim small overlaps, except exempt haul types and hauls with no large overlap.
  min_share <- cfg$apportionment$min_share %||% 0
  if (min_share > 0) {
    p_max <- ave(alloc$p_raw, alloc$haul_id, FUN = max)
    exempt <- alloc$haul_type %in% unlist(cfg$apportionment$exempt_haul_types)
    alloc <- alloc[alloc$p_raw >= min_share | exempt | p_max < min_share, ]
  }
  alloc$haul_type <- NULL
  alloc$p <- alloc$p_raw / ave(alloc$p_raw, alloc$haul_id, FUN = sum)

  attrs <- sf::st_drop_geometry(hauls)
  catch <- ref_apply_filters(ref_catch(con), spec$filters, cfg$fields)

  x <- merge(alloc, attrs, by = "haul_id")
  x <- merge(x, catch, by = "haul_id")
  x$w <- x$weight * x$p
  x$n <- x$count * x$p

  cols <- c("grid_id", spec$group_by)
  x$key <- ref_key(x, cols)
  out <- x[!duplicated(x$key), c("key", cols), drop = FALSE]
  out$catch_weight_mt <- as.numeric(tapply(x$w, x$key, sum)[out$key])
  out$catch_count     <- as.numeric(tapply(x$n, x$key, sum)[out$key])
  out$n_vessels       <- as.integer(tapply(x$vessel_id, x$key, function(v) length(unique(v)))[out$key])
  failing <- out$key[out$n_vessels < min_vessels]
  dropped_vessels <- length(unique(x$vessel_id[x$key %in% failing]))
  if (gated) out <- out[out$n_vessels >= min_vessels, , drop = FALSE]
  out <- out[order(out$key), , drop = FALSE]
  attr(out, "dropped_vessels") <- dropped_vessels
  out
}

# Run a dataset's delivered SQL and key the result the same way.
run_deliver <- function(con, spec, cfg) {
  res <- sf::st_read(con, query = build_sql(spec, cfg), quiet = TRUE)
  res$key <- ref_key(res, c("grid_id", spec$group_by))
  res[order(res$key), , drop = FALSE]
}
