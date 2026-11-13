# Tests of the generated SQL against the synthetic PostGIS database.

test_that("each haul's shares sum to 1 and are positive", {
  con <- test_con()
  cfg <- cfg_default()
  for (g in names(cfg$grids)) {
    alloc <- DBI::dbGetQuery(con, build_sql_internal(dataset_spec("x", g), cfg, "allocation"))
    expect_true(all(alloc$p > 0), info = g)
    sums <- tapply(alloc$p, alloc$haul_id, sum)
    expect_lt(max(abs(sums - 1)), 1e-9, label = paste(g, "max deviation from 1"))
  }
})

test_that("catch is conserved: kept + dropped = source - catch of hauls entirely off-grid", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_8km", group_by = c("year", "species_code"))
  d <- DBI::dbGetQuery(con, build_sql(spec, cfg, mode = "diagnostic"))

  off_grid <- DBI::dbGetQuery(con, "
    SELECT COALESCE(SUM(c.weight), 0) AS w, COALESCE(SUM(c.count), 0) AS n
    FROM data.hauls h JOIN data.haul_catch c USING (haul_id)
    WHERE NOT EXISTS (
      SELECT 1 FROM data.hex_8km g
      WHERE ST_Intersects(h.haul_polygon, g.shape)
        AND ST_Area(ST_Intersection(h.haul_polygon, g.shape)) > 0)")
  expect_gt(off_grid$w, 0)  # the synthetic data has hauls beyond the grid edge

  total <- DBI::dbGetQuery(con, "SELECT SUM(weight) AS w, SUM(count) AS n FROM data.haul_catch")
  expect_equal(d$weight_source, total$w, tolerance = 1e-9)
  expect_equal(d$weight_kept + d$weight_dropped, d$weight_source - off_grid$w, tolerance = 1e-9)
  expect_equal(d$count_kept + d$count_dropped, d$count_source - off_grid$n, tolerance = 1e-9)
})

test_that("delivered results match an independent sf implementation", {
  skip_if_not_installed("sf")
  con <- test_con()
  cfg <- cfg_default()
  for (spec in example_specs()) {
    got <- run_deliver(con, spec, cfg)
    want <- ref_aggregate(con, spec, cfg)
    expect_gt(nrow(want), 0)
    expect_identical(got$key, want$key, label = paste(spec$name, "delivered groups"))
    expect_equal(got$catch_weight_mt, want$catch_weight_mt, tolerance = 1e-6,
                 label = paste(spec$name, "weights"))
    expect_equal(got$catch_count, want$catch_count, tolerance = 1e-6,
                 label = paste(spec$name, "counts"))
    expect_false("n_vessels" %in% names(got))  # vessel counts are never delivered
    expect_equal(sf::st_crs(got)$epsg, 3338L)
  }
})

test_that("the gate drops exactly the groups with fewer than 3 vessels", {
  skip_if_not_installed("sf")
  con <- test_con()
  cfg <- cfg_default()
  for (spec in example_specs()) {
    all_groups <- ref_aggregate(con, spec, cfg, gated = FALSE)
    got <- run_deliver(con, spec, cfg)
    expect_gt(sum(all_groups$n_vessels < 3), 0, label = paste(spec$name, "groups below threshold"))
    expect_setequal(got$key, all_groups$key[all_groups$n_vessels >= 3])
  }
})

test_that("diagnostic totals agree with the delivered rows and the reference", {
  skip_if_not_installed("sf")
  con <- test_con()
  cfg <- cfg_default()
  for (spec in example_specs()) {
    d <- DBI::dbGetQuery(con, build_sql(spec, cfg, mode = "diagnostic"))
    got <- run_deliver(con, spec, cfg)
    all_groups <- ref_aggregate(con, spec, cfg, gated = FALSE)
    dropped <- all_groups[all_groups$n_vessels < 3, ]

    expect_equal(d$rows_kept, nrow(got))
    expect_equal(d$rows_dropped, nrow(dropped))
    expect_equal(d$weight_kept, sum(got$catch_weight_mt), tolerance = 1e-6)
    expect_equal(d$weight_dropped, sum(dropped$catch_weight_mt), tolerance = 1e-6)
    expect_equal(d$count_dropped, sum(dropped$catch_count), tolerance = 1e-6)
  }
})

test_that("a precomputed allocation table gives the same results as the overlay", {
  con <- test_con()
  cfg <- cfg_default()
  DBI::dbExecute(con, "DROP TABLE IF EXISTS data.test_alloc_hex_20km")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE data.test_alloc_hex_20km AS ",
    sub(";\\s*$", "", build_sql_internal(dataset_spec("x", "hex_20km"), cfg, "allocation"))))
  withr::defer(DBI::dbExecute(con, "DROP TABLE IF EXISTS data.test_alloc_hex_20km"))

  cfg_alloc <- cfg
  cfg_alloc$grids$hex_20km$allocation_table <- "test_alloc_hex_20km"
  for (spec in Filter(function(s) s$grid == "hex_20km", example_specs())) {
    a <- DBI::dbGetQuery(con, build_sql(spec, cfg))
    b <- DBI::dbGetQuery(con, build_sql(spec, cfg_alloc))
    expect_equal(b[setdiff(names(b), "grid_shape")], a[setdiff(names(a), "grid_shape")],
                 tolerance = 1e-9, label = spec$name)
  }
})

test_that("a rule excluding area hauls from vessel counts only removes groups", {
  con <- test_con()
  cfg <- cfg_default()
  cfg_strict <- cfg
  cfg_strict$rules[[1]]$filter <- list(column = "haul_type", op = "not_in", values = list("AREA"))
  spec <- example_specs()[[2]]
  base <- DBI::dbGetQuery(con, build_sql(spec, cfg))
  strict <- DBI::dbGetQuery(con, build_sql(spec, cfg_strict))
  expect_lt(nrow(strict), nrow(base))
  k <- function(d) paste(d$grid_id, d$year, d$species_code)
  expect_true(all(k(strict) %in% k(base)))
})

test_that("hostile filter values are inert", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_20km",
                       filters = list(species_code = "POLL'); DROP TABLE data.hauls; --"))
  res <- DBI::dbGetQuery(con, build_sql(spec, cfg))
  expect_equal(nrow(res), 0)
  expect_gt(q1(con, "SELECT count(*) FROM data.hauls"), 0)
})
