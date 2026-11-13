test_that("suppression_stats computes totals and percentages", {
  diag <- data.frame(rows_kept = 75L, rows_dropped = 25L,
                     weight_kept = 990, weight_dropped = 10,
                     count_kept = 1800, count_dropped = 200,
                     weight_source = 1100, count_source = 2000)
  s <- suppression_stats(diag)
  expect_equal(s$rows_total, 100L)
  expect_equal(s$pct_rows_dropped, 25)
  expect_equal(s$pct_weight_dropped, 1)
  expect_equal(s$pct_count_dropped, 10)
  expect_equal(s$weight_off_grid, 100)
  expect_equal(s$count_off_grid, 0)

  empty <- suppression_stats(data.frame(rows_kept = 0L, rows_dropped = 0L,
                                        weight_kept = 0, weight_dropped = 0,
                                        count_kept = 0, count_dropped = 0,
                                        weight_source = 0, count_source = 0))
  expect_equal(empty$pct_rows_dropped, 0)
})

test_that("run_dataset returns the delivered data and consistent statistics", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- example_specs()[[3]]
  r <- run_dataset(con, spec, cfg)

  expect_s3_class(r, "cia_dataset_result")
  expect_s3_class(r$data, "sf")
  expect_equal(sf::st_crs(r$data)$epsg, 3338L)
  expect_named(r$data, c("grid_id", spec$group_by, "catch_weight_mt", "catch_count", "grid_shape"))
  expect_type(r$data$grid_id, "integer")
  expect_true(all(sf::st_geometry_type(r$data) == "POLYGON"))

  # Same rows, in the same order, as running the SQL file directly.
  direct <- sf::st_read(con, query = r$sql, quiet = TRUE)
  expect_equal(sf::st_drop_geometry(r$data), sf::st_drop_geometry(direct), ignore_attr = TRUE)
  expect_true(all(sf::st_equals(r$data, direct, sparse = FALSE) |> diag()))

  expect_equal(r$stats$rows_kept, nrow(r$data))
  expect_equal(r$stats$weight_kept, sum(r$data$catch_weight_mt), tolerance = 1e-9)
  expect_gt(r$stats$rows_dropped, 0)
  expect_identical(r$sql_hash, sql_hash(build_sql(spec, cfg)))
  expect_output(print(r), "suppressed")
})

test_that("rows drop off faster than catch as grouping gets finer", {
  # The pattern seen in real requests: fine grouping removes many rows but
  # little catch, because suppressed groups are small.
  con <- test_con()
  cfg <- cfg_default()
  coarse <- run_dataset(con, dataset_spec("coarse", "hex_20km"), cfg)
  fine <- run_dataset(con, dataset_spec("fine", "hex_8km",
                                        group_by = c("year", "gear_code", "species_code")), cfg)
  expect_gt(fine$stats$pct_rows_dropped, coarse$stats$pct_rows_dropped)
  expect_gt(fine$stats$pct_rows_dropped, fine$stats$pct_weight_dropped)
})

test_that("off-grid catch is reported separately from suppression", {
  con <- test_con()
  r <- run_dataset(con, dataset_spec("x", "hex_8km"), cfg_default())
  expect_gt(r$stats$weight_off_grid, 0)
  expect_equal(r$stats$weight_kept + r$stats$weight_dropped + r$stats$weight_off_grid,
               r$stats$weight_source, tolerance = 1e-9)
})

test_that("a dataset with no rows still returns a valid sf object", {
  con <- test_con()
  r <- run_dataset(con, dataset_spec("x", "hex_8km", filters = list(species_code = "NONE"),
                                     group_by = "year"), cfg_default())
  expect_s3_class(r$data, "sf")
  expect_equal(nrow(r$data), 0)
  expect_equal(sf::st_crs(r$data)$epsg, 3338L)
  expect_equal(r$stats$rows_total, 0L)
})

test_that("field_values lists known values without missing ones", {
  con <- test_con()
  cfg <- cfg_default()
  sp <- field_values(con, cfg, "species_code")
  expect_true("POLL" %in% sp)
  expect_false(anyNA(sp))
  expect_identical(sp, sort(sp))
  expect_equal(field_values(con, cfg, "year"), c(min = 2018L, max = 2024L))
  expect_error(field_values(con, cfg, "colour"), "Unknown field")
})

test_that("unknown_filter_values catches typos", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_8km",
                       filters = list(species_code = c("POLL", "POLX"), gear_code = "TRW",
                                      year = list(min = 1900)))
  u <- unknown_filter_values(con, spec, cfg)
  expect_equal(u, data.frame(field = "species_code", value = "POLX"))
  ok <- dataset_spec("x", "hex_8km", filters = list(species_code = "POLL"))
  expect_equal(nrow(unknown_filter_values(con, ok, cfg)), 0)
})
