test_that("valid specs pass and are normalised", {
  cfg <- cfg_default()
  s <- validate_spec(dataset_spec(
    "x", "hex_8km",
    filters = list(species_code = c("PCOD", "POLL", "PCOD"), gear_code = character(),
                   year = list(min = 2018, max = NULL)),
    group_by = "year"
  ), cfg)
  expect_equal(names(s$filters), c("species_code", "year"))  # empty filter dropped, sorted
  expect_equal(s$filters$species_code, c("PCOD", "POLL"))
  expect_identical(s$filters$year, list(min = 2018L))
})

test_that("invalid specs are rejected with useful messages", {
  cfg <- cfg_default()
  v <- function(...) validate_spec(dataset_spec(...), cfg)
  expect_error(v("Bad Name", "hex_8km"), "Invalid dataset name")
  expect_error(v("x", "hex_99km"), "unknown grid")
  expect_error(v("x", "hex_8km", filters = list(colour = "red")), "unknown filter field")
  expect_error(v("x", "hex_8km", filters = list(species_code = 1)), "non-empty text")
  expect_error(v("x", "hex_8km", filters = list(species_code = c("POLL", NA))), "non-empty text")
  expect_error(v("x", "hex_8km", filters = list(year = 2020)), "list\\(min")
  expect_error(v("x", "hex_8km", filters = list(year = list(min = 2024, max = 2020))), "min > max")
  expect_error(v("x", "hex_8km", filters = list(year = list(min = 2020.5))), "whole numbers")
  expect_error(v("x", "hex_8km", group_by = "colour"), "unknown grouping")
  expect_error(v("x", "hex_8km", group_by = c("year", "year")), "more than once")
})

test_that("spec hash ignores order of filters and values but not grouping order", {
  cfg <- cfg_default()
  a <- dataset_spec("x", "hex_8km",
                    filters = list(species_code = c("POLL", "PCOD"), gear_code = "TRW"),
                    group_by = c("year", "gear_code"))
  b <- dataset_spec("x", "hex_8km",
                    filters = list(gear_code = "TRW", species_code = c("PCOD", "POLL")),
                    group_by = c("year", "gear_code"))
  c <- dataset_spec("x", "hex_8km",
                    filters = list(gear_code = "TRW", species_code = c("PCOD", "POLL")),
                    group_by = c("gear_code", "year"))
  expect_identical(spec_hash(a, cfg), spec_hash(b, cfg))
  expect_false(identical(spec_hash(a, cfg), spec_hash(c, cfg)))
})

test_that("specs survive a JSON round trip", {
  cfg <- cfg_default()
  for (s in example_specs()) {
    back <- spec_from_json(spec_to_json(s))
    expect_identical(spec_hash(back, cfg), spec_hash(s, cfg))
    expect_identical(build_sql(back, cfg), build_sql(s, cfg))
  }
})
