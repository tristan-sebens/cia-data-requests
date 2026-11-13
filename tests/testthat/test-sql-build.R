# Offline tests of the generated SQL (no database needed).

gate <- "HAVING COUNT(DISTINCT vessel_id) >= 3"

test_that("every delivered query carries the confidentiality gate", {
  cfg <- cfg_default()
  set_fields <- names(Filter(function(f) f$kind == "set", cfg$fields))
  set.seed(1)
  for (i in 1:200) {
    fs <- sample(set_fields, sample(0:4, 1))
    filters <- setNames(lapply(fs, function(f) sample(c("A", "B", "C"), sample(1:3, 1))), fs)
    if (runif(1) < 0.5) filters$year <- list(min = 2018 + sample(0:3, 1))
    spec <- dataset_spec(paste0("ds_", i), sample(names(cfg$grids), 1),
                         filters = filters,
                         group_by = sample(names(cfg$fields), sample(0:5, 1)))
    sql <- build_sql(spec, cfg)
    expect_true(grepl(gate, sql, fixed = TRUE), info = spec$name)
  }
})

test_that("the diagnostic query evaluates the rules but does not filter or return geometry", {
  cfg <- cfg_default()
  sql <- build_sql(example_specs()[[3]], cfg, mode = "diagnostic")
  expect_false(grepl("HAVING", sql, fixed = TRUE))
  expect_false(grepl("grid_shape", sql, fixed = TRUE))
  expect_true(grepl("COUNT(DISTINCT vessel_id) >= 3) AS passes", sql, fixed = TRUE))
})

test_that("filter values are quoted as literals", {
  cfg <- cfg_default()
  evil <- "POLL'); DROP TABLE data.hauls; --"
  sql <- build_sql(dataset_spec("x", "hex_8km", filters = list(species_code = evil)), cfg)
  expect_true(grepl("c.species_code IN ('POLL''); DROP TABLE data.hauls; --')", sql, fixed = TRUE))
})

test_that("free text cannot escape the header comments", {
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_8km",
                       description = "line one\nDROP TABLE data.hauls;\r\nline three",
                       filters = list(species_code = "A\nB"))
  lines <- strsplit(build_sql(spec, cfg), "\n", fixed = TRUE)[[1]]
  header <- lines[seq_len(which(lines == "WITH") - 1)]
  expect_true(all(startsWith(header, "--")))
})

test_that("haul-level and catch-level filters land in the right step", {
  cfg <- cfg_default()
  sql <- build_sql(example_specs()[[3]], cfg)
  hauls_f <- regmatches(sql, regexpr("(?s)hauls_f AS \\(.*?\n\\)", sql, perl = TRUE))
  expect_match(hauls_f, "h.gear_code IN ('TRW')", fixed = TRUE)
  expect_match(hauls_f, "h.year BETWEEN 2019 AND 2023", fixed = TRUE)
  expect_no_match(hauls_f, "species", fixed = TRUE)
  expect_match(sql, "c.species_group_code IN ('AKFL', 'FLAT')", fixed = TRUE)
})

test_that("unfiltered specs have no WHERE in the haul step and open ranges work", {
  cfg <- cfg_default()
  sql <- build_sql(dataset_spec("x", "hex_8km"), cfg)
  expect_no_match(sql, "WHERE h.", fixed = TRUE)
  sql <- build_sql(dataset_spec("x", "hex_8km", filters = list(year = list(max = 2020))), cfg)
  expect_match(sql, "h.year <= 2020", fixed = TRUE)
  sql <- build_sql(dataset_spec("x", "hex_8km", filters = list(year = list(min = 2020, max = 2020))), cfg)
  expect_match(sql, "h.year = 2020", fixed = TRUE)
})

test_that("a precomputed allocation table replaces the overlay", {
  cfg <- cfg_default()
  cfg$grids$hex_8km$allocation_table <- "hex_8km_alloc"
  sql <- build_sql(dataset_spec("x", "hex_8km"), cfg)
  expect_match(sql, "FROM data.hex_8km_alloc t", fixed = TRUE)
  expect_no_match(sql, "ST_Intersection", fixed = TRUE)
  expect_match(sql, gate, fixed = TRUE)
})

test_that("rules with a filter generate FILTER clauses", {
  cfg <- cfg_default()
  cfg$rules[[1]]$filter <- list(column = "haul_type", op = "not_in", values = list("AREA"))
  sql <- build_sql(dataset_spec("x", "hex_8km"), cfg)
  expect_match(sql, "COUNT(DISTINCT vessel_id) FILTER (WHERE haul_type NOT IN ('AREA')) >= 3",
               fixed = TRUE)
  expect_match(sql, "h.haul_type", fixed = TRUE)  # column carried through
})

test_that("generated SQL is stable (snapshot)", {
  cfg <- cfg_default()
  expect_snapshot(cat(build_sql(example_specs()[[3]], cfg)), transform = hide_version)
  expect_snapshot(cat(build_sql(example_specs()[[2]], cfg, mode = "diagnostic")),
                  transform = hide_version)
})
