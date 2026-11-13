# Spatial filters: spec handling, SQL and results against the synthetic
# areas view (docker/postgis-init/02_spatial_areas.sql).

nmfs <- function(mode, codes) list(mode = mode, source = "NMFS_REPORTING_AREA", codes = codes)

# --- Spec ---------------------------------------------------------------------

test_that("spatial filters are validated and normalised", {
  cfg <- cfg_default()
  s <- validate_spec(dataset_spec("x", "hex_8km",
                                  spatial_filters = list(nmfs("include", c("640", "620", "620", "1000")))), cfg)
  expect_equal(s$spatial_filters[[1]]$codes, c("620", "640", "1000"))  # numeric order, de-duplicated

  bad <- function(sf) dataset_spec("x", "hex_8km", spatial_filters = list(sf))
  expect_error(validate_spec(bad(nmfs("within", "610")), cfg), "include' or 'exclude")
  expect_error(validate_spec(bad(nmfs("include", character())), cfg), "choose at least one area")
  expect_error(validate_spec(bad(list(mode = "include", source = "x'; DROP", codes = "1")), cfg),
               "choose an area type")
  expect_error(validate_spec(bad(list(mode = "include", source = "A", codes = "1", extra = 1)), cfg),
               "must be list")

  no_areas <- cfg
  no_areas$areas <- NULL
  expect_error(validate_spec(bad(nmfs("include", "610")), no_areas), "areas.yaml")
})

test_that("spatial filters survive a JSON round trip; specs without them keep their old JSON", {
  cfg <- cfg_default()
  spec <- validate_spec(dataset_spec("x", "hex_8km", filters = list(gear_code = "TRW"),
                                     spatial_filters = list(nmfs("include", "610"),
                                                            nmfs("exclude", c("620", "630")))), cfg)
  back <- validate_spec(spec_from_json(spec_to_json(spec)), cfg)
  expect_identical(back$spatial_filters, spec$spatial_filters)
  expect_equal(spec_hash(back, cfg), spec_hash(spec, cfg))

  plain <- validate_spec(dataset_spec("x", "hex_8km", filters = list(gear_code = "TRW")), cfg)
  expect_no_match(as.character(spec_to_json(plain)), "spatial")
})

# --- SQL ------------------------------------------------------------------------

test_that("spatial filters become EXISTS / NOT EXISTS in the haul step", {
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_8km", filters = list(gear_code = "TRW"),
                       spatial_filters = list(nmfs("include", c("610", "620")), nmfs("exclude", "620")))
  sql <- build_sql(spec, cfg)
  hauls_step <- sub("(?s).*hauls_f AS \\((.*?)\n\\),.*", "\\1", sql, perl = TRUE)
  expect_match(hauls_step, "WHERE h.gear_code IN ('TRW')\n      AND (EXISTS (", fixed = TRUE)
  expect_match(hauls_step, "AND (NOT EXISTS (", fixed = TRUE)
  expect_match(hauls_step, "h.haul_polygon))) IS TRUE", fixed = TRUE)
  expect_match(hauls_step, "sa.source = 'NMFS_REPORTING_AREA'", fixed = TRUE)
  expect_match(hauls_step, "sa.area_code::text IN ('610', '620')", fixed = TRUE)
  expect_match(hauls_step, "NOT ST_Touches(sa.shape, h.haul_polygon)", fixed = TRUE)
  expect_match(sql, "-- Spatial:   include hauls overlapping NMFS reporting area 610 or 620", fixed = TRUE)
  expect_match(sql, "--            exclude hauls overlapping NMFS reporting area 620", fixed = TRUE)

  # Codes are quoted literals.
  inj <- dataset_spec("x", "hex_8km", spatial_filters = list(nmfs("include", "1') OR (1=1")))
  expect_match(build_sql(inj, cfg), "IN ('1'') OR (1=1')", fixed = TRUE)
})

test_that("SQL for datasets without spatial filters is unchanged", {
  cfg <- cfg_default()
  for (spec in example_specs()) expect_no_match(build_sql(spec, cfg), "spatial|EXISTS \\(  --|IS TRUE")
})

test_that("generated spatial SQL is stable (snapshot)", {
  cfg <- cfg_default()
  spec <- dataset_spec("pollock_610_620", "hex_20km",
                       filters = list(species_code = "POLL"), group_by = "year",
                       spatial_filters = list(nmfs("include", c("610", "620"))))
  expect_snapshot(cat(build_sql(spec, cfg)), transform = hide_version)
})

# --- Against the database ----------------------------------------------------------

test_that("the synthetic areas view has the expected sources", {
  con <- test_con()
  cfg <- cfg_default()
  a <- area_values(con, cfg)
  expect_setequal(unique(a$source), c("ADFG_STAT_AREA", "NMFS_REPORTING_AREA", "HEX_8KM", "HEX_20KM"))
  expect_equal(a$code[a$source == "NMFS_REPORTING_AREA"], c("610", "620", "630", "640"))
  expect_equal(order_area_sources(unique(a$source), cfg),
               c("NMFS_REPORTING_AREA", "ADFG_STAT_AREA", "HEX_20KM", "HEX_8KM"))
  expect_equal(order_area_sources(c("HEX_8KM", "ZZ_NEW", "NMFS_REPORTING_AREA"), cfg),
               c("NMFS_REPORTING_AREA", "HEX_8KM", "ZZ_NEW"))
})

test_that("spatially filtered results match the sf reference implementation", {
  skip_if_not_installed("sf")
  con <- test_con()
  cfg <- cfg_default()
  specs <- list(
    dataset_spec("incl_620", "hex_8km", group_by = "year",
                 spatial_filters = list(nmfs("include", "620"))),
    dataset_spec("excl_620_630", "hex_20km", filters = list(species_code = "POLL"),
                 spatial_filters = list(nmfs("exclude", c("620", "630")))),
    dataset_spec("incl_stat_excl_nmfs", "hex_8km",
                 spatial_filters = list(
                   list(mode = "include", source = "ADFG_STAT_AREA", codes = as.character(500001:500030)),
                   nmfs("exclude", "610"))),
    dataset_spec("incl_hex_cells", "hex_20km",
                 spatial_filters = list(list(mode = "include", source = "HEX_20KM",
                                             codes = as.character(30:60))))
  )
  for (spec in specs) {
    got <- run_deliver(con, spec, cfg)
    want <- ref_aggregate(con, spec, cfg)
    expect_gt(nrow(want), 0, label = paste(spec$name, "reference rows"))
    expect_identical(got$key, want$key, label = paste(spec$name, "delivered groups"))
    expect_equal(got$catch_weight_mt, want$catch_weight_mt, tolerance = 1e-6, label = spec$name)
    expect_equal(got$catch_count, want$catch_count, tolerance = 1e-6, label = spec$name)
  }
})

test_that("hauls are kept or dropped whole; touching a boundary is not overlapping", {
  con <- test_con()
  cfg <- cfg_default()
  matched <- function(mode) {
    spec <- validate_spec(dataset_spec("x", "hex_8km", spatial_filters = list(nmfs(mode, "620"))), cfg)
    sql <- sub("(?s)^.*?WITH\n(.*?\n\\)),\n-- 2\\..*$", "WITH \\1 SELECT haul_id FROM hauls_f", build_sql(spec, cfg),
               perl = TRUE)
    DBI::dbGetQuery(con, sql)$haul_id
  }
  inc <- matched("include")
  exc <- matched("exclude")
  all <- DBI::dbGetQuery(con, "SELECT haul_id FROM data.hauls")$haul_id
  expect_setequal(c(inc, exc), all)          # include and exclude partition the hauls
  expect_length(intersect(inc, exc), 0)

  # Area 620 is x = 100-200 km. AREA hauls on the 50-100 km stat areas share
  # only the x = 100 km boundary with it, so are not included...
  edge <- DBI::dbGetQuery(con, "
    SELECT haul_id FROM data.hauls
    WHERE haul_type = 'AREA' AND ST_XMin(haul_polygon) = 50000 AND ST_XMax(haul_polygon) = 100000")$haul_id
  expect_gt(length(edge), 0)
  expect_length(intersect(edge, inc), 0)
  # ...while a GPS haul crossing x = 200 km is included whole.
  crossing <- DBI::dbGetQuery(con, "
    SELECT haul_id FROM data.hauls
    WHERE haul_type <> 'AREA' AND ST_XMin(haul_polygon) < 200000 AND ST_XMax(haul_polygon) > 200000")$haul_id
  expect_gt(length(crossing), 0)
  expect_true(all(crossing %in% inc))
})

test_that("unknown area codes and sources are reported before running", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- dataset_spec("x", "hex_8km", spatial_filters = list(
    nmfs("include", c("610", "699")),
    list(mode = "exclude", source = "NO_SUCH_SOURCE", codes = "1")))
  u <- unknown_filter_values(con, spec, cfg)
  expect_equal(u$value, c("699", "1"))
  expect_match(u$field[1], "NMFS reporting area")

  r <- run_dataset_checked(con, spec, cfg)
  expect_s3_class(r, "cia_dataset_error")
  expect_match(r$error, "699")
})

test_that("retired stat area definitions are not used", {
  con <- test_con()
  cfg <- cfg_default()
  # The retired definition of 500001 lies 2,000 km north of every haul; only
  # the current one (the first stat area) should match.
  spec <- dataset_spec("x", "hex_8km", spatial_filters = list(
    list(mode = "include", source = "ADFG_STAT_AREA", codes = "500001")))
  outl <- area_outlines(con, validate_spec(spec, cfg), cfg)
  expect_equal(nrow(outl), 1)
  expect_equal(sf::st_crs(outl)$epsg, 4326L)
  expect_lt(sf::st_bbox(sf::st_transform(outl, 3338))[["ymax"]], 1300001)
})

test_that("the preview carries area outlines and the README describes the filter", {
  con <- test_con()
  cfg <- cfg_default()
  spec <- dataset_spec("pollock_620", "hex_20km", filters = list(species_code = "POLL"),
                       spatial_filters = list(nmfs("include", c("620", "630")), nmfs("exclude", "630")))
  r <- run_dataset_checked(con, spec, cfg)
  expect_s3_class(r, "cia_dataset_result")
  expect_equal(sort(r$areas$code), c("620", "630", "630"))
  expect_s3_class(dataset_map(r$data, r$areas), "leaflet")
  expect_s3_class(dataset_map(r$data[0, ], r$areas), "leaflet")

  readme <- render_readme(list(id = 1, title = "t", requester = "Org"),
                          list(list(spec = r$spec, stats = r$stats, sql_file = "01_x.sql")), "t.gpkg", cfg)
  expect_match(readme, "Only hauls overlapping NMFS reporting area 620, 630", fixed = TRUE)
  expect_match(readme, "Excluding hauls overlapping NMFS reporting area 630", fixed = TRUE)
  expect_match(readme, "Spatial filters select whole hauls", fixed = TRUE)
  plain <- render_readme(list(id = 1, title = "t", requester = "Org"),
                         list(list(spec = validate_spec(example_specs()[[2]], cfg), stats = r$stats,
                                   sql_file = "01_x.sql")), "t.gpkg", cfg)
  expect_no_match(plain, "Spatial filters", fixed = TRUE)
})

test_that("the editor reads spatial filter rows into the spec", {
  cfg <- cfg_default()
  inputs <- list(name = "x", grid = "hex_8km", description = "",
                 sf_3_mode = "include", sf_3_source = "NMFS_REPORTING_AREA", sf_3_codes = c("620", "610"),
                 sf_7_mode = "exclude", sf_7_source = "HEX_8KM", sf_7_codes = "12")
  spec <- validate_spec(spec_from_editor(function(n) inputs[[n]], cfg, sf_keys = c(3L, 7L)), cfg)
  expect_equal(spec$spatial_filters, list(
    list(mode = "include", source = "NMFS_REPORTING_AREA", codes = c("610", "620")),
    list(mode = "exclude", source = "HEX_8KM", codes = "12")))

  # A row with no areas chosen is an error, not silently ignored.
  inputs$sf_7_codes <- NULL
  expect_error(validate_spec(spec_from_editor(function(n) inputs[[n]], cfg, sf_keys = c(3L, 7L)), cfg),
               "spatial filter 2 \\(Hex grid cell, 8 km\\): choose at least one area")

  row <- as.character(spatial_filter_row_ui(5, list(mode = "exclude", source = "HEX_8KM"),
                                            c("NMFS_REPORTING_AREA", "HEX_8KM"), cfg))
  expect_match(row, 'id="ed_sf_5_codes"', fixed = TRUE)
  expect_match(row, '<option value="exclude" selected>', fixed = TRUE)
  expect_match(row, '<option value="HEX_8KM" selected>Hex grid cell, 8 km</option>', fixed = TRUE)
})
