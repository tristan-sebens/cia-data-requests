test_that("editor inputs become a valid spec", {
  cfg <- cfg_default()
  inputs <- list(name = " pollock_by_year ", description = "", grid = "hex_20km",
                 group_by = c("year", "species_code"),
                 f_species_code = "POLL", f_gear_code = NULL,
                 f_year_min = 2019, f_year_max = NA)
  spec <- spec_from_editor(function(n) inputs[[n]], cfg)
  expect_equal(spec$name, "pollock_by_year")
  expect_equal(spec$filters[sort(names(spec$filters))],
               list(species_code = "POLL", year = list(min = 2019, max = NULL)))
  expect_identical(spec_hash(spec, cfg),
                   spec_hash(dataset_spec("pollock_by_year", "hex_20km",
                                          filters = list(species_code = "POLL", year = list(min = 2019)),
                                          group_by = c("year", "species_code")), cfg))

  empty <- spec_from_editor(function(n) list(name = "x", grid = "hex_8km")[[n]], cfg)
  expect_length(empty$filters, 0)
  expect_length(empty$group_by, 0)
})

test_that("the editor modal round-trips a spec's values", {
  cfg <- cfg_default()
  spec <- validate_spec(example_specs()[[3]], cfg)
  choices <- lapply(cfg$fields, function(f) c("AKFL", "FLAT", "TRW", "X"))
  choices$year <- c(min = 2018L, max = 2024L)
  html <- as.character(dataset_editor_modal(spec, cfg, choices, "Edit"))
  expect_match(html, 'value="trawl_flatfish_detail"', fixed = TRUE)
  expect_match(html, '<option value="TRW" selected>', fixed = TRUE)
  expect_match(html, '<input id="ed_f_year_min" type="number"[^>]*value="2019"')
})

test_that("stats rendering states whether withheld totals go in the README", {
  s <- fake_stats(FALSE)
  html <- as.character(htmltools::tagList(stats_table(s), stats_notes(s)))
  expect_match(html, "will NOT report withheld catch totals", fixed = TRUE)
  expect_no_match(html, "Off-grid", fixed = TRUE)  # none in these stats
  s2 <- fake_stats(TRUE)
  expect_match(as.character(stats_notes(s2)), "will report the withheld catch totals", fixed = TRUE)
})

test_that("maps render for empty and non-empty data", {
  con <- test_con()
  r <- run_dataset(con, example_specs()[[2]], cfg_default())
  expect_s3_class(dataset_map(r$data), "leaflet")
  expect_s3_class(dataset_map(r$data[0, ]), "leaflet")
})

test_that("background jobs run a preview in a separate process", {
  con <- test_con()
  path <- withr::local_tempfile(fileext = ".sqlite")
  withr::local_envvar(CIA_REQUESTS_DB = path, CIA_DEV_PKG = testthat::test_path("..", ".."))
  store <- store_connect(path)
  on.exit(DBI::dbDisconnect(store), add = TRUE)
  cfg <- cfg_default()
  req <- create_request(store, "Job test", "Org")
  add_dataset(store, req, example_specs()[[2]], cfg)

  job <- start_job("preview", req)
  job$proc$wait(120000)
  p <- poll_job(job)
  expect_true(p$done)
  expect_null(p$error)
  expect_equal(p$result$status, "succeeded")
  expect_true(any(grepl("Running dataset 1 of 1: poll_by_year", p$log)))
  expect_equal(get_run(store, p$result$run_id)$status, "succeeded")
})

test_that("loading the installed package loads sf (needed to handle results from background jobs)", {
  skip_if_not(nzchar(system.file(package = "ciarequests")), "package not installed")
  skip_if(packageVersion("ciarequests") != pkgload::pkg_version(testthat::test_path("..", "..")),
          "installed package is not the version under test")
  expect_true(callr::r(function() {
    loadNamespace("ciarequests")
    "sf" %in% loadedNamespaces()
  }))
})
