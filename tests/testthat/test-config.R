test_that("the shipped configuration loads", {
  cfg <- cfg_default()
  expect_s3_class(cfg, "cia_config")
  expect_setequal(names(cfg$grids), c("hex_8km", "hex_20km"))
  expect_true("species_code" %in% names(cfg$fields))
  expect_length(cfg$rules, 1)
})

test_that("schema defaults to CIA_PG_SCHEMA", {
  withr::local_envvar(CIA_PG_SCHEMA = "other_schema")
  expect_equal(load_config()$schema, "other_schema")
})

# Copy the shipped config to a temp dir, apply `edit` to one file, and load it.
load_edited <- function(file, edit) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  file.copy(list.files(system.file("config", package = "ciarequests"), full.names = TRUE), dir)
  x <- yaml::read_yaml(file.path(dir, file))
  yaml::write_yaml(edit(x), file.path(dir, file))
  load_config(dir, schema = "data")
}

test_that("identifiers that are not plain SQL names are rejected", {
  expect_error(load_edited("grids.yaml", function(x) { x$hex_8km$table <- "hex; DROP TABLE x"; x }),
               "lowercase SQL identifier")
  expect_error(load_edited("schema.yaml", function(x) { x$hauls$vessel <- "Vessel_ID"; x }),
               "lowercase SQL identifier")
  expect_error(load_config(schema = "data; --"), "lowercase SQL identifier")
})

test_that("confidentiality rules are required and validated", {
  expect_error(load_edited("rules.yaml", function(x) list()), "at least one confidentiality rule")
  expect_error(load_edited("rules.yaml", function(x) { x[[1]]$min <- 0; x }), "positive whole number")
  expect_error(load_edited("rules.yaml", function(x) { x[[1]]$type <- "magic"; x }), "unsupported type")
})

test_that("grids must be EPSG:3338", {
  expect_error(load_edited("grids.yaml", function(x) { x$hex_8km$srid <- 4326; x }), "3338")
})
