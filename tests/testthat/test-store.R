# Request store tests. All use a throwaway SQLite file.

local_store <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  store <- store_connect(path)
  withr::defer(DBI::dbDisconnect(store), envir = env)
  store
}

new_request <- function(store) create_request(store, "Pollock request", "Org X", contact = "a@b.org")

test_that("the store is created with the current schema and safe settings", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  store <- store_connect(path)
  expect_setequal(DBI::dbListTables(store), c("requests", "datasets", "runs", "run_datasets"))
  expect_equal(DBI::dbGetQuery(store, "PRAGMA user_version")[[1]], length(store_migrations))
  expect_equal(DBI::dbGetQuery(store, "PRAGMA journal_mode")[[1]], "delete")
  expect_equal(DBI::dbGetQuery(store, "PRAGMA foreign_keys")[[1]], 1)
  id <- new_request(store)
  DBI::dbDisconnect(store)

  # Reopening keeps the data and does not re-run migrations.
  store <- store_connect(path)
  on.exit(DBI::dbDisconnect(store))
  expect_equal(get_request(store, id)$title, "Pollock request")
})

test_that("a store from a newer app version is refused", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  s <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(s, "PRAGMA user_version = 999")
  DBI::dbDisconnect(s)
  expect_error(store_connect(path), "newer than this app")
})

test_that("store_connect needs a path", {
  withr::local_envvar(CIA_REQUESTS_DB = "")
  expect_error(store_connect(), "CIA_REQUESTS_DB")
})

test_that("requests can be created, updated, listed, archived", {
  store <- local_store()
  a <- new_request(store)
  b <- create_request(store, "Cod request", "Org Y")
  expect_error(create_request(store, "", "Org"), "'title' is required")

  update_request(store, a, drive_path = "gdrive:Requests/org-x", notes = "line 1\nline 2")
  r <- get_request(store, a)
  expect_equal(r$drive_path, "gdrive:Requests/org-x")
  expect_false(r$archived)
  expect_error(update_request(store, a, id = 5), "Cannot update")
  expect_error(get_request(store, 999), "No request")

  expect_equal(list_requests(store)$id, c(b, a))  # newest first
  update_request(store, b, archived = TRUE)
  expect_equal(list_requests(store)$id, a)
  expect_equal(nrow(list_requests(store, include_archived = TRUE)), 2)
})

test_that("datasets are validated, ordered, duplicated and deleted", {
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  specs <- example_specs()
  ids <- vapply(specs[1:3], function(s) add_dataset(store, req, s, cfg), numeric(1))

  d <- get_datasets(store, req)
  expect_equal(d$name, vapply(specs[1:3], `[[`, "", "name"))
  expect_equal(d$position, 1:3)
  # Specs come back intact.
  expect_identical(spec_hash(d$spec[[3]], cfg), spec_hash(specs[[3]], cfg))

  expect_error(add_dataset(store, req, dataset_spec("Bad Name", "hex_8km"), cfg), "Invalid dataset name")
  expect_error(add_dataset(store, req, specs[[1]], cfg), "already has a dataset named")
  expect_equal(nrow(get_datasets(store, req)), 3)  # failed inserts left nothing behind

  dup <- duplicate_dataset(store, ids[2], "poll_by_year_copy", cfg)
  expect_equal(get_dataset(store, dup)$spec$filters, get_dataset(store, ids[2])$spec$filters)

  reorder_datasets(store, req, c(dup, ids[3], ids[1], ids[2]))
  expect_equal(get_datasets(store, req)$id, c(dup, ids[3], ids[1], ids[2]))
  expect_error(reorder_datasets(store, req, c(dup, ids[1])), "exactly once")

  delete_dataset(store, ids[3])
  d <- get_datasets(store, req)
  expect_equal(d$id, c(dup, ids[1], ids[2]))
  expect_equal(d$position, 1:3)
})

test_that("runs record per-dataset results and interrupted runs are recovered", {
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  ds <- add_dataset(store, req, example_specs()[[2]], cfg)
  spec <- get_dataset(store, ds)$spec

  run <- start_run(store, req, "preview")
  expect_equal(get_run(store, run)$status, "running")
  record_run_dataset(store, run, ds, 1, spec = spec, sql = build_sql(spec, cfg), error = "boom")
  finish_run(store, run, "failed", error = "Failed dataset(s): poll_by_year")
  rd <- get_run_datasets(store, run)
  expect_equal(rd$error, "boom")
  expect_true(is.na(rd$rows_kept))

  stuck <- start_run(store, req, "package")
  expect_equal(get_run(store, stuck)$upload_status, "pending")
  expect_equal(recover_interrupted_runs(store), 1)
  expect_equal(get_run(store, stuck)$status, "failed")
  expect_match(get_run(store, stuck)$error, "Interrupted")
  expect_equal(list_runs(store, req)$id, c(stuck, run))
  expect_equal(list_requests(store)$last_run_kind, "package")
})

test_that("packaged requests cannot be deleted, others can", {
  store <- local_store()
  a <- new_request(store)
  b <- new_request(store)
  start_run(store, a, "package")
  expect_error(delete_request(store, a), "archive it instead")
  start_run(store, b, "preview")
  delete_request(store, b)
  expect_equal(list_requests(store)$id, a)
})

# Simulates a successful preview without needing Postgres.
fake_preview <- function(store, req, cfg) {
  run <- start_run(store, req, "preview")
  d <- get_datasets(store, req)
  for (i in seq_len(nrow(d))) {
    record_run_dataset(store, run, d$id[i], i, spec = d$spec[[i]], sql = build_sql(d$spec[[i]], cfg))
  }
  finish_run(store, run, "succeeded")
  run
}

test_that("approval is tied to the previewed SQL", {
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  a <- add_dataset(store, req, example_specs()[[2]], cfg)
  b <- add_dataset(store, req, example_specs()[[4]], cfg)
  expect_false(ready_to_package(store, req, cfg))
  expect_equal(approval_status(store, req, cfg)$status, c("unapproved", "unapproved"))

  run <- fake_preview(store, req, cfg)
  approve_run(store, run, cfg)
  expect_true(ready_to_package(store, req, cfg))
  expect_equal(get_dataset(store, a)$approved_run_id, run)

  # Saving an equivalent spec (values reordered) keeps the approval.
  s <- get_dataset(store, b)$spec
  s$filters <- rev(s$filters)
  expect_false(update_dataset(store, b, s, cfg))
  expect_true(ready_to_package(store, req, cfg))

  # A real edit withdraws it, and the old run can no longer be approved.
  s$group_by <- c(s$group_by, "year")
  expect_true(update_dataset(store, b, s, cfg))
  expect_equal(approval_status(store, req, cfg)$status, c("approved", "unapproved"))
  expect_false(ready_to_package(store, req, cfg))
  expect_error(approve_run(store, run, cfg), "has changed since run")

  # A configuration change makes existing approvals stale.
  approve_run(store, fake_preview(store, req, cfg), cfg)
  expect_true(ready_to_package(store, req, cfg))
  cfg2 <- cfg
  cfg2$apportionment$min_share <- 0.1
  expect_equal(approval_status(store, req, cfg2)$status, c("stale", "stale"))
  expect_false(ready_to_package(store, req, cfg2))

  # Adding or deleting a dataset also blocks approving an older run.
  run3 <- fake_preview(store, req, cfg)
  add_dataset(store, req, example_specs()[[1]], cfg)
  expect_error(approve_run(store, run3, cfg), "datasets have changed")
  expect_false(ready_to_package(store, req, cfg))
})

test_that("failed or package runs cannot be approved", {
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  add_dataset(store, req, example_specs()[[2]], cfg)
  failed <- start_run(store, req, "preview")
  finish_run(store, failed, "failed", error = "x")
  expect_error(approve_run(store, failed, cfg), "did not succeed")
  pkg <- start_run(store, req, "package")
  finish_run(store, pkg, "succeeded")
  expect_error(approve_run(store, pkg, cfg), "Only preview runs")
})

test_that("preview_request runs and records every dataset", {
  con <- test_con()
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  for (s in example_specs()[2:3]) add_dataset(store, req, s, cfg)

  seen <- character()
  p <- preview_request(store, con, req, cfg, progress = function(i, n, name) seen <<- c(seen, name))
  expect_equal(p$status, "succeeded")
  expect_equal(seen, c("poll_by_year", "trawl_flatfish_detail"))
  expect_equal(get_run(store, p$run_id)$status, "succeeded")

  rd <- get_run_datasets(store, p$run_id)
  expect_equal(rd$dataset_name, seen)
  expect_equal(rd$rows_kept, vapply(p$results, function(r) nrow(r$data), integer(1)), ignore_attr = TRUE)
  expect_equal(rd$sql_text[1], p$results[[1]]$sql)
  expect_true(all(is.na(rd$error)))

  approve_run(store, p$run_id, cfg)
  expect_true(ready_to_package(store, req, cfg))
})

test_that("preview_request fails a dataset with unknown filter values but runs the rest", {
  con <- test_con()
  store <- local_store()
  cfg <- cfg_default()
  req <- new_request(store)
  add_dataset(store, req, dataset_spec("typo", "hex_20km",
                                       filters = list(species_code = c("POLL", "POLX"))), cfg)
  add_dataset(store, req, example_specs()[[2]], cfg)

  p <- preview_request(store, con, req, cfg)
  expect_equal(p$status, "failed")
  expect_s3_class(p$results$typo, "cia_dataset_error")
  expect_match(p$results$typo$error, "species_code = POLX")
  expect_s3_class(p$results$poll_by_year, "cia_dataset_result")

  run <- get_run(store, p$run_id)
  expect_equal(run$status, "failed")
  expect_match(run$error, "typo")
  expect_error(approve_run(store, p$run_id, cfg), "did not succeed")
})

test_that("preview_request records a query error against the dataset", {
  con <- test_con()
  store <- local_store()
  cfg <- cfg_default()
  cfg$grids$hex_8km$table <- "no_such_grid"
  req <- new_request(store)
  add_dataset(store, req, dataset_spec("broken", "hex_8km"), cfg)
  p <- preview_request(store, con, req, cfg)
  expect_equal(p$status, "failed")
  expect_match(get_run_datasets(store, p$run_id)$error, "no_such_grid")
})
