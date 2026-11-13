# Packaging and upload. Uploads use rclone with a local folder as the
# "remote", so no cloud access is needed.

test_that("Drive folder names cannot escape the upload root", {
  expect_equal(check_drive_folder(" 2026-031 Org X "), "2026-031 Org X")
  expect_equal(check_drive_folder("Org X/2026-031"), "Org X/2026-031")
  for (bad in c("", "gdrive:other", "/abs", "a/../b", "..", "a//b", "a\\b", "a/./b", "a\nb")) {
    expect_error(check_drive_folder(bad), info = bad)
  }
  expect_equal(join_remote("gdrive:", "x"), "gdrive:x")
  expect_equal(join_remote("gdrive:Data Requests", "x"), "gdrive:Data Requests/x")
  expect_error(rclone_uploader(remote_root = ""), "CIA_RCLONE_REMOTE")
})

test_that("file_slug makes safe file names", {
  expect_equal(file_slug("Pollock & Cod: 2018–2024 (Org X)"), "pollock_cod_2018_2024_org_x")
  expect_equal(file_slug("!!!", fallback = "request_3"), "request_3")
})

test_that("the README only reports suppressed catch when that is itself non-confidential", {
  cfg <- cfg_default()
  req <- list(id = 7, title = "Pollock request", requester = "Org X")
  spec <- validate_spec(example_specs()[[3]], cfg)
  entry <- function(stats) list(list(spec = spec, stats = stats, sql_file = "01_x.sql"))

  open <- render_readme(req, entry(fake_stats(TRUE)), "pollock_request.gpkg", cfg)
  expect_match(open, "Records withheld: 4 of 100 (4.0%)", fixed = TRUE)
  expect_match(open, "Catch weight withheld: 10.0 t of 1,000.0 t (1.0%)", fixed = TRUE)

  closed <- render_readme(req, entry(fake_stats(FALSE)), "pollock_request.gpkg", cfg)
  expect_match(closed, "Records withheld: 4 of 100", fixed = TRUE)
  expect_match(closed, "Catch withheld: not reported", fixed = TRUE)
  expect_no_match(closed, "10.0 t", fixed = TRUE)
  expect_no_match(closed, "1.0%", fixed = TRUE)

  none <- render_readme(req, entry(fake_stats(NA, dropped = 0)), "x.gpkg", cfg)
  expect_match(none, "No records were withheld", fixed = TRUE)

  # Off-grid catch is internal only.
  expect_no_match(open, "off-grid", ignore.case = TRUE)
  expect_match(open, "at least 3 distinct vessels", fixed = TRUE)
  expect_match(open, "`species_group_code` is AKFL, FLAT", fixed = TRUE)
  expect_match(open, "`year` from 2019 to 2023", fixed = TRUE)
})

test_that("a store at schema version 1 is migrated", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  s <- DBI::dbConnect(RSQLite::SQLite(), path)
  for (stmt in store_migrations[[1]]) DBI::dbExecute(s, stmt)
  DBI::dbExecute(s, "PRAGMA user_version = 1")
  DBI::dbExecute(s, "INSERT INTO requests (title, requester, created_at, updated_at)
                     VALUES ('old', 'Org', 'x', 'x')")
  DBI::dbDisconnect(s)

  store <- store_connect(path)
  on.exit(DBI::dbDisconnect(store))
  expect_equal(DBI::dbGetQuery(store, "PRAGMA user_version")[[1]], length(store_migrations))
  expect_true("upload_dest" %in% DBI::dbListFields(store, "runs"))
  expect_equal(list_requests(store)$title, "old")
})

# --- End to end -----------------------------------------------------------

local_pipeline <- function(env = parent.frame()) {
  con <- test_con(env)
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  store <- store_connect(path)
  withr::defer(DBI::dbDisconnect(store), envir = env)
  root <- withr::local_tempdir(.local_envir = env)
  out <- withr::local_tempdir(.local_envir = env)
  list(con = con, store = store, cfg = cfg_default(), out = out, root = root,
       uploader = rclone_uploader(remote_root = root, args = c("--config", "/dev/null")))
}

approved_request <- function(p, specs, folder = "org_x_2026") {
  req <- create_request(p$store, "Pollock & Cod (Org X)", "Org X", drive_path = folder)
  for (s in specs) add_dataset(p$store, req, s, p$cfg)
  pv <- preview_request(p$store, p$con, req, p$cfg)
  stopifnot(pv$status == "succeeded")
  approve_run(p$store, pv$run_id, p$cfg)
  req
}

test_that("an approved request is packaged and uploaded", {
  p <- local_pipeline()
  specs <- example_specs()[c(2, 3, 4)]
  req <- approved_request(p, specs)

  stages <- character()
  res <- package_request(p$store, p$con, req, p$cfg, uploader = p$uploader, output_root = p$out,
                         progress = function(stage, detail) stages <<- c(stages, stage))
  expect_equal(res$status, "succeeded")
  expect_equal(res$upload_status, "succeeded")
  expect_true(all(c("query", "write", "upload") %in% stages))

  # Local package
  expect_equal(res$output_dir, file.path(p$out, paste0("request_", req), paste0("run_", res$run_id)))
  files <- sort(list.files(res$output_dir))
  sql_files <- sprintf("%02d_%s.sql", 1:3, vapply(specs, `[[`, "", "name"))
  expect_equal(files, sort(c("README.md", "pollock_cod_org_x.gpkg", sql_files)))
  expect_length(list.files(dirname(res$output_dir), pattern = "partial"), 0)

  gpkg <- file.path(res$output_dir, "pollock_cod_org_x.gpkg")
  layers <- sf::st_layers(gpkg)
  expect_setequal(layers$name, vapply(specs, `[[`, "", "name"))
  rd <- get_run_datasets(p$store, res$run_id)
  for (i in seq_along(specs)) {
    lyr <- sf::st_read(gpkg, layer = specs[[i]]$name, quiet = TRUE)
    expect_equal(nrow(lyr), rd$rows_kept[i])
    expect_equal(sf::st_crs(lyr)$epsg, 3338L)
    expect_equal(sum(lyr$catch_weight_mt), rd$weight_kept[i], tolerance = 1e-9)
    expect_equal(paste(readLines(file.path(res$output_dir, sql_files[i])), collapse = "\n"),
                 sub("\n$", "", rd$sql_text[i]))
  }

  # Uploaded copy
  dest <- file.path(p$root, "org_x_2026")
  expect_equal(sort(list.files(dest)), files)
  run <- get_run(p$store, res$run_id)
  expect_equal(run$upload_dest, dest)
  expect_false(is.na(run$uploaded_at))
  expect_equal(run$gpkg_path, gpkg)
})

test_that("re-packaging overwrites, removes this app's outdated files, and leaves others alone", {
  p <- local_pipeline()
  specs <- example_specs()[c(2, 4)]
  req <- approved_request(p, specs)
  first <- package_request(p$store, p$con, req, p$cfg, uploader = p$uploader, output_root = p$out)
  dest <- file.path(p$root, "org_x_2026")
  writeLines("recipient's own notes", file.path(dest, "notes.txt"))

  # Drop the second dataset, re-preview, re-approve, re-package.
  d <- get_datasets(p$store, req)
  delete_dataset(p$store, d$id[2])
  approve_run(p$store, preview_request(p$store, p$con, req, p$cfg)$run_id, p$cfg)
  second <- package_request(p$store, p$con, req, p$cfg, uploader = p$uploader, output_root = p$out)

  expect_equal(second$upload_status, "succeeded")
  expect_setequal(list.files(dest), c(list.files(second$output_dir), "notes.txt"))
  expect_false(file.exists(file.path(dest, "02_pcod_targets_since_2021.sql")))
  expect_true(dir.exists(first$output_dir))  # local history kept
})

test_that("unapproved or edited requests cannot be packaged", {
  p <- local_pipeline()
  req <- create_request(p$store, "t", "Org", drive_path = "f")
  add_dataset(p$store, req, example_specs()[[2]], p$cfg)
  expect_error(package_request(p$store, p$con, req, p$cfg, uploader = NULL, output_root = p$out),
               "not ready to package")
  expect_equal(nrow(list_runs(p$store, req)[list_runs(p$store, req)$kind == "package", ]), 0)
})

test_that("a missing or invalid Drive folder stops packaging before anything runs", {
  p <- local_pipeline()
  req <- approved_request(p, example_specs()[2], folder = "")
  expect_error(package_request(p$store, p$con, req, p$cfg, uploader = p$uploader, output_root = p$out),
               "no Drive folder")
  update_request(p$store, req, drive_path = "../elsewhere")
  expect_error(package_request(p$store, p$con, req, p$cfg, uploader = p$uploader, output_root = p$out),
               "Invalid Drive folder")
  expect_equal(sum(list_runs(p$store, req)$kind == "package"), 0)
})

test_that("a failed upload is recorded and can be retried", {
  p <- local_pipeline()
  req <- approved_request(p, example_specs()[2])
  broken <- rclone_uploader(remote_root = p$root, rclone = "/nonexistent/rclone")
  res <- package_request(p$store, p$con, req, p$cfg, uploader = broken, output_root = p$out)
  expect_equal(res$status, "succeeded")
  expect_equal(res$upload_status, "failed")
  run <- get_run(p$store, res$run_id)
  expect_equal(run$upload_status, "failed")
  expect_match(run$upload_error, "Upload to .* failed")

  expect_equal(upload_run(p$store, res$run_id, p$uploader), "succeeded")
  expect_true(file.exists(file.path(p$root, "org_x_2026", "README.md")))
})

test_that("packaging without an uploader skips the upload", {
  p <- local_pipeline()
  req <- approved_request(p, example_specs()[2])
  res <- package_request(p$store, p$con, req, p$cfg, uploader = NULL, output_root = p$out)
  expect_equal(res$upload_status, "skipped")
  expect_equal(get_run(p$store, res$run_id)$upload_status, "skipped")
  expect_true(file.exists(file.path(res$output_dir, "README.md")))
})

test_that("a failure while building leaves no partial package", {
  p <- local_pipeline()
  req <- approved_request(p, example_specs()[c(2, 3)])
  dead <- cia_connect()
  DBI::dbDisconnect(dead)
  res <- package_request(p$store, dead, req, p$cfg, uploader = p$uploader, output_root = p$out)
  expect_equal(res$status, "failed")
  run <- get_run(p$store, res$run_id)
  expect_equal(run$status, "failed")
  expect_false(is.na(run$error))
  expect_length(list.files(file.path(p$out, paste0("request_", req))), 0)
  expect_false(dir.exists(file.path(p$root, "org_x_2026")))
})

test_that("a dataset with no delivered rows still gets an (empty) layer", {
  p <- local_pipeline()
  # Every cell of this one-vessel slice is suppressed.
  one_vessel <- DBI::dbGetQuery(p$con, "SELECT min(gear_code) AS g, min(management_program_code) AS m
                                         FROM data.hauls WHERE vessel_id = 40")
  spec <- dataset_spec("empty_slice", "hex_8km",
                       filters = list(gear_code = one_vessel$g, management_program_code = one_vessel$m,
                                      year = list(min = 2024)),
                       group_by = c("year", "species_code"))
  req <- create_request(p$store, "Empty", "Org", drive_path = "empty")
  add_dataset(p$store, req, spec, p$cfg)
  pv <- preview_request(p$store, p$con, req, p$cfg)
  skip_if(nrow(pv$results[[1]]$data) > 0, "slice not empty in this synthetic data")
  approve_run(p$store, pv$run_id, p$cfg)
  res <- package_request(p$store, p$con, req, p$cfg, uploader = NULL, output_root = p$out)
  expect_equal(res$status, "succeeded")
  lyr <- sf::st_layers(file.path(res$output_dir, "empty.gpkg"))
  expect_equal(lyr$features, 0)
})
