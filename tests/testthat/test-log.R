test_that("log lines respect the level and carry the context", {
  withr::local_envvar(CIA_LOG_LEVEL = "INFO")
  out <- capture.output(type = "message", {
    log_debug("hidden")
    log_info("shown")
    with_log_context("session abc", log_warn("tagged"))
  })
  expect_length(out, 2)
  expect_match(out[1], "INFO  shown$")
  expect_match(out[2], "WARN  \\[session abc\\] tagged$")
  expect_null(getOption("cia.log_context"))

  withr::local_envvar(CIA_LOG_LEVEL = "OFF")
  expect_length(capture.output(type = "message", log_error("x")), 0)
})

test_that("log_timed returns the value, flags slow calls and logs failures", {
  withr::local_envvar(CIA_LOG_LEVEL = "INFO")
  out <- capture.output(type = "message", v <- log_timed("quick", level = "INFO", 42))
  expect_equal(v, 42)
  expect_match(out, "INFO  quick took \\d+ ms$")

  out <- capture.output(type = "message", log_timed("slow", slow_ms = 0, Sys.sleep(0.01)))
  expect_match(out, "WARN  slow took .* \\(slow\\)$")

  out <- capture.output(type = "message",
                        expect_error(log_timed("broken", stop("boom")), "boom"))
  expect_match(out, "ERROR broken failed after .*: boom$")
})

test_that("filter values are loaded once per app process", {
  skip_if_no_test_db()
  con <- cia_connect()
  withr::local_envvar(CIA_LOG_LEVEL = "OFF")
  rm(list = ls(.cia_cache), envir = .cia_cache)
  withr::defer(rm(list = ls(.cia_cache), envir = .cia_cache))
  cfg <- cfg_default()
  first <- filter_choices(con, cfg)
  expect_setequal(names(first), names(cfg$fields))
  expect_true("POLL" %in% first$species_code)
  DBI::dbDisconnect(con)  # a second call must not touch the database
  expect_identical(filter_choices(con, cfg), first)
})
