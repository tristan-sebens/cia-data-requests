#' Run the CIA data request app
#'
#' @param host,port Passed to [shiny::runApp()].
#' @export
run_app <- function(host = "127.0.0.1", port = 3838) {
  log_info("Starting cia-data-requests ", app_version(), " (log level ",
           names(log_levels)[log_threshold()], "; set CIA_LOG_LEVEL=DEBUG for more detail)")
  cfg <- load_config()
  log_info("Config: grids ", paste(names(cfg$grids), collapse = ", "), "; rules ",
           paste(vapply(cfg$rules, function(r) r$name %||% r$type, ""), collapse = ", "))
  startup_checks(cfg)
  app <- shiny::shinyApp(app_ui(cfg), function(input, output, session) app_server(input, output, session, cfg))
  log_info("Open http://127.0.0.1:", port, " in a browser")
  shiny::runApp(app, host = host, port = port, launch.browser = FALSE)
}

#' Log the app's settings and check that its stores are reachable
#'
#' Only a broken request store stops the app; the database and rclone checks
#' just warn, so the app can still be used to edit requests.
#' @noRd
startup_checks <- function(cfg) {
  path <- Sys.getenv("CIA_REQUESTS_DB")
  log_info("Request store: ", path)
  store <- store_connect()
  on.exit(DBI::dbDisconnect(store))
  log_timed("Request store write check", level = "INFO", slow_ms = 500, {
    DBI::dbExecute(store, "BEGIN IMMEDIATE")
    DBI::dbExecute(store, "COMMIT")
  })
  n <- recover_interrupted_runs(store)
  if (n > 0) log_warn("Marked ", n, " interrupted run(s) as failed.")

  log_info("Output folder: ", Sys.getenv("CIA_OUTPUT_DIR"))
  remote <- Sys.getenv("CIA_RCLONE_REMOTE")
  if (nzchar(remote)) log_info("Upload root: ", remote) else log_warn("CIA_RCLONE_REMOTE is not set: uploads will fail.")
  rc <- Sys.getenv("RCLONE_CONFIG")
  if (nzchar(rc) && !grepl("^/", remote) && !file.exists(rc)) {
    log_warn("No rclone config at ", rc, ": uploads will fail. See README (RCLONE_CONFIG_DIR).")
  }

  tryCatch({
    con <- cia_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    log_timed("Postgres check query", level = "INFO", DBI::dbGetQuery(con, "SELECT 1"))
    if (!is.null(cfg$areas)) check_areas_view(con, cfg)
  }, error = function(e) {
    log_warn("Postgres is not reachable: ", trimws(conditionMessage(e)),
             ". The app will start, but previews and packaging will fail.")
  })
}

#' Check the spatial filter areas view and log what it contains
#'
#' Warns (does not fail) if it is missing or not in EPSG:3338.
#' @noRd
check_areas_view <- function(con, cfg) {
  V <- cfg$areas$view
  tbl <- paste0(V$schema, ".", V$table)
  x <- tryCatch(
    log_timed(paste0("Spatial filter areas check (", tbl, ")"), level = "INFO", slow_ms = 10000,
      DBI::dbGetQuery(con, paste0(
        "SELECT ", V$source, " AS source, count(*)::int AS n, ",
        "array_to_string(array_agg(DISTINCT ST_SRID(", V$geom, ")), ',') AS srids, ",
        "array_to_string(array_agg(DISTINCT GeometryType(", V$geom, ")), ',') AS types ",
        "FROM ", tbl, " GROUP BY 1 ORDER BY 1"))),
    error = function(e) {
      log_warn("Spatial filter areas unavailable (", tbl, "): ", trimws(conditionMessage(e)),
               ". Spatial filters cannot be used until it exists.")
      NULL
    })
  if (is.null(x)) return(invisible(FALSE))
  for (i in seq_len(nrow(x))) {
    log_info("  ", x$source[i], ": ", x$n[i], " areas (", x$types[i], ")")
    if (!identical(x$srids[i], "3338")) {
      log_warn("  ", x$source[i], " has SRID ", x$srids[i], "; spatial filters need EPSG:3338.")
    }
  }
  invisible(TRUE)
}

#' Filter values per field, cached for the life of the app process
#'
#' Loading them runs `SELECT DISTINCT` over the full tables, which can be slow
#' on the real database, so it happens once (on first use), not per session.
#' Restart the app to pick up new codes.
#' @noRd
filter_choices <- function(con, cfg) {
  if (is.null(.cia_cache$choices)) {
    .cia_cache$choices <- log_timed("Load filter values (first use since start-up)", level = "INFO",
      stats::setNames(lapply(names(cfg$fields), function(f) field_values(con, cfg, f)), names(cfg$fields)))
  }
  .cia_cache$choices
}
.cia_cache <- new.env(parent = emptyenv())

#' Spatial filter areas (source, code), cached like [filter_choices()]
#' @noRd
area_choices <- function(con, cfg) {
  if (is.null(.cia_cache$areas)) .cia_cache$areas <- area_values(con, cfg)
  .cia_cache$areas
}

#' @noRd
app_ui <- function(cfg) {
  bslib::page_sidebar(
    title = "CIA data requests",
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5, preset = "flatly"),
    window_title = "CIA data requests",
    sidebar = bslib::sidebar(
      width = 330,
      shiny::actionButton("new_request", "New request", class = "btn-primary w-100"),
      shiny::checkboxInput("show_archived", "Show archived requests"),
      shiny::uiOutput("request_list")
    ),
    shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE),
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
      .req-item { cursor: pointer; }
      .req-item.active { background: var(--bs-primary); color: #fff; }
      .req-item.active .text-muted { color: #e9ecef !important; }
      pre.sql { max-height: 420px; overflow: auto; font-size: 0.8rem; background: var(--bs-light); padding: .75rem; }
      .job-log { font-size: 0.8rem; max-height: 220px; overflow: auto; }
      /* Make multi-select filter fields look like dropdowns. */
      .cia-picker .selectize-control.multi .selectize-input { padding-right: 2.25rem; cursor: pointer;
        background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%23343a40' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\"); background-repeat: no-repeat;
        background-position: right .75rem center; background-size: 16px 12px; }
      .cia-picker .selectize-control.multi .selectize-input > input { cursor: pointer; }
    "))),
    shiny::conditionalPanel(
      "!output.has_request",
      bslib::card(bslib::card_body(
        htmltools::h4("No request selected"),
        htmltools::p("Choose a request on the left, or create a new one.")
      ))
    ),
    shiny::conditionalPanel(
      "output.has_request",
      shiny::uiOutput("request_header"),
      bslib::navset_card_tab(
        id = "tabs",
        bslib::nav_panel("Details", value = "details", details_panel()),
        bslib::nav_panel("Datasets", value = "datasets", shiny::uiOutput("datasets_panel")),
        bslib::nav_panel("Preview", value = "preview", shiny::uiOutput("preview_panel")),
        bslib::nav_panel("Package", value = "package", shiny::uiOutput("package_panel")),
        bslib::nav_panel("History", value = "history", shiny::uiOutput("history_panel"))
      )
    )
  )
}

#' @noRd
details_panel <- function() {
  root <- Sys.getenv("CIA_RCLONE_REMOTE")
  htmltools::div(
    class = "row",
    htmltools::div(
      class = "col-lg-6",
      shiny::textInput("req_title", "Title", width = "100%"),
      shiny::textInput("req_requester", "Requester (organisation)", width = "100%"),
      shiny::textInput("req_contact", "Contact", width = "100%"),
      shiny::textInput("req_drive", "Drive folder", width = "100%",
                       placeholder = "e.g. 2026-031 Org X"),
      htmltools::tags$small(class = "text-muted d-block mb-3",
        "Folder below the upload root",
        if (nzchar(root)) htmltools::tagList(" (", htmltools::tags$code(root), ")"),
        ". Create it in Drive and share it with the recipient before packaging."),
      shiny::textAreaInput("req_notes", "Notes (internal)", rows = 4, width = "100%"),
      bslib::input_task_button("req_save", "Save details", label_busy = "Saving ..."),
      shiny::actionButton("req_archive", "Archive", class = "btn-outline-secondary ms-2"),
      shiny::actionButton("req_delete", "Delete", class = "btn-outline-danger ms-2")
    )
  )
}

#' @noRd
app_server <- function(input, output, session, cfg) {
  sid <- paste0("session ", substr(session$token, 1, 6))
  with_log_context(sid, log_info("Session started (", session$request$HTTP_USER_AGENT %||% "unknown browser", ")"))
  store <- with_log_context(sid, store_connect())
  pg <- NULL  # opened on first use
  db <- function() {
    if (is.null(pg) || !DBI::dbIsValid(pg)) pg <<- cia_connect()
    pg
  }
  session$onSessionEnded(function() {
    with_log_context(sid, log_info("Session ended"))
    DBI::dbDisconnect(store)
    if (!is.null(pg)) try(DBI::dbDisconnect(pg), silent = TRUE)
  })

  state <- shiny::reactiveValues(
    request_id = NULL, tick = 0, job = NULL, job_log = character(),
    preview = NULL, editing = NULL
  )
  refresh <- function() state$tick <- state$tick + 1
  notify_error <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8)

  # Run a user action: log it, time it, and show any error instead of crashing
  # the session. `expr` is evaluated lazily inside the handler.
  guarded <- function(label, expr) with_log_context(sid, {
    t0 <- Sys.time()
    log_info(label, " ...")
    trace <- NULL
    tryCatch(
      withCallingHandlers({
        force(expr)
        log_info(label, " done in ", fmt_ms(t0))
      }, error = function(e) trace <<- short_trace()),
      shiny.silent.error = function(e) log_debug(label, ": skipped (nothing selected)"),
      error = function(e) {
        log_error(label, " failed after ", fmt_ms(t0), ": ", trimws(conditionMessage(e)))
        if (length(trace)) log_debug("Call stack:\n  ", paste(trace, collapse = "\n  "))
        notify_error(e)
      }
    )
    invisible()
  })
  # Time a read of the request store (logged at DEBUG; WARN when slow).
  timed <- function(label, expr) with_log_context(sid, log_timed(label, expr, slow_ms = 500))

  # Reactive reads of the store ------------------------------------------------
  requests <- shiny::reactive({
    state$tick
    timed("Store read: requests", list_requests(store, include_archived = isTRUE(input$show_archived)))
  })
  request <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    timed("Store read: request", get_request(store, state$request_id))
  })
  datasets <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    timed("Store read: datasets", get_datasets(store, state$request_id))
  })
  approvals <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    timed("Store read: approvals", approval_status(store, state$request_id, cfg))
  })
  runs <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    timed("Store read: runs", list_runs(store, state$request_id))
  })
  job_running <- shiny::reactive(!is.null(state$job))

  output$has_request <- shiny::reactive(!is.null(state$request_id))
  shiny::outputOptions(output, "has_request", suspendWhenHidden = FALSE)

  # Request list ---------------------------------------------------------------
  output$request_list <- shiny::renderUI({
    r <- requests()
    if (!nrow(r)) return(htmltools::p(class = "text-muted mt-3", "No requests yet."))
    htmltools::div(
      class = "list-group mt-2",
      lapply(seq_len(nrow(r)), function(i) {
        active <- identical(as.numeric(state$request_id), as.numeric(r$id[i]))
        htmltools::tags$a(
          class = paste("list-group-item list-group-item-action req-item", if (active) "active"),
          onclick = sprintf("Shiny.setInputValue('select_request', %d, {priority: 'event'})", r$id[i]),
          htmltools::div(class = "fw-semibold", paste0("#", r$id[i], " ", r$title[i])),
          htmltools::div(class = "small text-muted", r$requester[i],
                         sprintf(" · %d dataset%s", r$n_datasets[i], if (r$n_datasets[i] == 1) "" else "s"),
                         if (isTRUE(r$archived[i])) " · archived"),
          if (!is.na(r$last_run_kind[i])) htmltools::div(
            class = "small",
            paste0("Last ", r$last_run_kind[i], ": "), run_status_badge(r$last_run_status[i]),
            if (identical(r$last_run_kind[i], "package"))
              htmltools::tagList(" upload ", run_status_badge(r$last_upload_status[i]))
          )
        )
      })
    )
  })

  select_request <- function(id) {
    state$request_id <- id
    state$preview <- NULL
    r <- get_request(store, id)
    shiny::updateTextInput(session, "req_title", value = r$title)
    shiny::updateTextInput(session, "req_requester", value = r$requester)
    shiny::updateTextInput(session, "req_contact", value = r$contact)
    shiny::updateTextInput(session, "req_drive", value = r$drive_path)
    shiny::updateTextAreaInput(session, "req_notes", value = r$notes)
    shiny::updateActionButton(session, "req_archive", label = if (r$archived) "Unarchive" else "Archive")
  }
  shiny::observeEvent(input$select_request, guarded(paste("Open request", input$select_request), select_request(input$select_request)))

  # New request ----------------------------------------------------------------
  shiny::observeEvent(input$new_request, guarded("New request form", {
    shiny::showModal(shiny::modalDialog(
      title = "New request",
      shiny::textInput("new_title", "Title", width = "100%", placeholder = "e.g. Pollock catch for Org X"),
      shiny::textInput("new_requester", "Requester (organisation)", width = "100%"),
      shiny::textInput("new_drive", "Drive folder", width = "100%"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"),
                                  bslib::input_task_button("new_create", "Create", label_busy = "Creating ..."))
    ))
  }))
  shiny::observeEvent(input$new_create, guarded("Create request", {
    id <- create_request(store, input$new_title, input$new_requester,
                         drive_path = trimws(input$new_drive))
    shiny::removeModal()
    select_request(id)
    bslib::nav_select("tabs", "datasets")
    refresh()
  }))

  # Header -----------------------------------------------------------------------
  output$request_header <- shiny::renderUI({
    r <- request()
    a <- approvals()
    n_ok <- sum(a$status == "approved")
    htmltools::div(
      class = "mb-3",
      htmltools::h3(class = "mb-1", paste0("#", r$id, " ", r$title)),
      htmltools::div(
        class = "text-muted",
        r$requester,
        if (nzchar(r$drive_path)) htmltools::tagList(" · Drive: ", htmltools::tags$code(r$drive_path)),
        " · ", sprintf("%d dataset%s, %d approved", nrow(a), if (nrow(a) == 1) "" else "s", n_ok),
        if (nrow(a) && n_ok == nrow(a)) badge("ready to package", "success") else NULL,
        if (isTRUE(r$archived)) badge("archived", "secondary")
      )
    )
  })

  # Details ----------------------------------------------------------------------
  shiny::observeEvent(input$req_save, guarded(paste("Save request", state$request_id), {
    update_request(store, state$request_id, title = input$req_title, requester = input$req_requester,
                   contact = input$req_contact, drive_path = trimws(input$req_drive),
                   notes = input$req_notes)
    if (nzchar(trimws(input$req_drive))) check_drive_folder(input$req_drive)
    shiny::showNotification("Request saved.", type = "message")
    refresh()
  }))
  shiny::observeEvent(input$req_archive, guarded(paste("Archive/unarchive request", state$request_id), {
    archived <- !isTRUE(request()$archived)
    update_request(store, state$request_id, archived = archived)
    shiny::updateActionButton(session, "req_archive", label = if (archived) "Unarchive" else "Archive")
    refresh()
  }))
  shiny::observeEvent(input$req_delete, {
    shiny::showModal(shiny::modalDialog(
      title = "Delete request?",
      "This permanently deletes the request, its datasets and its preview history.",
      "Requests that have been packaged cannot be deleted; archive them instead.",
      footer = htmltools::tagList(shiny::modalButton("Cancel"),
                                  shiny::actionButton("req_delete_confirm", "Delete", class = "btn-danger"))
    ))
  })
  shiny::observeEvent(input$req_delete_confirm, guarded(paste("Delete request", state$request_id), {
    shiny::removeModal()
    delete_request(store, state$request_id)
    state$request_id <- NULL
    refresh()
  }))

  datasets_server(input, output, session, cfg, store, db, state, datasets, approvals, refresh, guarded)
  runs_server(input, output, session, cfg, store, state, datasets, approvals, runs, refresh, guarded,
              job_running)
}
