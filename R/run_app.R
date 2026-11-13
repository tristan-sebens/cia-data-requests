#' Run the CIA data request app
#'
#' @param host,port Passed to [shiny::runApp()].
#' @export
run_app <- function(host = "127.0.0.1", port = 3838) {
  cfg <- load_config()
  store <- store_connect()
  n <- recover_interrupted_runs(store)
  if (n > 0) message("Marked ", n, " interrupted run(s) as failed.")
  DBI::dbDisconnect(store)
  app <- shiny::shinyApp(app_ui(cfg), function(input, output, session) app_server(input, output, session, cfg))
  shiny::runApp(app, host = host, port = port, launch.browser = FALSE)
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
    htmltools::tags$head(htmltools::tags$style(htmltools::HTML("
      .req-item { cursor: pointer; }
      .req-item.active { background: var(--bs-primary); color: #fff; }
      .req-item.active .text-muted { color: #e9ecef !important; }
      pre.sql { max-height: 420px; overflow: auto; font-size: 0.8rem; background: var(--bs-light); padding: .75rem; }
      .job-log { font-size: 0.8rem; max-height: 220px; overflow: auto; }
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
      shiny::actionButton("req_save", "Save details", class = "btn-primary"),
      shiny::actionButton("req_archive", "Archive", class = "btn-outline-secondary ms-2"),
      shiny::actionButton("req_delete", "Delete", class = "btn-outline-danger ms-2")
    )
  )
}

#' @noRd
app_server <- function(input, output, session, cfg) {
  store <- store_connect()
  db <- local({
    con <- NULL
    function() {
      if (is.null(con) || !DBI::dbIsValid(con)) con <<- cia_connect()
      con
    }
  })
  session$onSessionEnded(function() {
    DBI::dbDisconnect(store)
    try(DBI::dbDisconnect(db()), silent = TRUE)
  })

  state <- shiny::reactiveValues(
    request_id = NULL, tick = 0, job = NULL, job_log = character(),
    preview = NULL, editing = NULL, choices = NULL
  )
  refresh <- function() state$tick <- state$tick + 1
  notify_error <- function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
  guarded <- function(expr) tryCatch(expr, error = notify_error)

  # Reactive reads of the store ------------------------------------------------
  requests <- shiny::reactive({
    state$tick
    list_requests(store, include_archived = isTRUE(input$show_archived))
  })
  request <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    get_request(store, state$request_id)
  })
  datasets <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    get_datasets(store, state$request_id)
  })
  approvals <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    approval_status(store, state$request_id, cfg)
  })
  runs <- shiny::reactive({
    state$tick
    shiny::req(state$request_id)
    list_runs(store, state$request_id)
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
  shiny::observeEvent(input$select_request, guarded(select_request(input$select_request)))

  # New request ----------------------------------------------------------------
  shiny::observeEvent(input$new_request, {
    shiny::showModal(shiny::modalDialog(
      title = "New request",
      shiny::textInput("new_title", "Title", width = "100%", placeholder = "e.g. Pollock catch for Org X"),
      shiny::textInput("new_requester", "Requester (organisation)", width = "100%"),
      shiny::textInput("new_drive", "Drive folder", width = "100%"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"),
                                  shiny::actionButton("new_create", "Create", class = "btn-primary"))
    ))
  })
  shiny::observeEvent(input$new_create, guarded({
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
  shiny::observeEvent(input$req_save, guarded({
    update_request(store, state$request_id, title = input$req_title, requester = input$req_requester,
                   contact = input$req_contact, drive_path = trimws(input$req_drive),
                   notes = input$req_notes)
    if (nzchar(trimws(input$req_drive))) check_drive_folder(input$req_drive)
    shiny::showNotification("Request saved.", type = "message")
    refresh()
  }))
  shiny::observeEvent(input$req_archive, guarded({
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
  shiny::observeEvent(input$req_delete_confirm, guarded({
    shiny::removeModal()
    delete_request(store, state$request_id)
    state$request_id <- NULL
    refresh()
  }))

  datasets_server(input, output, session, cfg, store, db, state, datasets, approvals, refresh, guarded)
  runs_server(input, output, session, cfg, store, state, datasets, approvals, runs, refresh, guarded,
              job_running)
}
