# Preview, Package and History tabs, and background job handling.

#' @noRd
runs_server <- function(input, output, session, cfg, store, state, datasets, approvals, runs,
                        refresh, guarded, job_running) {

  # Background job polling ----------------------------------------------------
  start <- function(kind, id) {
    if (job_running()) {
      shiny::showNotification("Another job is still running.", type = "warning")
      return()
    }
    state$job <- start_job(kind, id)
    state$job_log <- character()
  }

  shiny::observe({
    job <- state$job
    shiny::req(job)
    shiny::invalidateLater(1000)
    p <- poll_job(job)
    state$job_log <- p$log
    if (!p$done) return()
    state$job <- NULL
    if (!is.null(p$error)) {
      shiny::showNotification(paste(tools::toTitleCase(job$kind), "failed:", p$error),
                              type = "error", duration = NULL)
    } else if (job$kind == "preview") {
      if (identical(as.numeric(job$id), as.numeric(state$request_id))) state$preview <- p$result
      shiny::showNotification(paste("Preview", p$result$status), duration = 5,
                              type = if (p$result$status == "succeeded") "message" else "warning")
    } else if (job$kind == "package") {
      msg <- paste0("Package ", p$result$status,
                    if (p$result$status == "succeeded") paste0("; upload ", p$result$upload_status),
                    if (!is.null(p$result$error)) paste0(": ", p$result$error))
      ok <- p$result$status == "succeeded" && p$result$upload_status %in% c("succeeded", "skipped")
      shiny::showNotification(msg, type = if (ok) "message" else "error", duration = if (ok) 5 else NULL)
    } else if (job$kind == "upload") {
      shiny::showNotification(paste("Upload", p$result$status),
                              type = if (p$result$status == "succeeded") "message" else "error")
    }
    refresh()
  })

  job_box <- function(kinds) {
    job <- state$job
    log <- state$job_log
    if (is.null(job) || !job$kind %in% kinds) return(NULL)
    kind <- job$kind
    bslib::card(
      class = "border-info",
      bslib::card_header(htmltools::span(class = "spinner-border spinner-border-sm me-2"),
                         tools::toTitleCase(kind), " running ..."),
      htmltools::tags$pre(class = "job-log mb-0", paste(log, collapse = "\n"))
    )
  }

  output$job_box_preview <- shiny::renderUI(job_box("preview"))
  output$job_box_package <- shiny::renderUI(job_box(c("package", "upload")))

  # Preview -----------------------------------------------------------------------
  output$preview_panel <- shiny::renderUI({
    d <- datasets()
    pv <- state$preview
    htmltools::tagList(
      htmltools::div(
        class = "d-flex gap-2 align-items-center mb-3",
        shiny::actionButton("preview_run", "Run preview", class = "btn-primary",
                            disabled = job_running() || !nrow(d)),
        if (!is.null(pv) && pv$status == "succeeded")
          shiny::actionButton("preview_approve", "Approve these results", class = "btn-success",
                              disabled = job_running()),
        htmltools::span(class = "text-muted small",
          if (!nrow(d)) "Add a dataset first."
          else "Runs every dataset against the database. Review the results, then approve them to enable packaging.")
      ),
      shiny::uiOutput("job_box_preview"),
      if (!is.null(pv)) preview_results_ui(pv, cfg)
    )
  })

  shiny::observeEvent(input$preview_run, start("preview", state$request_id))

  shiny::observeEvent(input$preview_approve, guarded({
    approve_run(store, state$preview$run_id, cfg)
    shiny::showNotification("Approved. The request can now be packaged.", type = "message")
    refresh()
  }))

  # Render map and table outputs for the current preview.
  shiny::observe({
    pv <- state$preview
    shiny::req(pv)
    for (i in seq_along(pv$results)) {
      local({
        r <- pv$results[[i]]
        k <- i
        if (inherits(r, "cia_dataset_result")) {
          output[[paste0("pv_map_", k)]] <- leaflet::renderLeaflet(dataset_map(r$data))
          output[[paste0("pv_table_", k)]] <- shiny::renderTable({
            utils::head(sf::st_drop_geometry(r$data), 500)
          }, digits = 3, striped = TRUE, spacing = "xs")
        }
      })
    }
  })

  # Package -----------------------------------------------------------------------
  output$package_panel <- shiny::renderUI({
    a <- approvals()
    r <- runs()
    req_row <- get_request(store, state$request_id)
    ready <- nrow(a) > 0 && all(a$status == "approved")
    root <- Sys.getenv("CIA_RCLONE_REMOTE")
    dest <- tryCatch(join_remote(root, check_drive_folder(req_row$drive_path)), error = function(e) NULL)
    last <- r[r$kind == "package", ][1, ]
    has_last <- nrow(r[r$kind == "package", ]) > 0

    htmltools::tagList(
      htmltools::tags$table(
        class = "table table-sm w-auto",
        htmltools::tags$tbody(lapply(seq_len(nrow(a)), function(i) {
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$code(a$name[i])),
                             htmltools::tags$td(approval_badge(a$status[i])))
        }))
      ),
      htmltools::p(
        "Destination: ",
        if (!is.null(dest)) htmltools::tags$code(dest)
        else htmltools::span(class = "text-danger", "set a valid Drive folder on the Details tab"),
        if (!nzchar(root)) htmltools::span(class = "text-danger", " (CIA_RCLONE_REMOTE is not set)")
      ),
      if (!ready) htmltools::p(class = "text-muted",
        "Every dataset must be previewed and approved with its current definition before packaging."),
      shiny::actionButton("package_run", "Package and upload", class = "btn-primary", width = "auto",
                          disabled = !ready || is.null(dest) || !nzchar(root) || job_running()),
      shiny::uiOutput("job_box_package"),
      if (has_last) bslib::card(
        class = "mt-3",
        bslib::card_header(paste0("Last package run #", last$id, " "), run_status_badge(last$status),
                           " upload ", run_status_badge(last$upload_status)),
        bslib::card_body(
          htmltools::p("Started ", fmt_time(last$started_at),
                       if (!is.na(last$uploaded_at)) paste0(" · uploaded ", fmt_time(last$uploaded_at))),
          if (!is.na(last$error)) htmltools::div(class = "alert alert-danger", last$error),
          if (!is.na(last$upload_error)) htmltools::div(class = "alert alert-danger", last$upload_error),
          if (!is.na(last$output_dir)) htmltools::p("Local copy: ", htmltools::tags$code(last$output_dir)),
          if (!is.na(last$upload_dest)) htmltools::p("Uploaded to: ", htmltools::tags$code(last$upload_dest)),
          if (identical(last$upload_status, "failed"))
            shiny::actionButton("upload_retry", "Retry upload", class = "btn-warning",
                                disabled = job_running())
        )
      )
    )
  })

  shiny::observeEvent(input$package_run, start("package", state$request_id))
  shiny::observeEvent(input$upload_retry, {
    r <- runs()
    last <- r[r$kind == "package", ][1, ]
    if (identical(last$upload_status, "failed")) start("upload", last$id)
  })

  # History -----------------------------------------------------------------------
  output$history_panel <- shiny::renderUI({
    r <- runs()
    if (!nrow(r)) return(htmltools::p(class = "text-muted", "No runs yet."))
    do.call(bslib::accordion, c(list(open = FALSE), lapply(seq_len(nrow(r)), function(i) {
        run <- r[i, ]
        rd <- get_run_datasets(store, run$id)
        bslib::accordion_panel(
          title = htmltools::span(
            paste0("#", run$id, " ", run$kind, " · ", fmt_time(run$started_at), " "),
            run_status_badge(run$status),
            if (run$kind == "package") htmltools::tagList(" upload ", run_status_badge(run$upload_status))
          ),
          value = paste0("run_", run$id),
          if (!is.na(run$error)) htmltools::div(class = "alert alert-danger", run$error),
          htmltools::p(class = "small text-muted",
                       "App version ", run$app_version,
                       if (!is.na(run$finished_at)) paste0(" · finished ", fmt_time(run$finished_at)),
                       if (!is.na(run$output_dir)) htmltools::tagList(" · ", htmltools::tags$code(run$output_dir))),
          lapply(seq_len(nrow(rd)), function(j) history_dataset_ui(rd[j, ])),
          if (!is.na(run$upload_log) && nzchar(run$upload_log)) htmltools::tags$details(
            htmltools::tags$summary("Upload log"),
            htmltools::tags$pre(class = "sql", run$upload_log))
        )
      })))
  })
}

#' @noRd
preview_results_ui <- function(pv, cfg) {
  panels <- lapply(seq_along(pv$results), function(i) {
    r <- pv$results[[i]]
    if (inherits(r, "cia_dataset_error")) {
      return(bslib::card(
        class = "border-danger mb-3",
        bslib::card_header(htmltools::tags$code(r$spec$name), " ", badge("failed", "danger")),
        bslib::card_body(htmltools::div(class = "alert alert-danger mb-0", r$error))
      ))
    }
    n <- nrow(r$data)
    bslib::card(
      class = "mb-3", fill = FALSE,
      bslib::card_header(htmltools::tags$code(r$spec$name), " ",
                         badge(sprintf("%s records", fmt_n(n)), "primary"),
                         htmltools::span(class = "small text-muted", sprintf(" ran in %.1f s", r$duration_s))),
      bslib::card_body(
        fillable = FALSE,
        htmltools::div(
          class = "row g-3",
          htmltools::div(class = "col-xl-6", stats_table(r$stats), stats_notes(r$stats)),
          htmltools::div(class = "col-xl-6", leaflet::leafletOutput(paste0("pv_map_", i), height = 340))
        ),
        htmltools::tags$details(
          class = "mt-3",
          htmltools::tags$summary(if (n > 500) sprintf("Data (first 500 of %s rows)", fmt_n(n)) else "Data"),
          htmltools::div(style = "max-height: 360px; overflow: auto;",
                         shiny::tableOutput(paste0("pv_table_", i)))
        ),
        htmltools::tags$details(
          htmltools::tags$summary("SQL"),
          htmltools::tags$pre(class = "sql", r$sql)
        )
      )
    )
  })
  htmltools::tagList(
    htmltools::h5(class = "mt-2", paste0("Preview run #", pv$run_id, " "), run_status_badge(pv$status)),
    panels
  )
}

#' @noRd
history_dataset_ui <- function(rd) {
  stats <- if (!is.na(rd$rows_kept)) {
    s <- suppression_stats(data.frame(
      rows_kept = rd$rows_kept, rows_dropped = rd$rows_dropped,
      weight_kept = rd$weight_kept, weight_dropped = rd$weight_dropped,
      count_kept = rd$count_kept, count_dropped = rd$count_dropped,
      weight_source = rd$weight_kept + rd$weight_dropped + rd$weight_off_grid,
      count_source = rd$count_kept + rd$count_dropped + rd$count_off_grid,
      dropped_disclosable = as.logical(rd$dropped_disclosable)))
    htmltools::tagList(stats_table(s), stats_notes(s))
  }
  htmltools::div(
    class = "border rounded p-2 mb-2",
    htmltools::div(htmltools::tags$code(rd$dataset_name),
                   if (!is.na(rd$duration_s)) htmltools::span(class = "small text-muted", sprintf(" %.1f s", rd$duration_s))),
    if (!is.na(rd$error)) htmltools::div(class = "alert alert-danger my-2", rd$error),
    stats,
    htmltools::tags$details(htmltools::tags$summary("SQL"), htmltools::tags$pre(class = "sql", rd$sql_text))
  )
}
