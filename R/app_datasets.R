# Datasets tab: list, add, edit, duplicate, reorder, delete.

#' @noRd
datasets_server <- function(input, output, session, cfg, store, db, state, datasets, approvals,
                            refresh, guarded) {

  output$datasets_panel <- shiny::renderUI({
    d <- datasets()
    a <- approvals()
    act <- function(action, id, label, cls = "btn-outline-secondary", disabled = FALSE) {
      htmltools::tags$button(
        class = paste("btn btn-sm", cls), disabled = if (disabled) NA,
        onclick = sprintf(
          "Shiny.setInputValue('ds_action', {action: '%s', id: %d, nonce: Math.random()}, {priority: 'event'})",
          action, id),
        label)
    }
    rows <- lapply(seq_len(nrow(d)), function(i) {
      spec <- d$spec[[i]]
      grid <- cfg$grids[[spec$grid]]$label %||% spec$grid
      filters <- if (length(spec$filters)) {
        paste(vapply(names(spec$filters), function(f) describe_filter(f, spec$filters[[f]]), ""),
              collapse = "; ")
      } else "none"
      htmltools::tags$tr(
        htmltools::tags$td(i),
        htmltools::tags$td(htmltools::tags$code(spec$name),
                           if (nzchar(spec$description)) htmltools::div(class = "small text-muted", spec$description)),
        htmltools::tags$td(grid),
        htmltools::tags$td(class = "small", gsub("`", "", filters)),
        htmltools::tags$td(class = "small", paste(c("grid cell", spec$group_by), collapse = ", ")),
        htmltools::tags$td(approval_badge(a$status[a$id == d$id[i]])),
        htmltools::tags$td(
          class = "text-nowrap",
          act("edit", d$id[i], "Edit"),
          act("duplicate", d$id[i], "Duplicate"),
          act("up", d$id[i], "↑", disabled = i == 1),
          act("down", d$id[i], "↓", disabled = i == nrow(d)),
          act("delete", d$id[i], "Delete", "btn-outline-danger")
        )
      )
    })
    htmltools::tagList(
      htmltools::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        htmltools::p(class = "text-muted mb-0",
                     "Each dataset becomes one layer in the GeoPackage. Editing a dataset withdraws its approval."),
        bslib::input_task_button("ds_add", "Add dataset", label_busy = "Loading filter values ...")
      ),
      if (!nrow(d)) htmltools::p(class = "mt-3", "No datasets yet.") else
        htmltools::tags$table(
          class = "table table-hover align-middle",
          htmltools::tags$thead(htmltools::tags$tr(lapply(
            c("#", "Name", "Grid", "Filters", "Grouping", "Status", ""), htmltools::tags$th))),
          htmltools::tags$tbody(rows)
        )
    )
  })

  # Values for the filter pickers: loaded from the database on first use, then
  # cached for the life of the app process (see filter_choices()).
  open_editor <- function(spec, dataset_id = NULL, title) {
    choices <- tryCatch(filter_choices(db(), cfg), error = function(e) {
      stop("Could not load filter values from the database: ", conditionMessage(e), call. = FALSE)
    })
    state$editing <- list(dataset_id = dataset_id)
    shiny::showModal(dataset_editor_modal(spec, cfg, choices, title))
  }

  shiny::observeEvent(input$ds_add, guarded("Open dataset editor (new)", {
    n <- nrow(datasets())
    open_editor(dataset_spec(paste0("dataset_", n + 1), names(cfg$grids)[1]), NULL, "Add dataset")
  }))

  shiny::observeEvent(input$ds_action, guarded(
    paste0("Dataset action '", input$ds_action$action, "' on dataset ", input$ds_action$id), {
    a <- input$ds_action
    id <- a$id
    d <- datasets()
    if (a$action == "edit") {
      open_editor(get_dataset(store, id)$spec, id, "Edit dataset")
    } else if (a$action == "duplicate") {
      spec <- get_dataset(store, id)$spec
      base <- substr(paste0(spec$name, "_copy"), 1, 55)
      new_name <- base
      k <- 2
      while (new_name %in% d$name) { new_name <- paste0(base, "_", k); k <- k + 1 }
      duplicate_dataset(store, id, new_name, cfg)
      refresh()
    } else if (a$action %in% c("up", "down")) {
      ids <- d$id
      i <- match(id, ids)
      j <- if (a$action == "up") i - 1 else i + 1
      if (j >= 1 && j <= length(ids)) {
        ids[c(i, j)] <- ids[c(j, i)]
        reorder_datasets(store, state$request_id, ids)
        refresh()
      }
    } else if (a$action == "delete") {
      state$editing <- list(delete_id = id)
      shiny::showModal(shiny::modalDialog(
        title = "Delete dataset?",
        paste0("Delete dataset '", get_dataset(store, id)$name, "'?"),
        footer = htmltools::tagList(shiny::modalButton("Cancel"),
                                    shiny::actionButton("ds_delete_confirm", "Delete", class = "btn-danger"))
      ))
    }
  }))

  shiny::observeEvent(input$ds_delete_confirm, guarded("Delete dataset", {
    shiny::removeModal()
    delete_dataset(store, state$editing$delete_id)
    state$editing <- NULL
    refresh()
  }))

  editor_error <- shiny::reactiveVal(NULL)
  output$ed_error <- shiny::renderUI({
    msg <- editor_error()
    if (!is.null(msg)) htmltools::div(class = "alert alert-danger mt-3 mb-0", msg)
  })

  shiny::observeEvent(input$ed_save, guarded("Save dataset", {
    id <- state$editing$dataset_id
    res <- tryCatch({
      spec <- spec_from_editor(function(n) input[[paste0("ed_", n)]], cfg)
      log_debug("Dataset spec from editor: ", spec_to_json(spec))
      if (is.null(id)) {
        add_dataset(store, state$request_id, spec, cfg)
        "Dataset added."
      } else if (update_dataset(store, id, spec, cfg)) {
        "Dataset saved. Preview and approve it again before packaging."
      } else {
        "No changes."
      }
    }, error = function(e) e)
    if (inherits(res, "error")) {
      # Usually invalid input; shown in the editor, which stays open.
      log_warn("Save dataset rejected: ", conditionMessage(res))
      editor_error(conditionMessage(res))
      return()
    }
    log_info(res, if (is.null(id)) "" else paste0(" (dataset ", id, ")"))
    editor_error(NULL)
    state$editing <- NULL
    shiny::removeModal()
    shiny::showNotification(res, type = "message")
    refresh()
  }))
}
