# Pure helpers for the Shiny app: building specs from editor inputs and
# rendering results. Kept free of reactivity so they can be unit tested.

#' Dataset editor inputs -> dataset spec
#'
#' @param get Function returning the value of an editor input by short name
#'   (e.g. `get("name")`, `get("f_species_code")`).
#' @noRd
spec_from_editor <- function(get, cfg) {
  filters <- list()
  for (f in names(cfg$fields)) {
    if (cfg$fields[[f]]$kind == "set") {
      v <- get(paste0("f_", f))
      if (length(v)) filters[[f]] <- as.character(v)
    } else {
      lo <- get(paste0("f_", f, "_min"))
      hi <- get(paste0("f_", f, "_max"))
      lo <- if (is.null(lo) || is.na(lo)) NULL else lo
      hi <- if (is.null(hi) || is.na(hi)) NULL else hi
      if (!is.null(lo) || !is.null(hi)) filters[[f]] <- list(min = lo, max = hi)
    }
  }
  dataset_spec(
    name        = trimws(get("name") %||% ""),
    grid        = get("grid") %||% "",
    filters     = filters,
    group_by    = as.character(get("group_by") %||% character()),
    description = trimws(get("description") %||% "")
  )
}

#' Dataset editor modal
#'
#' @param spec Spec to edit (or a blank one).
#' @param choices Named list of available values per set field, plus
#'   `c(min, max)` for range fields.
#' @noRd
dataset_editor_modal <- function(spec, cfg, choices, title, ns = identity) {
  grid_choices <- stats::setNames(names(cfg$grids),
                                  vapply(names(cfg$grids), function(g) cfg$grids[[g]]$label %||% g, ""))
  field_inputs <- lapply(names(cfg$fields), function(f) {
    def <- cfg$fields[[f]]
    if (def$kind == "set") {
      shiny::selectizeInput(ns(paste0("ed_f_", f)), f, choices = choices[[f]],
                            selected = spec$filters[[f]], multiple = TRUE, width = "100%",
                            options = list(placeholder = "any"))
    } else {
      r <- choices[[f]]
      htmltools::div(
        class = "d-flex gap-2 align-items-end",
        shiny::numericInput(ns(paste0("ed_f_", f, "_min")), paste(f, "from"),
                            value = spec$filters[[f]]$min %||% NA, min = r[["min"]], max = r[["max"]],
                            width = "50%"),
        shiny::numericInput(ns(paste0("ed_f_", f, "_max")), "to",
                            value = spec$filters[[f]]$max %||% NA, min = r[["min"]], max = r[["max"]],
                            width = "50%")
      )
    }
  })

  shiny::modalDialog(
    title = title, size = "xl", easyClose = FALSE,
    bslib::layout_columns(
      col_widths = c(5, 7),
      htmltools::div(
        shiny::textInput(ns("ed_name"), "Name (GeoPackage layer)", spec$name, width = "100%",
                         placeholder = "e.g. pollock_by_year"),
        htmltools::tags$small(class = "text-muted",
                              "Lowercase letters, digits and underscores, starting with a letter."),
        shiny::textAreaInput(ns("ed_description"), "Description (shown in the README)",
                             spec$description, rows = 2, width = "100%"),
        shiny::selectInput(ns("ed_grid"), "Grid", grid_choices, selected = spec$grid, width = "100%"),
        shiny::selectizeInput(ns("ed_group_by"), "Group by (in addition to the grid cell)",
                              choices = names(cfg$fields), selected = spec$group_by,
                              multiple = TRUE, width = "100%",
                              options = list(placeholder = "grid cell only")),
        htmltools::tags$small(class = "text-muted",
                              "Columns appear in the order selected.")
      ),
      htmltools::div(
        htmltools::tags$h6("Filters"),
        htmltools::tags$small(class = "text-muted d-block mb-2",
                              "Leave a field empty for no restriction. Values within a field are combined with OR, fields with AND."),
        field_inputs
      )
    ),
    shiny::uiOutput(ns("ed_error")),
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("ed_save"), "Save dataset", class = "btn-primary")
    )
  )
}

#' Suppression statistics as a small HTML table
#' @noRd
stats_table <- function(s) {
  row <- function(label, kept, dropped, total, pct) {
    htmltools::tags$tr(htmltools::tags$th(class = "text-nowrap", label), htmltools::tags$td(kept),
                       htmltools::tags$td(dropped), htmltools::tags$td(total),
                       htmltools::tags$td(pct))
  }
  htmltools::tags$table(
    class = "table table-sm mb-2",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th(""), htmltools::tags$th("Delivered"), htmltools::tags$th("Withheld"),
      htmltools::tags$th("Total"), htmltools::tags$th("% withheld"))),
    htmltools::tags$tbody(
      row("Records", fmt_n(s$rows_kept), fmt_n(s$rows_dropped), fmt_n(s$rows_total), fmt_pct(s$pct_rows_dropped)),
      row("Catch weight (t)", fmt_n(s$weight_kept, 1), fmt_n(s$weight_dropped, 1), fmt_n(s$weight_total, 1),
          fmt_pct(s$pct_weight_dropped)),
      row("Catch count", fmt_n(s$count_kept), fmt_n(s$count_dropped), fmt_n(s$count_total),
          fmt_pct(s$pct_count_dropped))
    )
  )
}

#' Notes under the stats table: off-grid catch and README disclosure
#' @noRd
stats_notes <- function(s) {
  htmltools::tags$ul(
    class = "small text-muted mb-0",
    if (s$weight_off_grid > 0) htmltools::tags$li(sprintf(
      "Off-grid (hauls with no overlap with the grid; not delivered, not in the README): %s t.",
      fmt_n(s$weight_off_grid, 1))),
    htmltools::tags$li(if (s$rows_dropped == 0) {
      "Nothing withheld."
    } else if (isTRUE(s$dropped_disclosable)) {
      "The README will report the withheld catch totals (the withheld records together meet the confidentiality rules)."
    } else {
      "The README will NOT report withheld catch totals: the withheld records together represent too few vessels."
    })
  )
}

#' Leaflet map of a dataset's delivered cells, shaded by catch weight
#' @noRd
dataset_map <- function(data) {
  m <- leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = FALSE)) |>
    leaflet::addTiles()
  if (!nrow(data)) {
    return(leaflet::setView(m, lng = -160, lat = 58, zoom = 3))
  }
  # Aggregate over the grouping fields so each cell is drawn once.
  totals <- tapply(data$catch_weight_mt, data$grid_id, sum)
  cells <- data[!duplicated(data$grid_id), "grid_id"]
  cells$catch_weight_mt <- as.numeric(totals[as.character(cells$grid_id)])
  cells <- sf::st_shift_longitude(sf::st_transform(cells, 4326))  # keep the Aleutians together
  pal <- leaflet::colorNumeric("viridis", domain = sqrt(cells$catch_weight_mt))
  bb <- sf::st_bbox(cells)
  m <- leaflet::fitBounds(m, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  m <- leaflet::addLegend(m, "bottomright", pal = pal, values = sqrt(cells$catch_weight_mt),
                          title = "Catch (t)", opacity = 0.8,
                          labFormat = leaflet::labelFormat(transform = function(x) round(x^2)))
  leaflet::addPolygons(
    m, data = cells, weight = 0.5, color = "#333", fillOpacity = 0.7,
    fillColor = pal(sqrt(cells$catch_weight_mt)),
    label = sprintf("Cell %s: %s t (all groups)", cells$grid_id, fmt_n(cells$catch_weight_mt, 1))
  )
}

#' Bootstrap badge
#' @noRd
badge <- function(text, kind = "secondary") {
  htmltools::span(class = paste0("badge text-bg-", kind, " me-1"), text)
}

#' @noRd
approval_badge <- function(status) {
  switch(status,
    approved = badge("approved", "success"),
    stale = badge("approval out of date", "warning"),
    badge("not approved", "secondary")
  )
}

#' @noRd
run_status_badge <- function(status) {
  if (is.null(status) || is.na(status)) return(NULL)
  switch(status,
    succeeded = badge("succeeded", "success"),
    failed = badge("failed", "danger"),
    running = badge("running", "info"),
    pending = badge("pending", "secondary"),
    skipped = badge("skipped", "secondary"),
    badge(status)
  )
}

#' Local time for display
#' @noRd
fmt_time <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  t <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  format(t, "%Y-%m-%d %H:%M", tz = Sys.getenv("TZ", "UTC"))
}
