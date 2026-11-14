# Pure helpers for the Shiny app: building specs from editor inputs and
# rendering results. Kept free of reactivity so they can be unit tested.

#' Dataset editor inputs -> dataset spec
#'
#' @param get Function returning the value of an editor input by short name
#'   (e.g. `get("name")`, `get("f_species_code")`).
#' @noRd
spec_from_editor <- function(get, cfg, sf_keys = integer()) {
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
  spatial <- lapply(sf_keys, function(k) {
    list(mode = get(paste0("sf_", k, "_mode")) %||% "include",
         source = get(paste0("sf_", k, "_source")) %||% "",
         codes = as.character(get(paste0("sf_", k, "_codes")) %||% character()))
  })
  dataset_spec(
    name        = trimws(get("name") %||% ""),
    grid        = get("grid") %||% "",
    filters     = filters,
    group_by    = as.character(get("group_by") %||% character()),
    description = trimws(get("description") %||% ""),
    spatial_filters = spatial
  )
}

#' One spatial filter row in the dataset editor
#'
#' The area picker starts empty; the server fills it (server-side selectize,
#' since a source such as a fine hex grid can have tens of thousands of areas).
#' @param k Row key, unique within the session.
#' @param s The filter (`mode`, `source`, `codes`), or `NULL` for a new row.
#' @param sources Area sources, in display order.
#' @noRd
spatial_filter_row_ui <- function(k, s, sources, cfg, ns = identity) {
  id <- function(x) ns(paste0("ed_sf_", k, "_", x))
  source_choices <- stats::setNames(sources, vapply(sources, area_source_label, "", cfg = cfg))
  htmltools::div(
    id = ns(paste0("ed_sf_row_", k)), class = "border rounded p-2 pb-0 mb-2",
    htmltools::div(
      class = "d-flex gap-2 align-items-start",
      htmltools::div(style = "flex: 1 1 45%;",
        shiny::selectInput(id("mode"), NULL, width = "100%", selected = s$mode %||% "include",
                           choices = c("Only hauls overlapping" = "include",
                                       "Excluding hauls overlapping" = "exclude"))),
      htmltools::div(style = "flex: 1 1 45%;",
        shiny::selectInput(id("source"), NULL, source_choices, width = "100%",
                           selected = s$source %||% sources[1])),
      htmltools::tags$button(
        type = "button", class = "btn btn-sm btn-outline-danger mt-1", title = "Remove this spatial filter",
        onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("ed_sf_remove"), k),
        "Remove")
    ),
    spatial_codes_ui(k, ns)
  )
}

#' The area picker of a spatial filter row (empty; filled by the server)
#'
#' Replaced, not updated, when the row's source changes: selectize re-applies
#' its plugins on every update, which would duplicate the remove buttons.
#' @noRd
spatial_codes_ui <- function(k, ns = identity) {
  htmltools::div(
    id = ns(paste0("ed_sf_", k, "_codes_wrap")), class = "cia-picker",
    shiny::selectizeInput(ns(paste0("ed_sf_", k, "_codes")), NULL, choices = NULL, multiple = TRUE,
                          width = "100%", options = list(placeholder = "Loading areas ..."))
  )
}

#' Dataset editor modal
#'
#' @param spec Spec to edit (or a blank one).
#' @param choices Named list of available values per set field, plus
#'   `c(min, max)` for range fields.
#' @param sf_rows Spatial filter section content: rows from
#'   `spatial_filter_row_ui()`, or a message if areas are unavailable.
#' @param sf_available Whether spatial filters can be added.
#' @noRd
dataset_editor_modal <- function(spec, cfg, choices, title, ns = identity, sf_rows = NULL,
                                 sf_available = !is.null(cfg$areas)) {
  grid_choices <- stats::setNames(names(cfg$grids),
                                  vapply(names(cfg$grids), function(g) cfg$grids[[g]]$label %||% g, ""))
  field_inputs <- lapply(names(cfg$fields), function(f) {
    def <- cfg$fields[[f]]
    if (def$kind == "set") {
      n <- length(choices[[f]])
      htmltools::div(
        class = "cia-picker",
        shiny::selectizeInput(
          ns(paste0("ed_f_", f)), f, choices = choices[[f]],
          selected = spec$filters[[f]], multiple = TRUE, width = "100%",
          options = list(
            placeholder = sprintf("any (click to choose from %s value%s, or type to search)",
                                  fmt_n(n), if (n == 1) "" else "s"),
            plugins = list("remove_button"),
            maxOptions = max(n, 1),
            closeAfterSelect = FALSE
          )
        )
      )
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
        htmltools::div(class = "cia-picker", shiny::selectizeInput(
          ns("ed_group_by"), "Group by (in addition to the grid cell)",
          choices = names(cfg$fields), selected = spec$group_by, multiple = TRUE, width = "100%",
          options = list(placeholder = "grid cell only (click to add fields)",
                         plugins = list("remove_button")))),
        htmltools::tags$small(class = "text-muted",
                              "Columns appear in the order selected.")
      ),
      htmltools::div(
        htmltools::tags$h6("Filters"),
        htmltools::tags$small(class = "text-muted d-block mb-2",
                              "Leave a field empty for no restriction. Values within a field are combined with OR, fields with AND."),
        field_inputs,
        if (!is.null(cfg$areas)) htmltools::tagList(
          htmltools::tags$h6(class = "mt-3", "Spatial filters"),
          htmltools::tags$small(class = "text-muted d-block mb-2",
            "Keep or drop whole hauls whose footprint overlaps any of the chosen areas ",
            "(touching a boundary does not count). Several spatial filters are combined with AND."),
          htmltools::div(id = ns("ed_sf_rows"), sf_rows),
          if (sf_available) shiny::actionButton(ns("ed_sf_add"), "Add spatial filter",
                                                class = "btn-sm btn-outline-primary mb-2")
        )
      )
    ),
    shiny::uiOutput(ns("ed_error")),
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      bslib::input_task_button(ns("ed_save"), "Save dataset", label_busy = "Saving ...")
    )
  )
}

#' Popup showing a dataset's SQL, generated from its current definition
#'
#' Both queries a preview runs: the deliver query (exported, written to the
#' .sql file) and the diagnostic query (suppression summary). Each has a copy
#' button, e.g. for running EXPLAIN in the database.
#' @param sqls Named list with `deliver` and `diagnostic`.
#' @noRd
sql_modal <- function(name, sqls) {
  block <- function(key, sql) {
    id <- paste0("sql_text_", key)
    htmltools::tagList(
      htmltools::div(
        class = "d-flex justify-content-end my-2",
        htmltools::tags$button(
          type = "button", class = "btn btn-sm btn-outline-primary",
          onclick = sprintf(paste0(
            "var b = this, t = document.getElementById('%s').textContent;",
            "navigator.clipboard.writeText(t).then(function () {",
            " b.textContent = 'Copied'; setTimeout(function () { b.textContent = 'Copy'; }, 1500); },",
            " function () { var r = document.createRange();",
            " r.selectNodeContents(document.getElementById('%s'));",
            " var s = window.getSelection(); s.removeAllRanges(); s.addRange(r);",
            " b.textContent = 'Press Ctrl+C'; });"), id, id),
          "Copy")
      ),
      htmltools::tags$pre(id = id, class = "sql", style = "max-height: 60vh;", sql)
    )
  }
  shiny::modalDialog(
    title = htmltools::tagList("SQL for ", htmltools::tags$code(name)),
    size = "xl", easyClose = TRUE,
    htmltools::p(class = "small text-muted",
      "Generated from the dataset's current definition; nothing has been run. ",
      "A preview runs both queries."),
    bslib::navset_tab(
      bslib::nav_panel("Deliver query", block("deliver", sqls$deliver)),
      bslib::nav_panel("Diagnostic query", block("diagnostic", sqls$diagnostic))
    ),
    footer = shiny::modalButton("Close")
  )
}

#' Suppression statistics as a small HTML table
#'
#' Measures as columns, delivered / withheld / total / % as rows, so it fits
#' a narrow column beside the map.
#' @noRd
stats_table <- function(s) {
  row <- function(label, records, weight, count, cls = NULL) {
    htmltools::tags$tr(class = cls, htmltools::tags$th(class = "text-nowrap", label),
                       lapply(list(records, weight, count), htmltools::tags$td, class = "text-end"))
  }
  htmltools::tags$table(
    class = "table table-sm small mb-2",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th(""), htmltools::tags$th(class = "text-end", "Records"),
      htmltools::tags$th(class = "text-end", "Weight (t)"), htmltools::tags$th(class = "text-end", "Count"))),
    htmltools::tags$tbody(
      row("Delivered", fmt_n(s$rows_kept), fmt_n(s$weight_kept, 1), fmt_n(s$count_kept)),
      row("Withheld", fmt_n(s$rows_dropped), fmt_n(s$weight_dropped, 1), fmt_n(s$count_dropped)),
      row("Total", fmt_n(s$rows_total), fmt_n(s$weight_total, 1), fmt_n(s$count_total)),
      row("% withheld", fmt_pct(s$pct_rows_dropped), fmt_pct(s$pct_weight_dropped),
          fmt_pct(s$pct_count_dropped), cls = "text-muted")
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
#'
#' @param areas Optional outlines of the dataset's spatial filter areas (from
#'   `area_outlines()`): included areas in blue, excluded in red, dashed.
#' @noRd
dataset_map <- function(data, areas = NULL) {
  has_areas <- !is.null(areas) && nrow(areas) > 0
  if (has_areas) areas <- sf::st_shift_longitude(areas)
  m <- leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = FALSE)) |>
    leaflet::addTiles()

  if (nrow(data)) {
    # Aggregate over the grouping fields so each cell is drawn once.
    cells <- data[!duplicated(data$grid_id), "grid_id"]
    sum_by <- function(x) as.numeric(tapply(x, data$grid_id, sum, na.rm = TRUE)[as.character(cells$grid_id)])
    cells$catch_weight_mt <- sum_by(data$catch_weight_mt)
    cells$catch_count <- sum_by(data$catch_count)
    cells <- sf::st_shift_longitude(sf::st_transform(cells, 4326))  # keep the Aleutians together
    shade <- map_shading(cells)
    fill <- "#3b528b"
    if (!is.null(shade)) {
      pal <- leaflet::colorNumeric("viridis", domain = sqrt(shade$values), na.color = "#bbbbbb")
      fill <- pal(sqrt(shade$values))
      m <- leaflet::addLegend(m, "bottomright", pal = pal, values = sqrt(shade$values),
                              title = shade$title, opacity = 0.8, na.label = "none",
                              labFormat = leaflet::labelFormat(transform = function(x) round(x^2)))
    }
    m <- leaflet::addPolygons(
      m, data = cells, weight = 0.5, color = "#333", fillOpacity = 0.7, fillColor = fill,
      label = sprintf("Cell %s: %s t, %s fish (all groups)", cells$grid_id,
                      fmt_n(cells$catch_weight_mt, 1), fmt_n(cells$catch_count, 0))
    )
  }

  # Spatial filter outlines on top of the cells.
  if (has_areas) {
    modes <- c("include", "exclude")
    present <- modes %in% areas$mode
    m <- leaflet::addPolygons(
      m, data = areas, fill = FALSE, weight = 2, dashArray = "6 4", opacity = 0.9,
      color = ifelse(areas$mode == "include", "#1f6feb", "#d1242f"),
      label = paste0(ifelse(areas$mode == "include", "Included: ", "Excluded: "), areas$source, " ", areas$code)
    )
    m <- leaflet::addLegend(m, "topright", colors = c("#1f6feb", "#d1242f")[present],
                            labels = c("Included area", "Excluded area")[present], opacity = 0.9)
  }

  extent <- if (nrow(data)) cells else if (has_areas) areas
  if (is.null(extent)) return(leaflet::setView(m, lng = -160, lat = 58, zoom = 3))
  bb <- sf::st_bbox(extent)
  leaflet::fitBounds(m, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}

#' What to shade the map cells by
#'
#' Catch weight where any cell has a positive weight, otherwise catch count
#' (count-only catch has no weight), otherwise nothing (`NULL`: plain fill).
#' Non-positive values are left unshaded, so they cannot break the scale.
#' @noRd
map_shading <- function(cells) {
  for (m in list(list(col = "catch_weight_mt", title = "Catch (t)"),
                 list(col = "catch_count", title = "Catch (count)"))) {
    v <- cells[[m$col]]
    v[!is.finite(v) | v <= 0] <- NA
    if (any(!is.na(v))) return(list(values = v, title = m$title))
  }
  NULL
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
