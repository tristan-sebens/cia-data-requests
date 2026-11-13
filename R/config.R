#' Load and validate the app configuration
#'
#' Reads `schema.yaml`, `grids.yaml`, `rules.yaml` and (optional) `areas.yaml`. Every identifier in the
#' configuration is checked to be a plain lowercase SQL name, so it can be
#' written into SQL unquoted (keeps generated SQL readable) without risk.
#'
#' @param dir Directory holding the YAML files. Defaults to the package's
#'   installed `config` directory.
#' @param schema Schema holding `hauls` / `haul_catch`. Defaults to the
#'   `CIA_PG_SCHEMA` environment variable, or `"data"`.
#' @return A `cia_config` list with elements `schema`, `hauls`, `haul_catch`,
#'   `fields`, `grids`, `rules`, `areas` (`NULL` without `areas.yaml`).
#' @export
load_config <- function(dir = system.file("config", package = "ciarequests"),
                        schema = cia_schema()) {
  read <- function(f) yaml::read_yaml(file.path(dir, f))
  cfg <- list(
    schema     = schema,
    hauls      = read("schema.yaml")$hauls,
    haul_catch = read("schema.yaml")$haul_catch,
    fields     = read("schema.yaml")$fields,
    apportionment = read("schema.yaml")$apportionment %||% list(min_share = 0),
    grids      = read("grids.yaml"),
    rules      = read("rules.yaml"),
    areas      = if (file.exists(file.path(dir, "areas.yaml"))) read("areas.yaml")
  )
  class(cfg) <- "cia_config"
  validate_config(cfg)
}

#' @noRd
validate_config <- function(cfg) {
  check_ident(cfg$schema, "schema")

  for (part in c("hauls", "haul_catch")) {
    for (k in names(cfg[[part]])) check_ident(cfg[[part]][[k]], paste0(part, ".", k))
  }
  required <- list(hauls = c("table", "key", "vessel", "geom"),
                   haul_catch = c("table", "key", "weight", "count"))
  for (part in names(required)) {
    missing <- setdiff(required[[part]], names(cfg[[part]]))
    if (length(missing)) cfg_error(part, " is missing: ", paste(missing, collapse = ", "))
  }

  if (!length(cfg$fields)) cfg_error("no fields defined")
  for (f in names(cfg$fields)) {
    check_ident(f, paste0("fields.", f))
    def <- cfg$fields[[f]]
    if (!isTRUE(def$table %in% c("hauls", "haul_catch"))) {
      cfg_error("fields.", f, ".table must be 'hauls' or 'haul_catch'")
    }
    if (!isTRUE(def$kind %in% c("set", "range"))) {
      cfg_error("fields.", f, ".kind must be 'set' or 'range'")
    }
  }
  reserved <- c("grid_id", "grid_shape", "catch_weight_mt", "catch_count")
  if (any(names(cfg$fields) %in% reserved)) {
    cfg_error("field names may not be any of: ", paste(reserved, collapse = ", "))
  }

  if (!length(cfg$grids)) cfg_error("no grids defined")
  for (g in names(cfg$grids)) {
    check_ident(g, paste0("grids.", g))
    def <- cfg$grids[[g]]
    for (k in c("schema", "table", "id", "geom")) {
      check_ident(def[[k]], paste0("grids.", g, ".", k))
    }
    if (!is.null(def$allocation_table)) {
      check_ident(def$allocation_table, paste0("grids.", g, ".allocation_table"))
    }
    if (!identical(as.integer(def$srid), 3338L)) {
      cfg_error("grids.", g, ".srid must be 3338 (haul polygons are in EPSG:3338)")
    }
  }

  A <- cfg$apportionment
  if (!is.numeric(A$min_share %||% 0) || length(A$min_share %||% 0) != 1 ||
      (A$min_share %||% 0) < 0 || (A$min_share %||% 0) >= 1) {
    cfg_error("apportionment.min_share must be a number in [0, 1)")
  }
  if (length(A$exempt_haul_types) && is.null(cfg$hauls$haul_type)) {
    cfg_error("apportionment.exempt_haul_types needs hauls.haul_type to be configured")
  }

  if (!is.null(cfg$areas)) {
    for (k in c("schema", "table", "source", "code", "geom")) {
      check_ident(cfg$areas$view[[k]], paste0("areas.view.", k))
    }
    for (s in names(cfg$areas$sources)) {
      if (!is_area_source(s)) cfg_error("areas.sources: invalid source name '", s, "'")
    }
  }

  if (!length(cfg$rules)) cfg_error("at least one confidentiality rule is required")
  for (r in cfg$rules) validate_rule(r)

  cfg
}

#' @noRd
validate_rule <- function(r) {
  check_ident(r$name, "rule name")
  if (!identical(r$type, "min_distinct")) {
    cfg_error("rule ", r$name, ": unsupported type '", r$type, "'")
  }
  check_ident(r$column, paste0("rule ", r$name, ".column"))
  if (!is.numeric(r$min) || length(r$min) != 1 || r$min < 1 || r$min != round(r$min)) {
    cfg_error("rule ", r$name, ": min must be a positive whole number")
  }
  if (!is.null(r$filter)) {
    check_ident(r$filter$column, paste0("rule ", r$name, ".filter.column"))
    if (!isTRUE(r$filter$op %in% c("in", "not_in"))) {
      cfg_error("rule ", r$name, ": filter.op must be 'in' or 'not_in'")
    }
    if (!length(r$filter$values)) cfg_error("rule ", r$name, ": filter.values is empty")
  }
  invisible(r)
}

#' Is `x` a plain lowercase SQL identifier?
#' @noRd
is_ident <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && grepl("^[a-z_][a-z0-9_]*$", x)
}

#' @noRd
check_ident <- function(x, what) {
  if (!is_ident(x)) {
    cfg_error(what, " must be a lowercase SQL identifier (letters, digits, underscore); got: ",
              paste(format(x), collapse = " "))
  }
  invisible(x)
}

#' @noRd
cfg_error <- function(...) stop("Invalid configuration: ", ..., call. = FALSE)

#' Is `x` a valid spatial area source name (as stored in the areas view)?
#' @noRd
is_area_source <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && grepl("^[A-Za-z0-9_]{1,64}$", x)
}

#' Display name of an area source
#' @noRd
area_source_label <- function(source, cfg) {
  cfg$areas$sources[[source]]$label %||% source
}
