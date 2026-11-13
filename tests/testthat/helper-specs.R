cfg_default <- function() load_config(schema = "data")

# A representative set of specs covering haul-level and catch-level filters
# and grouping, both grids, open/closed ranges, and no grouping at all.
example_specs <- function() {
  list(
    dataset_spec("all_catch_by_cell", grid = "hex_20km"),
    dataset_spec("poll_by_year", grid = "hex_20km",
                 filters = list(species_code = "POLL"),
                 group_by = c("year", "species_code")),
    dataset_spec("trawl_flatfish_detail", grid = "hex_8km",
                 filters = list(gear_code = "TRW",
                                species_group_code = c("AKFL", "FLAT"),
                                year = list(min = 2019, max = 2023)),
                 group_by = c("year", "management_program_code",
                              "species_group_code", "species_code")),
    dataset_spec("pcod_targets_since_2021", grid = "hex_20km",
                 filters = list(trip_target_code = "C", year = list(min = 2021)),
                 group_by = c("harvest_sector", "processing_sector", "species_code"))
  )
}

# Suppression stats for rendering tests.
fake_stats <- function(disclosable, dropped = 4) {
  suppression_stats(data.frame(
    rows_kept = 96L, rows_dropped = as.integer(dropped), weight_kept = 990, weight_dropped = 10,
    count_kept = 9000, count_dropped = 1000, weight_source = 1000, count_source = 10000,
    dropped_disclosable = disclosable))
}


# Snapshot transform: the package version appears in SQL headers.
hide_version <- function(x) sub("ciarequests [0-9.]+", "ciarequests <version>", x)
