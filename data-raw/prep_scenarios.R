# Build data/scenarios.csv from the raw model output.
# Run from the rz_options folder: source("data-raw/prep_scenarios.R")
#
# Inputs: one CSV per metric with 200 simulation columns (V1-V200) plus Scenario, the median column,
# P_0.025, P_0.975, Year and the relative column (relative_biomass / relative_eggs, a proportion of unfished).
# Only the summary columns are kept; the 95% interval is put on the same relative scale as the median.
# The "No NTZs" scenarios (all existing NTZs removed) are dropped.
#
# Extra runs with medians only (Year, Scenario, relative_* in %) are added from `extra`, for scenarios not
# already in the main files. These have no 95% interval.
#
# Business as usual is stored as recovery zones "None" with closure 0 (no spawning closure).

library(dplyr)
library(readr)
library(stringr)

files <- c(biomass = "data-raw/median_biomass_scenarios_1.csv",
           eggs    = "data-raw/median_egg_production_scenarios_1.csv")

extra <- c(biomass = "data-raw/median_biomass_scenarios2_1.csv")  # adds recovery zones with no temporal closure

# recovery-zone option and closure length (0 = none) from the scenario name
label_scenarios <- function(d) {
  d |>
    filter(!str_detect(Scenario, "No NTZ")) |>
    mutate(
      scenario = Scenario,
      rz       = case_when(str_detect(Scenario, "Business as usual") ~ "None",
                           str_detect(Scenario, "^No RZ") ~ "None",
                           TRUE ~ str_match(Scenario, "^(.+) RZ")[, 2]),
      closure  = case_when(str_detect(Scenario, "Business as usual|no temporal closure") ~ 0L,
                           TRUE ~ as.integer(str_match(Scenario, "(\\d+) month")[, 2]))
    )
}

read_metric <- function(path, metric) {
  read_csv(path, show_col_types = FALSE) |>
    select(Scenario, median = starts_with("median_"), lo = `P_0.025`, hi = `P_0.975`, year = Year,
           rel = starts_with("relative_")) |>
    label_scenarios() |>
    mutate(metric = metric,
           rel_lo = lo * rel / median,  # same scaling as the median
           rel_hi = hi * rel / median)
}

read_extra <- function(path, metric) {
  read_csv(path, show_col_types = FALSE) |>
    select(Scenario, year = Year, rel = starts_with("relative_")) |>
    label_scenarios() |>
    mutate(metric = metric, rel = rel / 100, rel_lo = NA_real_, rel_hi = NA_real_)  # given in %
}

main <- bind_rows(lapply(names(files), \(m) read_metric(files[[m]], m)))
more <- bind_rows(lapply(names(extra), \(m) read_extra(extra[[m]], m))) |>
  anti_join(main, by = c("metric", "rz", "closure", "year"))

scen <- bind_rows(main, more) |>
  select(scenario, rz, closure, metric, year, rel, rel_lo, rel_hi) |>
  arrange(metric, rz, closure, year)

stopifnot(!anyNA(select(scen, -rel_lo, -rel_hi)))
write_csv(scen, "data/scenarios.csv", na = "")
count(scen, metric, rz, closure)
