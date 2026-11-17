# Build data/scenarios.csv from the raw model output.
# Run from the rz_options folder: source("data-raw/prep_scenarios.R")
#
# Inputs: one CSV per metric with 200 simulation columns (V1-V200) plus Scenario, the median column,
# P_0.025, P_0.975 and Year. Only the summary columns are kept. Scenarios 13-15 (all existing NTZs
# removed) are dropped.

library(dplyr)
library(readr)
library(stringr)

files <- c(biomass = "data-raw/median_biomass_scenarios.csv",
           eggs    = "data-raw/median_egg_production_scenarios.csv")

read_metric <- function(path, metric) {
  read_csv(path, show_col_types = FALSE) |>
    select(Scenario, median = starts_with("median_"), lo = `P_0.025`, hi = `P_0.975`, year = Year) |>
    filter(!str_detect(Scenario, "No NTZ")) |>
    mutate(
      metric   = metric,
      scenario = as.integer(str_match(Scenario, "^Scenario (\\d+)")[, 2]),
      rz       = case_when(str_detect(Scenario, "Business as usual") ~ "BAU",
                           str_detect(Scenario, "No RZ") ~ "None",
                           TRUE ~ str_match(Scenario, ": (\\w+) RZ")[, 2]),
      closure  = if_else(rz == "BAU", 0L, as.integer(str_match(Scenario, "(\\d+) month")[, 2]))
    )
}

scen <- bind_rows(lapply(names(files), \(m) read_metric(files[[m]], m))) |>
  select(scenario, rz, closure, metric, year, median, lo, hi) |>
  arrange(metric, scenario, year)

stopifnot(!anyNA(scen))
write_csv(scen, "data/scenarios.csv")
count(scen, metric, rz, closure)
