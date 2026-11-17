# Recovery Zone Options: Demersal Fish Scenarios

An interactive Shiny dashboard showing how recovery-zone options and spawning closures compare for spawning
biomass (objective A) and egg production (objective B), from scenario runs of a fine-scale population model.

**Concept illustration only.** Not stock projections or management advice.

## Files

```
rz_options/
├── app.R                     UI and server
├── R/model.R                 data loading and scenario lookups
├── data/scenarios.csv        median and 95% interval per scenario, metric and year (2025–2065)
├── data/rz_options.csv       recovery-zone options (zones, area, share of mapped reef)
├── data-raw/prep_scenarios.R builds data/scenarios.csv from the raw model output
└── www/styles.css            styling
```

The old digitised files (`data/aston2025_*.csv`) are no longer used and can be deleted.

## Scenarios

| Scenario | Recovery zones | Closure |
|---|---|---|
| 1–3 | Large | 5, 7, 9 months |
| 4–6 | Medium | 5, 7, 9 months |
| 7–9 | Small | 5, 7, 9 months (biomass only; no egg-production runs) |
| 10–12 | None (existing NTZs only) | 5, 7, 9 months |
| 16 | Business as usual (existing NTZs, no closure) | – |

Scenarios 13–15 (all existing NTZs removed) are not shown.

## Requirements

R 4.1 or later and these packages:

```r
install.packages(c("shiny", "bslib", "bsicons", "plotly", "dplyr", "readr", "purrr"))
```

`data-raw/prep_scenarios.R` also needs `stringr`.

## Run

```r
shiny::runApp("rz_options")
```

On Shiny Server, copy the folder to `/srv/shiny-server/` and open `http://<server>:3838/rz_options/`.
On shinyapps.io or Posit Connect: `rsconnect::deployApp("rz_options")`.

## Using the dashboard

- Pick a closure length (5, 7 or 9 months) and a recovery-zone option (None, Small, Medium, Large) in the sidebar.
- Each chart shows business as usual (grey dashed), the closure with no new zones (orange) and the closure with
  the chosen recovery zones (blue), as a percentage of the 2025 level.
- The "All scenarios" card shows every scenario's 2065 value, with the selected one highlighted.

Values to check (2065, % of 2025): business as usual 242% biomass and 295% eggs; 5-month closure only 273% and
342%; Large zones + 5-month closure 345% and 452%.

## Updating the data

Put new model output in `data-raw/` as `median_biomass_scenarios.csv` and `median_egg_production_scenarios.csv`,
then run `source("data-raw/prep_scenarios.R")` from the `rz_options` folder.
