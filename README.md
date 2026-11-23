# Recovery Zone Options: Demersal Fish Scenarios

An interactive Shiny dashboard showing how recovery-zone options and spawning closures compare for spawning
biomass (objective A) and egg production (objective B), from scenario runs of a fine-scale population model.

**Concept illustration only.** Not stock projections or management advice.

## Files

```
rz_options/
├── app.R                     UI and server
├── R/model.R                 data loading and scenario lookups
├── data/scenarios.csv        median and 95% interval (proportion of unfished) per scenario, metric and year
├── data/rz_options.csv       recovery-zone options (zones, area, share of mapped reef)
├── data-raw/prep_scenarios.R builds data/scenarios.csv from the raw model output
└── www/styles.css            styling
```

The old digitised files (`data/aston2025_*.csv`) are no longer used and can be deleted.

## Scenarios

| Recovery zones | Closure |
|---|---|
| None (existing NTZs only), Small, Medium, Large, Extra large (40% of mapped reef) | None, 5, 6, 7, 9 months |

No zones with no closure is the "no spawning closure" scenario (business as usual). Recovery zones with no closure
have spawning biomass only, without a 95% interval; every other scenario has both metrics with intervals, 2025–2060.
The "No NTZs" scenarios (all existing NTZs removed) are not shown.

## Requirements

R 4.1 or later and these packages:

```r
install.packages(c("shiny", "shinyWidgets", "bslib", "bsicons", "plotly", "dplyr", "readr", "purrr"))
```

`data-raw/prep_scenarios.R` also needs `stringr`.

## Run

```r
shiny::runApp("rz_options")
```

On Shiny Server, copy the folder to `/srv/shiny-server/` and open `http://<server>:3838/rz_options/`.
On shinyapps.io or Posit Connect: `rsconnect::deployApp("rz_options")`.

## Using the dashboard

- Use the two sliders in the sidebar to pick a closure length (None, 5, 6, 7 or 9 months) and a recovery-zone
  option (None to Extra large). The sliders snap to the modelled options only.
- Each chart shows business as usual (grey dashed), the closure with no new zones (orange) and the closure with
  the chosen recovery zones (blue), as a percentage of unfished. The dotted line is the common 2025 starting level.
- The boxes under each chart give the selected scenarios' values in 2030 and 2040 (set by `BOX_YEARS` in `app.R`).
- The "All scenarios" card shows every scenario's 2040 value (`REPORT_YEAR`), with the selected one highlighted.

Values to check (2060, % of unfished): business as usual 25% biomass and 41% eggs; 5-month closure only 29% and
49%; Large zones + 5-month closure 39% and 68%; Extra large zones + 9-month closure 52% and 92%.

## Updating the data

Put new model output in `data-raw/` as `median_biomass_scenarios_1.csv` and `median_egg_production_scenarios_1.csv`,
plus `median_biomass_scenarios2_1.csv` (the no-closure runs), then run `source("data-raw/prep_scenarios.R")`
from the `rz_options` folder.
