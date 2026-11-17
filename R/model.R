# Data functions for the Recovery Zone Options dashboard.
#
# Scenario runs of a fine-scale population model (after Aston et al. 2025): median and 95% interval of
# 200 simulations per scenario for spawning biomass and egg production, 2025-2065.
# Hypothetical illustration only, not a stock projection or management advice.
#
# data/scenarios.csv is built from the raw model output by data-raw/prep_scenarios.R. Scenarios 13-15
# (all existing NTZs removed) are left out.

# Shiny sources R/ before app.R, so this file loads its own packages
library(dplyr)
library(readr)

RZ_LEVELS <- c("None", "Small", "Medium", "Large")  # recovery-zone options ("None" = existing NTZs only)
CLOSURES  <- c(5L, 7L, 9L)                          # modelled spawning-closure lengths (months)
METRICS   <- c(biomass = "Spawning biomass", eggs = "Egg production")
RZ0       <- "Large"                                # defaults
CL0       <- 5L

# all scenarios, with values also expressed as % of the 2025 level (every scenario starts from the same stock)
load_scenarios <- function(dir = "data") {
  d <- read_csv(file.path(dir, "scenarios.csv"), show_col_types = FALSE)
  base <- d |> filter(rz == "BAU", year == min(year)) |> select(metric, base = median)
  d |>
    left_join(base, by = "metric") |>
    mutate(m_pct = median / base * 100, lo_pct = lo / base * 100, hi_pct = hi / base * 100) |>
    arrange(metric, scenario, year)
}

# one scenario's series (% of 2025), or NULL if that scenario wasn't run for this metric.
# Base-R subsetting on purpose: the arguments share names with the columns.
get_series <- function(S, metric, rz, closure) {
  s <- S[S$metric == metric & S$rz == rz & S$closure == closure, ]
  if (nrow(s) == 0) return(NULL)
  list(year = s$year, m = s$m_pct, lo = s$lo_pct, hi = s$hi_pct, raw = s$median, scenario = s$scenario[1])
}

# the three lines on each chart: business as usual, closure only (no new zones), recovery zones + closure
scenario_lines <- function(S, metric, rz, closure) {
  list(bau = get_series(S, metric, "BAU", 0),
       cl  = get_series(S, metric, "None", closure),
       rz  = if (rz != "None") get_series(S, metric, rz, closure))
}

# value in the final year (% of 2025)
end_value <- function(S, metric, rz, closure) {
  s <- get_series(S, metric, rz, closure)
  if (is.null(s)) NA_real_ else s$m[length(s$m)]
}
