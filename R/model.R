# Data functions for the Recovery Zone Options dashboard.
#
# Scenario runs of a fine-scale population model (after Aston et al. 2025): median and 95% interval of
# 200 simulations per scenario for spawning biomass and egg production relative to unfished.
# Hypothetical illustration only, not a stock projection or management advice.
#
# data/scenarios.csv is built from the raw model output by data-raw/prep_scenarios.R. The "No NTZs"
# scenarios (all existing NTZs removed) are left out.

# Shiny sources R/ before app.R, so this file loads its own packages
library(dplyr)
library(readr)

RZ_LEVELS <- c("None", "Small", "Medium", "Large", "Extra large")  # recovery-zone options ("None" = existing NTZs only)
CLOSURES  <- c(5L, 6L, 7L, 9L)                      # modelled spawning-closure lengths (months)
METRICS   <- c(biomass = "Spawning biomass", eggs = "Egg production")
RZ0       <- "Large"                                # defaults
CL0       <- 5L

# all scenarios, with relative values (proportion of unfished) expressed as %
load_scenarios <- function(dir = "data") {
  read_csv(file.path(dir, "scenarios.csv"), show_col_types = FALSE) |>
    mutate(m_pct = rel * 100, lo_pct = rel_lo * 100, hi_pct = rel_hi * 100) |>
    arrange(metric, rz, closure, year)
}

# one scenario's series (% of unfished), or NULL if that scenario wasn't run for this metric.
# Base-R subsetting on purpose: the arguments share names with the columns.
get_series <- function(S, metric, rz, closure) {
  s <- S[S$metric == metric & S$rz == rz & S$closure == closure, ]
  if (nrow(s) == 0) return(NULL)
  s <- s[order(s$year), ]
  list(year = s$year, m = s$m_pct, lo = s$lo_pct, hi = s$hi_pct, scenario = s$scenario[1])
}

# the three lines on each chart: business as usual, closure only (no new zones), recovery zones + closure
scenario_lines <- function(S, metric, rz, closure) {
  list(bau = get_series(S, metric, "BAU", 0),
       cl  = get_series(S, metric, "None", closure),
       rz  = if (rz != "None") get_series(S, metric, rz, closure))
}

# value in the final year (% of unfished)
end_value <- function(S, metric, rz, closure) {
  s <- get_series(S, metric, rz, closure)
  if (is.null(s)) NA_real_ else s$m[length(s$m)]
}
