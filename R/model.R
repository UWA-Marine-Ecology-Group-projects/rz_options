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
CLOSURES  <- c(0L, 4L, 5L, 6L, 7L, 8L, 9L)          # spawning-closure lengths (months; 0 = no closure). The app shows
# only those that are in data/scenarios.csv
CLOSURE_MONTHS <- c(`4` = "Dec–Mar", `8` = "Oct–May")  # months closed, where known
CL_FIXED  <- 8L                                     # closure (no new zones) that can be kept on the charts as a comparison
METRICS   <- c(biomass = "Spawning biomass", eggs = "Egg production")
RZ0       <- "Large"                                # defaults
CL0       <- 5L

# all scenarios, with relative values (proportion of unfished) expressed as %
load_scenarios <- function(dir = "data") {
  read_csv(file.path(dir, "scenarios.csv"), show_col_types = FALSE) |>
    # business as usual is the same scenario as no new zones with no closure
    mutate(closure = if_else(rz == "BAU", 0L, as.integer(closure)), rz = if_else(rz == "BAU", "None", rz),
           m_pct = rel * 100, lo_pct = rel_lo * 100, hi_pct = rel_hi * 100) |>
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

# the lines on each chart: no spawning closure (existing NTZs only), closure only (no new zones), recovery zones +
# closure, and optionally a fixed closure with no new zones (`fixed`, months). With no closure selected (closure = 0)
# the closure-only line is the same as the first; the fixed line is left out when it is the selected closure.
scenario_lines <- function(S, metric, rz, closure, fixed = NULL) {
  list(bau = get_series(S, metric, "None", 0),
       cl  = if (closure > 0) get_series(S, metric, "None", closure),
       rz  = if (rz != "None") get_series(S, metric, rz, closure),
       fx  = if (!is.null(fixed) && fixed != closure) get_series(S, metric, "None", fixed))
}

# value in a given year, or the final year if none is given (% of unfished)
end_value <- function(S, metric, rz, closure, year = NULL) {
  s <- get_series(S, metric, rz, closure)
  if (is.null(s)) return(NA_real_)
  if (is.null(year)) s$m[length(s$m)] else s$m[match(year, s$year)]
}