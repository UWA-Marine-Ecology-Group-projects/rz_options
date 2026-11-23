# Recovery Zone Options: Demersal Fish Scenarios
# Run locally with shiny::runApp("rz_options") or deploy the folder to Shiny Server (see README.md).
# Concept illustration only: not a stock projection or management advice.

library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(plotly)
library(dplyr)
library(readr)
library(purrr)

# Text uses en dashes and other non-ASCII characters: make sure R works in UTF-8 even if the server's locale is "C"
if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE))

# Shiny loads R/model.R automatically before this file; source it here only if that's switched off
if (!exists("load_scenarios")) source("R/model.R", local = TRUE)

# ---- Setup -------------------------------------------------------------------------------------------

S     <- load_scenarios("data")
START <- min(S$year)
END   <- max(S$year)
CLOSURES <- CLOSURES[CLOSURES %in% S$closure]  # only closure lengths that have been modelled
# spawning-biomass reference levels (% unfished), shown on the biomass chart when ticked in the sidebar
REF <- list(target = c(40, 60), threshold = 30, limit = 20)
REF_COL <- c(target = "rgba(144,238,144,0.35)", threshold = "#f0a030", limit = "#e03c31")
REF_NAMES <- c(target = sprintf("Target range (%g–%g%%)", REF$target[1], REF$target[2]),
               threshold = sprintf("Threshold (%g%%)", REF$threshold), limit = sprintf("Limit (%g%%)", REF$limit))

REPORT_YEAR <- 2040              # starting year for the all-scenarios table (picked with a slider)
BOX_YEARS   <- c(2030, 2040)     # years shown in the summary boxes under each chart
OPTS  <- read_csv("data/rz_options.csv", show_col_types = FALSE) |> filter(preset_label %in% RZ_LEVELS)

# no spawning closure / closure only / recovery zones + closure / fixed closure, no new zones
COL  <- c(bau = "#6c757d", cl = "#d9622b", rz = "#1d6fa5", fx = "#7a4fa0")
BAND <- c(bau = "rgba(108,117,125,0.12)", cl = "rgba(217,98,43,0.15)", rz = "rgba(29,111,165,0.15)",
          fx = "rgba(122,79,160,0.12)")

# fixed y-axis maximum per metric across all scenarios, so changing a scenario never rescales the axis
YMAX <- S |> group_by(metric) |> summarise(off = max(m_pct), on = max(hi_pct, m_pct, na.rm = TRUE))
ymax_for <- function(metric, band) YMAX[[if (band) "on" else "off"]][YMAX$metric == metric]

# common starting level (every scenario starts from the same stock), drawn as a dotted reference line
start_level <- function(metric) S$m_pct[S$metric == metric & S$rz == "None" & S$closure == 0 & S$year == START]

pts_change <- function(a, b) { r <- round(a - b); paste0(if (r >= 0) "+" else "−", abs(r), " pts") }  # percentage points
fmt_pct    <- function(v) if (is.na(v)) "–" else paste0(round(v), "%")
closure_months <- function(cl) unname(CLOSURE_MONTHS[as.character(cl)])  # e.g. "Oct–May", NA if not given
closure_lab <- function(cl) {
  if (cl == 0) return("no spawning closure")
  w <- closure_months(cl)
  paste0(cl, "-month closure", if (!is.na(w)) paste0(" (", w, ")"))
}
closure_choice <- function(cl) ifelse(cl == 0, "None", paste(cl, "months"))
closure_tick   <- function(cl) ifelse(cl == 0, "None", as.character(cl))  # short labels for the slider
# "none, 4 (Dec–Mar), 5, ... or 9" for the tooltip
closure_list <- function(cl) {
  w <- closure_months(cl)
  x <- ifelse(cl == 0, "none", ifelse(is.na(w), cl, paste0(cl, " (", w, ")")))
  paste(paste(head(x, -1), collapse = ", "), "or", tail(x, 1))
}
YLAB <- c(biomass = "% unfished spawning biomass", eggs = "% unfished egg production")

opt_text <- function(rz) {
  if (rz == "None") return("No new recovery zones (existing NTZs only)")
  o <- OPTS[OPTS$preset_label == rz, ]
  if (nrow(o) == 0) return(paste(rz, "option"))
  if (is.na(o$zones)) return(sprintf("%s: %g%% of mapped reef", o$option, o$share_of_mapped_reef_pct))
  sprintf("%s: %d zones, %.1f%% of mapped reef", o$option, as.integer(o$zones), o$share_of_mapped_reef_pct)
}

# label with an info icon that shows a tooltip on hover or tap
info <- function(label, tip) {
  span(label, tooltip(bs_icon("info-circle", class = "text-secondary ms-1", a11y = "sem", title = "More information"), tip))
}

# ---- Text --------------------------------------------------------------------------------------------

about_modal <- function() {
  modalDialog(
    title = "About this dashboard",
    p(strong("Concept illustration only."), "This dashboard shows scenario runs of a fine-scale population model of a
      West Coast demersal fish, West Australian dhufish", em("(Glaucosoma hebraicum),"), "built on the approach of
      Aston et al. (2025), with egg production that allows for reproductive output scaling hyperallometrically
      with body mass."),
    p("It shows what a scenario analysis of spawning closures and recovery zones could look like for spawning biomass
      and egg production. The plots are", strong("not"), "stock projections or management advice and should not be
      relied upon."),
    footer = modalButton("I understand"),
    easyClose = TRUE, size = "l"
  )
}

TIPS <- list(
  refs    = "Adds the target range (green), threshold (orange) and limit (red) to the spawning biomass chart.",
  bau     = "Adds the no spawning closure scenario (dashed line): existing NTZs only, no spawning closure.",
  fixed   = paste0("Keeps the ", closure_lab(CL_FIXED), " with no new recovery zones on the charts (purple line) as a
             fixed comparison, whatever is selected above."),
  closure = paste0("Months closed to fishing each year during the spawning season: ", closure_list(CLOSURES), "."),
  rz      = "Recovery-zone option added to the existing no-take zones. 'None' keeps the existing NTZs only, with the
             spawning closure.",
  band    = "2.5th to 97.5th percentiles of the 200 simulations for each scenario.",
  biomass = paste0("Spawning biomass as a percentage of unfished. Every scenario starts from the same stock in ",
                   START, " (dotted line). Lines are medians of 200 simulations."),
  eggs    = paste0("Egg production as a percentage of unfished, allowing for big, old females that spawn for longer,
             more often and in bigger batches. Every scenario starts from the same stock in ", START, " (dotted line).
             Lines are medians of 200 simulations.")
)

# ---- UI ----------------------------------------------------------------------------------------------

sidebar_ui <- sidebar(
  width = 320,
  # sliders that snap to the modelled options only (closure lengths aren't evenly spaced, zones are categories)
  sliderTextInput("closure", info("Spawning closure length (months)", TIPS$closure),
                  choices = closure_tick(CLOSURES), selected = closure_tick(CL0),
                  grid = TRUE, force_edges = TRUE, hide_min_max = TRUE, width = "100%"),
  hr(),
  sliderTextInput("rz", info("Recovery zones", TIPS$rz), choices = RZ_LEVELS, selected = RZ0,
                  grid = TRUE, force_edges = TRUE, hide_min_max = TRUE, width = "100%"),
  div(class = "control-note", textOutput("rz_txt", inline = TRUE)),
  hr(),
  checkboxInput("bau", info("Show no spawning closure option on plots", TIPS$bau), FALSE),
  checkboxInput("fixed", info(paste0("Always show ", CL_FIXED, "-month closure, no new zones"), TIPS$fixed), FALSE),
  checkboxInput("refs", info("Show reference levels", TIPS$refs), FALSE),
  checkboxInput("band", info("Show 95% interval", TIPS$band), FALSE),
  actionButton("reset", "Reset", icon = icon("rotate-left"), class = "btn-sm btn-link px-0")
)

chart_card <- function(id, objective, title, tip) {
  card(
    full_screen = TRUE,
    card_header(
      div(class = "objective", objective),
      div(class = "d-flex align-items-center", h5(class = "mb-0", title), tooltip(bs_icon("info-circle", class = "text-secondary ms-2"), tip))
    ),
    card_body(class = "pb-2",
              plotlyOutput(paste0("plot_", id), height = "340px"),
              uiOutput(paste0("legend_", id))),
    card_footer(uiOutput(paste0("boxes_", id)))
  )
}

ui <- page_sidebar(
  title = div(class = "d-flex align-items-center w-100 gap-3",
              span(class = "app-title", "Recovery Zone Scenarios"),
              span(class = "app-subtitle d-none d-md-inline", "Demersal fish scenarios · West Coast"),
              actionButton("about", "About & disclaimer", icon = icon("circle-info"), class = "btn-sm btn-outline-light ms-auto")),
  window_title = "Recovery Zone Options",
  theme = bs_theme(version = 5, primary = "#12465e", "navbar-bg" = "#12465e",
                   base_font = font_google("Source Sans 3", local = FALSE), font_scale = 0.95),
  fillable = FALSE,
  tags$head(includeCSS("www/styles.css")),  # inlined, so the page never depends on fetching the file separately
  sidebar = sidebar_ui,
  
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)),
    chart_card("biomass", "Objective A · Spawning biomass", "Spawning biomass (% of unfished)", TIPS$biomass),
    chart_card("eggs", "Objective B · Egg production", "Egg production (% of unfished)", TIPS$eggs)
  ),
  
  card(
    card_header(div(class = "objective", "All scenarios"),
                h5(class = "mb-0", textOutput("grid_title", inline = TRUE))),
    card_body(
      div(class = "grid-year",
          sliderInput("grid_year", "Year", min = START, max = END, value = REPORT_YEAR, step = 1, sep = "",
                      ticks = FALSE, width = "100%")),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = c(6, 6)),
        div(h6("Spawning biomass"), uiOutput("grid_biomass")),
        div(h6("Egg production"), uiOutput("grid_eggs"))
      ),
      p(class = "small text-secondary mb-0", "The highlighted cell is the scenario selected in the sidebar.")
    )
  ),
  
  accordion(
    open = FALSE,
    accordion_panel(
      "Why big females matter", icon = bs_icon("graph-up-arrow"),
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        value_box("Spawning season, 900 mm vs 350 mm female", "6 vs 2 months"),
        value_box("Spawning events per season", "155 vs 60"),
        value_box("2 kg females to match one 10 kg female", "≈ 15", p("(5 if eggs scaled with weight)"))
      ),
      p(class = "small text-secondary mt-2 mb-0", "Evans-Powell et al. (2024). Across marine fishes, reproductive-energy
        output rises faster than body mass (Barneche et al. 2018).")
    ),
    accordion_panel(
      "Data tables", icon = bs_icon("table"),
      h6("Recovery-zone options"), tableOutput("tbl_opt"),
      h6(textOutput("tbl_title", inline = TRUE)),
      layout_columns(
        div(h6("Spawning biomass (% of unfished)"), tableOutput("tbl_biomass")),
        div(h6("Egg production (% of unfished)"), tableOutput("tbl_eggs"))
      )
    ),
    accordion_panel(
      "Methods and references", icon = bs_icon("book"),
      tags$ul(class = "small",
              tags$li("Each line is the median of 200 simulations for one modelled scenario; the band is the 2.5th to 97.5th
                 percentile."),
              tags$li(paste0("Values are relative to the unfished level. All scenarios start from the same stock in ", START, ".")),
              tags$li("Differences between scenarios are given in percentage points (pts) of unfished."),
              tags$li("No spawning closure keeps the existing no-take zones with no closure. 'No new recovery zones'
                 keeps the existing no-take zones and adds the spawning closure."),
              tags$li("Some scenarios, including the 4-month (Dec–Mar) and 8-month (Oct–May) closures and recovery zones
                 with no spawning closure, are medians only, without a 95% interval.")
      ),
      div(class = "small text-secondary refs",
          p("Aston C, Hesp A, Navarro M, Goetze JS, Wise B, Langlois T (2025). A fine-scale fish population model reveals
           how integration of spatial and temporal management can maximise conservation and fisheries benefits.",
           em("Journal of Applied Ecology"), "62(11): 2954–2970.",
           a("doi.org/10.1111/1365-2664.70185", href = "https://doi.org/10.1111/1365-2664.70185", target = "_blank")),
          p("Evans-Powell RT, Hesp SA, Denham A, Beckley LE (2024). Implications of big, old, fat, fecund, female fish
           (BOFFFFs) for the reproductive potential of a demersal teleost stock.", em("Fisheries Research"), "272: 106934.",
           a("doi.org/10.1016/j.fishres.2023.106934", href = "https://doi.org/10.1016/j.fishres.2023.106934", target = "_blank")),
          p("Barneche DR, Robertson DR, White CR, Marshall DJ (2018). Fish reproductive-energy output increases
           disproportionately with body size.", em("Science"), "360: 642–645.",
           a("doi.org/10.1126/science.aao6868", href = "https://doi.org/10.1126/science.aao6868", target = "_blank")),
          p("Fisher EA, Fairclough DV, Lek E, Briggs J, Hesp SA, Denham A (2025). West Coast Demersal Scalefish Resource:
           2025 Assessment. Resource Assessment Report No. 2, DPIRD, Western Australia.")
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  showModal(about_modal())
  observeEvent(input$about, showModal(about_modal()))
  
  observeEvent(input$reset, {
    updateSliderTextInput(session, "closure", selected = closure_tick(CL0))
    updateSliderTextInput(session, "rz", selected = RZ0)
    updateCheckboxInput(session, "bau", value = FALSE)
    updateCheckboxInput(session, "fixed", value = FALSE)
    updateCheckboxInput(session, "refs", value = FALSE)
    updateCheckboxInput(session, "band", value = FALSE)
    updateSliderInput(session, "grid_year", value = REPORT_YEAR)
  })
  
  closure <- reactive(CLOSURES[match(input$closure, closure_tick(CLOSURES))])
  rz      <- reactive(input$rz)
  fixed   <- reactive(if (isTRUE(input$fixed)) CL_FIXED)  # NULL when not ticked
  
  NAMES <- reactive(c(bau = "No spawning closure",
                      cl  = paste0(closure_lab(closure()), ", no new zones"),
                      rz  = paste0(rz(), " recovery zones", if (closure() == 0) ", no spawning closure" else paste(" +", closure_lab(closure()))),
                      fx  = paste0(closure_lab(CL_FIXED), ", no new zones")))
  
  # shorter names for the hover box
  SHORT <- reactive(c(bau = "No spawning closure", cl = paste0(closure(), "-month spawning closure"),
                      rz = paste0(rz(), " zones", if (closure() == 0) ", no closure" else paste0(" + ", closure(), "-month closure")),
                      fx = paste0(CL_FIXED, "-month closure, no new zones")))
  show_bau <- reactive(input$bau || closure() == 0)  # with no closure selected, it is the comparison line
  
  output$rz_txt <- renderText(opt_text(rz()))
  
  # ---- charts ----
  trend_plot <- function(metric) {
    L <- scenario_lines(S, metric, rz(), closure(), fixed())
    if (!show_bau()) L$bau <- NULL  # no-closure line only when ticked in the sidebar, or no closure is selected
    keys <- intersect(c("bau", "fx", "cl", "rz"), names(L)[!map_lgl(L, is.null)])
    p <- plot_ly()
    if (input$band) for (k in keys[!map_lgl(keys, \(k) all(is.na(L[[k]]$lo)))]) {  # some runs have no interval
      p <- add_ribbons(p, x = L[[k]]$year, ymin = L[[k]]$lo, ymax = L[[k]]$hi, fillcolor = BAND[[k]],
                       line = list(width = 0), hoverinfo = "skip", showlegend = FALSE)
    }
    for (k in keys) {
      p <- add_lines(p, x = L[[k]]$year, y = L[[k]]$m, name = SHORT()[[k]], hovertemplate = "%{y:.0f}%",
                     line = list(color = COL[[k]], width = if (k %in% c("bau", "fx")) 2 else 2.5, dash = if (k == "bau") "dash" else "solid"),
                     showlegend = FALSE)
    }
    start_line <- list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = start_level(metric), y1 = start_level(metric),
                       line = list(color = "#adb5bd", width = 1, dash = "dot"))
    refs_on <- isTRUE(input$refs) && metric == "biomass"
    ref_shapes <- if (refs_on) list(
      list(type = "rect", xref = "x", x0 = START, x1 = END, y0 = REF$target[1], y1 = REF$target[2],
           fillcolor = REF_COL[["target"]], line = list(width = 0), layer = "below"),
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = REF$threshold, y1 = REF$threshold,
           line = list(color = REF_COL[["threshold"]], width = 1.5)),
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = REF$limit, y1 = REF$limit,
           line = list(color = REF_COL[["limit"]], width = 1.5))
    )
    ytop <- max(ymax_for(metric, input$band), if (refs_on) REF$target[2]) * 1.05
    
    # scenarios asked for but not run for this metric
    missing <- c(if (rz() != "None" && is.null(L$rz)) NAMES()[["rz"]],
                 if (!is.null(fixed()) && fixed() != closure() && is.null(L$fx)) NAMES()[["fx"]])
    note <- if (length(missing) > 0)
      list(list(text = paste0(missing, ": not yet modelled for ", tolower(METRICS[[metric]]), collapse = "<br>"),
                xref = "paper", yref = "paper", x = 0.02, y = 0.98, xanchor = "left", yanchor = "top", align = "left",
                showarrow = FALSE, font = list(color = "#6c757d")))
    p |>
      layout(
        font = list(family = "Source Sans 3, sans-serif", size = 13, color = "#495057"),
        hovermode = "x unified",
        # system font for hover labels: plotly sizes the box before web fonts load, which clipped the text
        hoverlabel = list(font = list(family = "system-ui, -apple-system, 'Segoe UI', Arial, sans-serif", size = 13)),
        margin = list(l = 10, r = 10, t = 10, b = 10),
        xaxis = list(title = "", tickvals = seq(START, END, by = 5), range = c(START - 1, END + 1), showgrid = FALSE,
                     fixedrange = TRUE),
        yaxis = list(title = list(text = YLAB[[metric]], standoff = 10), automargin = TRUE, ticksuffix = "%",
                     range = c(0, ytop), gridcolor = "#e9ecef", zeroline = FALSE, fixedrange = TRUE),
        shapes = c(ref_shapes, list(start_line)),
        annotations = note
      ) |>
      config(displayModeBar = FALSE)
  }
  
  # legend drawn in HTML above each chart, so long names wrap instead of overlapping
  chart_legend <- function(metric) {
    L <- scenario_lines(S, metric, rz(), closure(), fixed())
    keys <- c("rz", "cl", "fx", "bau")[c(!is.null(L$rz), !is.null(L$cl), !is.null(L$fx), show_bau())]
    refs <- if (isTRUE(input$refs) && metric == "biomass") names(REF_NAMES)
    div(class = "chart-legend",
        map(keys, \(k) span(class = "item", span(class = paste("swatch", paste0("swatch-", k))), NAMES()[[k]])),
        map(refs, \(k) span(class = "item", span(class = paste("swatch", paste0("swatch-", k))), REF_NAMES[[k]])))
  }
  output$legend_biomass <- renderUI(chart_legend("biomass"))
  output$legend_eggs    <- renderUI(chart_legend("eggs"))
  
  output$plot_biomass <- renderPlotly(trend_plot("biomass"))
  output$plot_eggs    <- renderPlotly(trend_plot("eggs"))
  
  # ---- summary boxes (one row per year in BOX_YEARS) ----
  summary_boxes <- function(metric, year) {
    bau <- end_value(S, metric, "None", 0, year)
    cl  <- if (closure() > 0) end_value(S, metric, "None", closure(), year)
    box <- \(title, value, note, key) div(class = paste("stat", paste0("stat-", key)),
                                          div(class = "stat-title", title), div(class = "stat-value", value), div(class = "stat-note", note))
    boxes <- list(
      box("No spawning closure", fmt_pct(bau), paste0("of unfished (", round(start_level(metric)), "% in ", START, ")"), "bau")
    )
    if (!is.null(cl)) {
      boxes <- c(boxes, list(box(sub("^(.)", "\\U\\1", closure_lab(closure()), perl = TRUE), fmt_pct(cl), paste(pts_change(cl, bau), "vs no spawning closure"), "cl")))
    }
    if (rz() != "None") {
      v   <- end_value(S, metric, rz(), closure(), year)
      ref <- if (is.null(cl)) bau else cl
      vs  <- if (is.null(cl)) "vs no spawning closure" else "vs closure only"
      title <- paste0(rz(), " zones", if (closure() == 0) ", no closure" else " + closure")
      boxes <- c(boxes, list(box(title, fmt_pct(v), if (is.na(v)) "Not yet modelled" else paste(pts_change(v, ref), vs), "rz")))
    }
    if (!is.null(fixed()) && fixed() != closure()) {
      v <- end_value(S, metric, "None", fixed(), year)
      boxes <- c(boxes, list(box(paste0(fixed(), "-month closure, no new zones"), fmt_pct(v),
                                 if (is.na(v)) "Not yet modelled" else paste(pts_change(v, bau), "vs no spawning closure"), "fx")))
    }
    div(class = "box-year",
        div(class = "box-year-label", paste("In", year)),
        do.call(layout_column_wrap, c(list(width = "140px", fill = FALSE), boxes)))
  }
  year_boxes <- function(metric) tagList(map(BOX_YEARS, \(y) summary_boxes(metric, y)))
  output$boxes_biomass <- renderUI(year_boxes("biomass"))
  output$boxes_eggs    <- renderUI(year_boxes("eggs"))
  
  # ---- all-scenario grids ----
  grid_year <- reactive(if (is.null(input$grid_year)) REPORT_YEAR else input$grid_year)
  output$grid_title <- renderText(paste(grid_year(), "Spawning Biomass and Egg Production (% of unfished)"))
  scenario_grid <- function(metric) {
    cell <- \(r, cl) {
      sel <- r == rz() && cl == closure()
      tags$td(class = if (sel) "sel", fmt_pct(end_value(S, metric, r, cl, grid_year())))
    }
    tags$table(class = "table table-sm scen-grid",
               tags$thead(
                 tags$tr(tags$th(rowspan = 2, class = "corner", "Recovery-zone option"),
                         tags$th(colspan = length(CLOSURES), class = "group", "Spawning closure length")),
                 tags$tr(map(CLOSURES, \(cl) tags$th(closure_choice(cl))))
               ),
               tags$tbody(
                 map(RZ_LEVELS, \(r) tags$tr(tags$th(r), map(CLOSURES, \(cl) cell(r, cl))))
               )
    )
  }
  output$grid_biomass <- renderUI(scenario_grid("biomass"))
  output$grid_eggs    <- renderUI(scenario_grid("eggs"))
  
  # ---- tables ----
  output$tbl_title <- renderText(paste0("Selected scenario: ", if (rz() == "None") "no new recovery zones" else paste(rz(), "recovery zones"),
                                        ", ", closure_lab(closure())))
  series_table <- function(metric) {
    L <- scenario_lines(S, metric, rz(), closure(), fixed())
    f <- \(s) if (is.null(s)) rep("–", length(L$bau$year)) else formatC(s$m, format = "f", digits = 0)
    d <- tibble(Year = as.character(L$bau$year), `No spawning closure` = f(L$bau))
    if (!is.null(L$cl)) d[["Closure only"]] <- f(L$cl)
    if (rz() != "None") d[[paste(rz(), if (closure() == 0) "zones, no closure" else "zones + closure")]] <- f(L$rz)
    if (!is.null(fixed()) && fixed() != closure()) d[[paste0(fixed(), "-month closure only")]] <- f(L$fx)
    d
  }
  output$tbl_biomass <- renderTable(series_table("biomass"), align = "r", striped = TRUE, spacing = "s")
  output$tbl_eggs    <- renderTable(series_table("eggs"), align = "r", striped = TRUE, spacing = "s")
  
  output$tbl_opt <- renderTable(
    OPTS |>
      transmute(
        Option = option,
        `GIS layer` = coalesce(gis_layer, "–"),
        Zones = if_else(is.na(zones), "–", as.character(zones)),
        `Zone area (km²)` = if_else(is.na(zone_area_km2), "–", formatC(zone_area_km2, format = "d", big.mark = ",")),
        `Reef inside zones (km²)` = if_else(is.na(reef_inside_km2), "–", formatC(reef_inside_km2, format = "d", big.mark = ",")),
        `Share of mapped reef` = sprintf("%.1f%%", share_of_mapped_reef_pct)
      ),
    align = "llrrrr", striped = TRUE, spacing = "s"
  )
}

shinyApp(ui, server)