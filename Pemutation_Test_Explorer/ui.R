library(shiny)
library(bslib)
library(bsicons)
library(colourpicker)
library(shinyjs)

# ── Theme ──────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version      = 5,
  bootswatch   = "minty",
  primary      = "#6C63FF",
  secondary    = "#F8A04B",
  success      = "#2DCE89",
  info         = "#11CDEF",
  base_font    = font_google("Inter"),
  heading_font = font_google("Poppins")
)

# ── UI ─────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  theme = app_theme,

  useShinyjs(),

  # Loading overlay
  tags$div(
    id = "loading_overlay",
    style = paste(
      "display: none;",
      "position: fixed;",
      "top: 0; left: 0;",
      "width: 100%; height: 100%;",
      "background: rgba(108, 99, 255, 0.82);",
      "z-index: 9999;",
      "display: none;",
      "align-items: center;",
      "justify-content: center;",
      "flex-direction: column;"
    ),
    tags$div(
      style = "text-align: center; color: white;",
      tags$div(
        class = "spinner-border",
        style = "width: 3rem; height: 3rem; color: white;",
        role  = "status"
      ),
      tags$br(),
      tags$span("Running simulation...",
                style = "font-size: 1.3em; font-weight: 600; margin-top: 1rem; display: block;"),
      tags$br(),
      tags$button(
        id    = "cancel",
        class = "btn",
        style = paste(
          "margin-top: 0.75rem;",
          "background: rgba(255,255,255,0.15);",
          "color: white;",
          "border: 2px solid rgba(255,255,255,0.7);",
          "border-radius: 8px;",
          "padding: 0.4rem 1.4rem;",
          "font-size: 1em;",
          "font-weight: 600;",
          "cursor: pointer;"
        ),
        onclick = "
          document.getElementById('loading_overlay').style.display = 'none';
          document.getElementById('run').disabled = false;
          document.getElementById('run').innerHTML = '<i class=\"fa fa-play\"></i> Run Simulation';
          Shiny.setInputValue('cancel', Math.random());
        ",
        tags$i(class = "fa fa-times"), " Cancel"
      )
    )
  ),

  # Responsive scaling
  tags$head(tags$style(HTML("
    html { font-size: clamp(11px, 1.2vw, 16px); }

    /* Override shinyjs opacity on disabled elements - prevents page transparency */
    .shinyjs-disabled { opacity: 1 !important; }
    * { opacity: 1 !important; }
    #loading_overlay { opacity: 1 !important; }
    #loading_overlay * { opacity: 1 !important; }
    .card { min-width: 0; overflow: auto; }
    table { font-size: 0.85em; }
    pre { font-size: 0.8em; white-space: pre-wrap; word-break: break-word; }
    @media (max-width: 768px) {
      .bslib-column-wrap { flex-direction: column !important; }
      .col-sm-7, .col-sm-5 { width: 100% !important; flex: 0 0 100% !important; max-width: 100% !important; }
    }
    @media (max-width: 992px) {
      .card-body { padding: 0.5rem !important; }
      .card-header { padding: 0.4rem 0.5rem !important; font-size: 0.9em; }
    }
  "))),

  title = tags$span(
    tags$b("Permutation Test Explorer"),
    tags$small(" | STA380", style = "color: #aaa; font-size: 0.6em;")
  ),

  sidebar = sidebar(
    width = 310,

    accordion(
      open = c("sim", "mc", "plot"),

      accordion_panel(
        "Simulation Settings", value = "sim",
        icon = bs_icon("sliders"),

        sliderInput("n", "Sample size (each group):",
                    min = 5, max = 100, value = 30, step = 5),

        numericInput("B", "Number of permutations:",
                     value = 500, min = 100, max = 5000, step = 100),

        numericInput("seed", "Random seed:",
                     value = 42, min = 1),

        selectInput("dist", "Distribution:",
                    choices  = c("Normal"      = "normal",
                                 "Exponential" = "exponential",
                                 "t (df = 3)"  = "t"),
                    selected = "normal"),

        numericInput("delta", "Mean shift (\u03b4):",
                     value = 0, step = 0.1),

        selectInput("stat", "Test statistic:",
                    choices  = c("Mean difference"   = "mean_diff",
                                 "t-statistic"       = "t_stat",
                                 "Median difference" = "median_diff"),
                    selected = "mean_diff")
      ),

      accordion_panel(
        "Monte Carlo Settings", value = "mc",
        icon = bs_icon("cpu"),

        numericInput("nsim", "Number of simulations:",
                     value = 200, min = 50, max = 1000, step = 50),

        helpText("More simulations = more accurate rejection rate",
                 "estimates, but slower computation.")
      ),

      accordion_panel(
        "Power Curve Settings", value = "power",
        icon = bs_icon("graph-up"),

        sliderInput("delta_max", "Max \u03b4 for power curve:",
                    min = 0.5, max = 3, value = 2, step = 0.1),

        numericInput("nsim_power", "Simulations per \u03b4:",
                     value = 100, min = 50, max = 500, step = 50),

        helpText("Fewer simulations per delta = faster but noisier curve.")
      ),

      accordion_panel(
        "Plot Colours", value = "plot",
        icon = bs_icon("palette"),

        colourpicker::colourInput("col_hist", "Histogram colour:",
                                  value = "#6C63FF"),

        colourpicker::colourInput("col_obs", "Observed statistic line:",
                                  value = "#F8A04B"),

        colourpicker::colourInput("col_mirror", "Mirror line:",
                                  value = "#2DCE89"),

        checkboxInput("show_tdist",
                      "Overlay theoretical t-distribution (t-stat only)",
                      value = FALSE)
      )
    ),

    hr(),

    actionButton("run", "Run Simulation",
                 class = "btn-primary w-100",
                 icon  = icon("play")),

    br(), br(),

    downloadButton("download", "Download data (CSV)",
                   class = "btn-outline-secondary w-100")
  ),

  # ── Main content ──────────────────────────────────────────────────────────
  navset_card_underline(
    id = "tabs",

    # Tab 1: Permutation Distribution
    nav_panel(
      title = tagList(bs_icon("bar-chart-fill"), " Permutation Test"),
      value = "perm",
      layout_columns(
        col_widths = c(7, 5),

        card(
          card_header(class = "bg-primary text-white",
                      bs_icon("bar-chart-fill"), " Permutation Distribution"),
          plotOutput("perm_plot", height = "380px"),
          card_footer(class = "text-muted small",
                      textOutput("perm_pvalue", inline = TRUE))
        ),

        card(
          card_header(class = "bg-success text-white",
                      bs_icon("table"), " Monte Carlo Results"),
          card_body(
            p(tags$b("What is the Type I error rate?"),
              helpText("Rejection rate when \u03b4 = 0. Should be near 0.05.")),
            p(tags$b("What is the power?"),
              helpText("Rejection rate when \u03b4 \u2260 0. Higher = better.")),
            p(tags$b("How often does each test reject?")),
            tableOutput("mc_table"),
            hr(),
            uiOutput("mc_interpretation"),
            hr(),
            tags$b("Monte Carlo Standard Errors"),
            verbatimTextOutput("mc_se")
          )
        )
      )
    ),

    # Tab 2: Power Curve
    nav_panel(
      title = tagList(bs_icon("graph-up"), " Power Curve"),
      value = "power",
      card(
        card_header(class = "bg-primary text-white",
                    bs_icon("graph-up"), " Power vs Effect Size"),
        card_body(
          p(class = "text-muted",
            "Shows how rejection rate changes as the true effect size (\u03b4) increases.",
            "A steeper curve indicates a more powerful test.",
            "Click Run Simulation to compute."),
          plotOutput("power_curve_plot", height = "420px")
        )
      )
    ),

    # Tab 3: Robustness Study
    nav_panel(
      title = tagList(bs_icon("shield-check"), " Robustness Study"),
      value = "robust",
      card(
        card_header(class = "bg-info text-white",
                    bs_icon("shield-check"), " Type I Error and Power Across Distributions"),
        card_body(
          p(class = "text-muted",
            "Compares both tests across all three distributions at the current settings.",
            "Type I error (\u03b4 = 0) and power (\u03b4 = current setting) are shown.",
            "Click Run Simulation to compute."),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Type I Error (\u03b4 = 0)"),
              tableOutput("robust_type1")
            ),
            card(
              card_header(uiOutput("robust_power_header")),
              tableOutput("robust_power")
            )
          ),
          hr(),
          plotOutput("robust_plot", height = "350px")
        )
      )
    )
  )
)
