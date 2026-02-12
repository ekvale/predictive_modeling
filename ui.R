# =============================================================================
# Healthcare Access & Continuity Dashboard - UI
# =============================================================================
# Multi-tab layout with shared filters. Designed for conference presentation
# (readable on projectors, color-blind friendly).
# =============================================================================

fluidPage(
  title = "Healthcare Access & Continuity Dashboard",
  theme = bslib::bs_theme(bootswatch = "flatly"),

  # Application title and methodology note
  titlePanel(
    title = div(
      "Healthcare Access & Continuity Dashboard",
      br(),
      tags$small(
        "Synthetic patient data • HIPAA-compliant • No real patient information"
      )
    ),
    windowTitle = "Healthcare Access Dashboard"
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters", class = "text-primary"),
      # Date range for intake
      dateRangeInput(
        "date_range",
        "Intake date range",
        start = min(patients_df$intake_date),
        end = max(patients_df$intake_date),
        min = min(patients_df$intake_date),
        max = max(patients_df$intake_date),
        separator = " to "
      ),
      # Demographics
      checkboxGroupInput(
        "age_groups",
        "Age groups",
        choices = levels(patients_df$age_group),
        selected = levels(patients_df$age_group)
      ),
      checkboxGroupInput(
        "health",
        "General health",
        choices = levels(patients_df$health),
        selected = levels(patients_df$health)
      ),
      # Geographic filter (optional: by lat/lon bounds or "All")
      radioButtons(
        "region_filter",
        "Geographic region",
        choices = c("All regions", "North", "South", "East", "West"),
        selected = "All regions"
      ),
      hr(),
      h4("Disruption scenario", class = "text-secondary"),
      p(
        class = "text-muted small",
        "Simulate a healthcare disruption period that increases dropout and missed appointments in the synthetic data (e.g. reduced access). Default: Minneapolis metro scenario."
      ),
      dateRangeInput(
        "event_range",
        "Disruption period (start – end)",
        start = DEFAULT_EVENT_START,
        end = DEFAULT_EVENT_END,
        min = as.Date("2020-01-01"),
        max = as.Date("2024-12-01"),
        separator = " to ",
        startview = "year"
      ),
      p(
        class = "text-muted small",
        "Changing the period regenerates data with elevated dropout and missed appointments during this window."
      ),
      hr(),
      p(
        class = "text-muted small",
        "Filters apply across all tabs. Data is synthetic and for demonstration only."
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        type = "tabs",

        # ---------------------------------------------------------------------
        # Tab 1: Overview Dashboard
        # ---------------------------------------------------------------------
        tabPanel(
          "Overview Dashboard",
          h3("Summary statistics and key metrics"),
          fluidRow(
            column(3, uiOutput("vb_patients")),
            column(3, uiOutput("vb_appointments")),
            column(3, uiOutput("vb_attendance_rate")),
            column(3, uiOutput("vb_dropout_rate"))
          ),
          fluidRow(
            column(6, plotlyOutput("overview_attendance_trend", height = "320px")),
            column(6, plotlyOutput("overview_missed_reasons", height = "320px"))
          ),
          fluidRow(
            column(12, DT::dataTableOutput("overview_summary_table"))
          )
        ),

        # ---------------------------------------------------------------------
        # Tab 2: Longitudinal Analysis
        # ---------------------------------------------------------------------
        tabPanel(
          "Longitudinal Analysis",
          h3("Patient tracking over time and dropout visualization"),
          p("Kaplan-Meier style curves show retention (1 - dropout) by selected strata."),
          selectInput(
            "km_strata",
            "Stratify retention curve by",
            choices = c("None" = "none", "Age group" = "age_group", "General health" = "health"),
            selected = "health"
          ),
          plotOutput("longitudinal_km", height = "400px"),
          fluidRow(
            column(6, plotlyOutput("longitudinal_timeseries", height = "320px")),
            column(6, plotlyOutput("longitudinal_dropout_by_group", height = "320px"))
          ),
          downloadButton("download_km_png", "Export KM curve (PNG)", class = "btn-secondary"),
          downloadButton("download_km_pdf", "Export KM curve (PDF)", class = "btn-secondary")
        ),

        # ---------------------------------------------------------------------
        # Tab 3: Geographic Distribution
        # ---------------------------------------------------------------------
        tabPanel(
          "Geographic Distribution",
          h3("Spatial analysis of patient intake patterns"),
          p("Points represent patient intake locations (synthetic coordinates). Clustering indicates areas of higher utilization."),
          leafletOutput("geo_map", height = "500px"),
          fluidRow(
            column(6, plotlyOutput("geo_heatmap_attendance", height = "320px")),
            column(6, plotlyOutput("geo_region_summary", height = "320px"))
          )
        ),

        # ---------------------------------------------------------------------
        # Tab 4: Disruption Modeling
        # ---------------------------------------------------------------------
        tabPanel(
          "Disruption Modeling",
          h3("Appointment adherence and healthcare access barriers"),
          p("Analysis of missed appointments and reasons, including access barriers and agoraphobia-related patterns."),
          fluidRow(
            column(6, plotlyOutput("disruption_reasons", height = "360px")),
            column(6, plotlyOutput("disruption_by_health", height = "360px"))
          ),
          fluidRow(
            column(6, plotlyOutput("disruption_timeseries", height = "320px")),
            column(6, plotlyOutput("disruption_attendance_ci", height = "320px"))
          ),
          h4("Statistical summary (attendance by group)"),
          DT::dataTableOutput("disruption_stats_table")
        ),

        # ---------------------------------------------------------------------
        # Tab 5: Reproducibility & Methodology
        # ---------------------------------------------------------------------
        tabPanel(
          "Reproducibility",
          h3("Data synthesis and reproducibility"),
          includeMarkdown("methodology.md"),
          h4("Data generation parameters"),
          verbatimTextOutput("repro_params")
        )
      )
    )
  )
)
