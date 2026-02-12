# Healthcare Access & Continuity Dashboard

A multi-tab **R Shiny** application for analyzing and visualizing synthetic healthcare data, with a focus on patient intake patterns, longitudinal tracking, and dropout modeling. Designed for presentation at the r/medicine conference.

## Features

- **Overview Dashboard**: Summary statistics (patient count, appointments, attendance rate, dropout rate), monthly attendance trend, missed-appointment reasons, and summary table by health status.
- **Longitudinal Analysis**: Kaplan–Meier–style retention curves (time to dropout), stratified by age or health; time series of attendance; dropout rate by group; export of KM curve (PNG/PDF).
- **Geographic Distribution**: Interactive Leaflet map of patient intake locations; attendance and volume by geographic quadrant.
- **Disruption Modeling**: Missed-appointment reasons (including access barriers and agoraphobia), attendance by health status, time series, and attendance rates with 95% confidence intervals.
- **Reproducibility**: Methodology note and data generation parameters (seed, sample size) for full reproducibility.

## Technical Stack

- **R** (tidyverse, dplyr, lubridate)
- **Shiny** (bslib for theme)
- **ggplot2** and **plotly** for static and interactive charts
- **leaflet** and **sf** for mapping
- **survival** for Kaplan–Meier curves
- **DT** for interactive tables

## Data

All data are **synthetic** and generated in-app (see `global.R`). No real patient data are used. The data model includes:

- Patient ID (anonymized), intake date, latitude/longitude, age group, general health, dropout date (if applicable).
- Appointment-level: patient ID, appointment date, attended (TRUE/FALSE), missed reason (e.g. access_barrier, agoraphobia, transportation, scheduling, other).

## Installation and Run

1. Install R (and optionally RStudio).
2. Install required packages (run once):

```r
install.packages(c(
  "shiny", "tidyverse", "ggplot2", "plotly", "scales", "survival",
  "leaflet", "sf", "lubridate", "DT", "bslib"
))
```

3. Launch the app:

```r
setwd("path/to/predictive_modeling")  # or your project path
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

## Reproducibility

- Synthetic data are generated with a fixed seed (42) and 1,200 patients by default.
- Parameters and the generation function are documented in the **Reproducibility** tab and in `global.R` (`generate_synthetic_patients()`).

## Code Structure

- **global.R**: Libraries, options, synthetic data generation, survival/KM helpers, shared palette.
- **ui.R**: Multi-tab UI, sidebar filters, outputs per tab.
- **server.R**: Reactive filtered data and all plot/table rendering.
- **app.R**: Sources global, ui, server and launches `shinyApp(ui, server)`.
- **methodology.md**: Short methodology note (synthetic data, analyses, intended use).

## Success Criteria (from spec)

- Load and render within a few seconds.
- Handles 1,200+ synthetic patient records and thousands of appointments.
- Publication-style, color-blind friendly visualizations.
- Clear for medical professionals; demonstrates patterns in access and continuity.

## License and Disclaimer

For educational and conference use only. Data are synthetic; no real patient information is included.
