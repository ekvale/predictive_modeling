# Run once to install required packages for the Healthcare Access Dashboard
pkgs <- c(
  "shiny", "tidyverse", "ggplot2", "plotly", "scales", "survival",
  "leaflet", "sf", "lubridate", "DT", "bslib"
)
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
message("Done. Launch with: shiny::runApp() or source('app.R')")
