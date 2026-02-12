# =============================================================================
# Healthcare Access & Continuity Dashboard - Launcher
# =============================================================================
# Run this file to launch the app (e.g. Run App in RStudio, or source("app.R")).
# Alternatively: setwd("path/to/predictive_modeling"); shiny::runApp()
# =============================================================================

source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)
