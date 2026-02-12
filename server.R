# =============================================================================
# Healthcare Access & Continuity Dashboard - Server
# =============================================================================
# Reactive data filtering and all output rendering. Uses filtered patients
# and appointments derived from sidebar inputs.
# =============================================================================

# Geographic region bounds (must match global.R synthetic area)
lat_mid <- 39.85
lon_mid <- -75.15

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Filtered data (reactive)
  # ---------------------------------------------------------------------------
  filtered_patients <- reactive({
    req(patients_df, input$date_range, input$region_filter)
    # Use all levels if none selected (avoid empty result from empty checkbox)
    age_sel <- if (length(input$age_groups) == 0) levels(patients_df$age_group) else input$age_groups
    health_sel <- if (length(input$health) == 0) levels(patients_df$health) else input$health
    out <- patients_df %>%
      filter(
        intake_date >= as.Date(input$date_range[1]),
        intake_date <= as.Date(input$date_range[2]),
        as.character(age_group) %in% age_sel,
        as.character(health) %in% health_sel
      )
    # Region filter (North/South/East/West by lat/lon)
    if (input$region_filter != "All regions") {
      switch(
        input$region_filter,
        North = out <- out %>% filter(latitude >= lat_mid),
        South = out <- out %>% filter(latitude < lat_mid),
        East  = out <- out %>% filter(longitude >= lon_mid),
        West  = out <- out %>% filter(longitude < lon_mid)
      )
    }
    out
  })

  filtered_appointments <- reactive({
    req(appointments_df, filtered_patients())
    appointments_df %>%
      filter(patient_id %in% filtered_patients()$patient_id)
  })

  # Survival data for KM (one row per patient: time_days, event)
  filtered_survival <- reactive({
    req(filtered_patients())
    filtered_patients() %>%
      mutate(
        time_days = as.numeric(difftime(
          pmin(coalesce(dropout_date, as.Date("2024-12-01")), as.Date("2024-12-01")),
          intake_date, units = "days"
        )),
        event = !is.na(dropout_date)
      )
  })

  # ---------------------------------------------------------------------------
  # Overview Dashboard
  # ---------------------------------------------------------------------------
  output$vb_patients <- renderUI({
    n <- nrow(filtered_patients())
    bslib::value_box(
      title = "Patients",
      value = format(n, big.mark = ","),
      theme = "primary"
    )
  })
  output$vb_appointments <- renderUI({
    n <- nrow(filtered_appointments())
    bslib::value_box(
      title = "Appointments",
      value = format(n, big.mark = ","),
      theme = "info"
    )
  })
  output$vb_attendance_rate <- renderUI({
    att <- filtered_appointments() %>% summarise(p = mean(attended, na.rm = TRUE)) %>% pull(p)
    bslib::value_box(
      title = "Attendance rate",
      value = scales::percent(att, accuracy = 0.1),
      theme = "success"
    )
  })
  output$vb_dropout_rate <- renderUI({
    p <- filtered_patients() %>% summarise(p = mean(!is.na(dropout_date))) %>% pull(p)
    bslib::value_box(
      title = "Dropout rate",
      value = scales::percent(p, accuracy = 0.1),
      theme = "warning"
    )
  })

  # Attendance trend over time (monthly)
  overview_trend_data <- reactive({
    filtered_appointments() %>%
      mutate(month = lubridate::floor_date(appointment_date, "month")) %>%
      group_by(month) %>%
      summarise(
        n = n(),
        attended = sum(attended, na.rm = TRUE),
        rate = attended / n(),
        .groups = "drop"
      )
  })
  output$overview_attendance_trend <- renderPlotly({
    d <- overview_trend_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(month, rate)) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "Month", y = "Attendance rate", title = "Monthly appointment attendance")
    ggplotly(p, tooltip = c("month", "rate", "n"))
  })

  # Missed appointment reasons
  overview_reasons_data <- reactive({
    filtered_appointments() %>%
      filter(!attended, !is.na(missed_reason)) %>%
      count(missed_reason, name = "count") %>%
      mutate(missed_reason = forcats::fct_reorder(as.character(missed_reason), count))
  })
  output$overview_missed_reasons <- renderPlotly({
    d <- overview_reasons_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(missed_reason, count, fill = missed_reason)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = cb_palette) +
      coord_flip() +
      labs(x = NULL, y = "Count", title = "Missed appointments by reason")
    ggplotly(p, tooltip = "count")
  })

  # Summary table (descriptive stats)
  overview_table_data <- reactive({
    p <- filtered_patients()
    a <- filtered_appointments()
    if (nrow(p) == 0) return(data.frame())
    by_health <- a %>%
      left_join(p %>% select(patient_id, health), by = "patient_id") %>%
      group_by(health) %>%
      summarise(
        appointments = n(),
        attended = sum(attended, na.rm = TRUE),
        attendance_pct = 100 * mean(attended, na.rm = TRUE),
        .groups = "drop"
      )
    by_health
  })
  output$overview_summary_table <- DT::renderDataTable({
    overview_table_data()
  }, options = list(pageLength = 10), rownames = FALSE)

  # ---------------------------------------------------------------------------
  # Longitudinal Analysis
  # ---------------------------------------------------------------------------
  km_strata_var <- reactive({
    if (is.null(input$km_strata) || input$km_strata == "none") NULL else input$km_strata
  })
  output$longitudinal_km <- renderPlotly({
    df <- filtered_survival()
    if (nrow(df) == 0) return(plotly_empty())
    gg <- km_curve_ggplot(df, group_var = km_strata_var(), title = "Patient retention (time to dropout)")
    ggplotly(gg, tooltip = c("time", "surv", "strata"))
  })

  # Time series: dropout rate and attendance over time
  long_timeseries_data <- reactive({
    p <- filtered_patients()
    a <- filtered_appointments()
    if (nrow(p) == 0) return(data.frame())
    a %>%
      left_join(p %>% select(patient_id, intake_date), by = "patient_id") %>%
      mutate(month = lubridate::floor_date(appointment_date, "month")) %>%
      group_by(month) %>%
      summarise(
        attendance_rate = mean(attended, na.rm = TRUE),
        n_appts = n(),
        .groups = "drop"
      )
  })
  output$longitudinal_timeseries <- renderPlotly({
    d <- long_timeseries_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(month, attendance_rate)) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "Month", y = "Attendance rate", title = "Attendance over time")
    ggplotly(p, tooltip = c("month", "attendance_rate", "n_appts"))
  })

  # Dropout rate by group (e.g. health)
  long_dropout_data <- reactive({
    filtered_patients() %>%
      mutate(dropped = !is.na(dropout_date)) %>%
      count(health, dropped) %>%
      group_by(health) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      filter(dropped)
  })
  output$longitudinal_dropout_by_group <- renderPlotly({
    d <- long_dropout_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(health, pct, fill = health)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "General health", y = "Dropout rate", title = "Dropout rate by health status")
    ggplotly(p, tooltip = c("health", "pct", "n"))
  })

  # Export KM curve
  km_plot_obj <- reactive({
    df <- filtered_survival()
    if (nrow(df) == 0) return(ggplot())
    km_curve_ggplot(df, group_var = km_strata_var(), title = "Patient retention (time to dropout)")
  })
  output$download_km_png <- downloadHandler(
    filename = "kaplan_meier_retention.png",
    content = function(file) ggsave(file, plot = km_plot_obj(), device = "png", width = 8, height = 5, dpi = 150)
  )
  output$download_km_pdf <- downloadHandler(
    filename = "kaplan_meier_retention.pdf",
    content = function(file) ggsave(file, plot = km_plot_obj(), device = "pdf", width = 8, height = 5)
  )

  # ---------------------------------------------------------------------------
  # Geographic Distribution
  # ---------------------------------------------------------------------------
  output$geo_map <- renderLeaflet({
    p <- filtered_patients()
    if (nrow(p) == 0) {
      m <- leaflet() %>% addTiles() %>% setView(lon_mid, lat_mid, zoom = 9)
      return(m)
    }
    leaflet(p) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 4, opacity = 0.6, color = cb_palette[2],
        popup = ~paste0("Patient: ", patient_id, "<br>Intake: ", intake_date, "<br>Health: ", health)
      ) %>%
      setView(lng = mean(p$longitude), lat = mean(p$latitude), zoom = 9)
  })

  # Heatmap-style: attendance by region (quadrant)
  geo_heatmap_data <- reactive({
    filtered_appointments() %>%
      left_join(filtered_patients() %>% select(patient_id, latitude, longitude), by = "patient_id") %>%
      mutate(
        lat_region = if_else(latitude >= lat_mid, "North", "South"),
        lon_region = if_else(longitude >= lon_mid, "East", "West"),
        region = paste(lat_region, lon_region)
      ) %>%
      group_by(region) %>%
      summarise(attendance_rate = mean(attended, na.rm = TRUE), n = n(), .groups = "drop")
  })
  output$geo_heatmap_attendance <- renderPlotly({
    d <- geo_heatmap_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(region, attendance_rate, fill = region)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      coord_flip() +
      labs(x = "Region", y = "Attendance rate", title = "Attendance by geographic quadrant")
    ggplotly(p, tooltip = c("region", "attendance_rate", "n"))
  })

  output$geo_region_summary <- renderPlotly({
    d <- geo_heatmap_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(region, n, fill = region)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = cb_palette) +
      coord_flip() +
      labs(x = "Region", y = "Appointments", title = "Appointment volume by region")
    ggplotly(p, tooltip = c("region", "n"))
  })

  # ---------------------------------------------------------------------------
  # Disruption Modeling
  # ---------------------------------------------------------------------------
  disruption_reasons_data <- reactive({
    filtered_appointments() %>%
      filter(!attended, !is.na(missed_reason)) %>%
      count(missed_reason, name = "count") %>%
      mutate(missed_reason = forcats::fct_reorder(as.character(missed_reason), count))
  })
  output$disruption_reasons <- renderPlotly({
    d <- disruption_reasons_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(missed_reason, count, fill = missed_reason)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = cb_palette) +
      coord_flip() +
      labs(x = NULL, y = "Count", title = "Missed appointments by reason (access barriers)")
    ggplotly(p, tooltip = "count")
  })

  disruption_by_health_data <- reactive({
    filtered_appointments() %>%
      left_join(filtered_patients() %>% select(patient_id, health), by = "patient_id") %>%
      group_by(health) %>%
      summarise(
        n = n(),
        attended = sum(attended, na.rm = TRUE),
        rate = mean(attended, na.rm = TRUE),
        .groups = "drop"
      )
  })
  output$disruption_by_health <- renderPlotly({
    d <- disruption_by_health_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(health, rate, fill = health)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = mean(filtered_appointments()$attended, na.rm = TRUE), linetype = 2) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "General health", y = "Attendance rate", title = "Attendance by health status")
    ggplotly(p, tooltip = c("health", "rate", "n"))
  })

  output$disruption_timeseries <- renderPlotly({
    d <- long_timeseries_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(month, attendance_rate)) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "Month", y = "Attendance rate", title = "Attendance trend over time")
    ggplotly(p, tooltip = c("month", "attendance_rate"))
  })

  # Attendance with CI by group
  disruption_ci_data <- reactive({
    filtered_appointments() %>%
      left_join(filtered_patients() %>% select(patient_id, health), by = "patient_id") %>%
      group_by(health) %>%
      summarise(
        n = n(),
        rate = mean(attended, na.rm = TRUE),
        se = sqrt(rate * (1 - rate) / n),
        ci_low = pmax(0, rate - 1.96 * se),
        ci_high = pmin(1, rate + 1.96 * se),
        .groups = "drop"
      )
  })
  output$disruption_attendance_ci <- renderPlotly({
    d <- disruption_ci_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(health, rate, fill = health)) +
      geom_col(show.legend = FALSE) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, linewidth = 0.8) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      labs(x = "General health", y = "Attendance rate (95% CI)", title = "Attendance with 95% confidence intervals")
    ggplotly(p, tooltip = c("health", "rate", "ci_low", "ci_high", "n"))
  })

  output$disruption_stats_table <- DT::renderDataTable({
    disruption_ci_data()
  }, options = list(pageLength = 10), rownames = FALSE)

  # ---------------------------------------------------------------------------
  # Reproducibility
  # ---------------------------------------------------------------------------
  output$repro_params <- renderPrint({
    list(
      n_patients = 1200L,
      seed = 42L,
      intake_start = "2022-01-01",
      intake_end = "2024-06-30",
      follow_up_end = "2024-12-01",
      call = "generate_synthetic_patients(n_patients = 1200, seed = 42)"
    )
  })
}
