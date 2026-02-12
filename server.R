# =============================================================================
# Healthcare Access & Continuity Dashboard - Server
# =============================================================================
# Reactive data filtering and all output rendering. Uses filtered patients
# and appointments derived from sidebar inputs.
# =============================================================================

# Geographic region bounds (Minneapolis metro; must match global.R)
lat_mid <- 45.015
lon_mid <- -93.175

server <- function(input, output, session) {

  # Update disruption date range when preset changes
  observeEvent(input$disruption_preset, {
    if (input$disruption_preset == "current") {
      updateDateRangeInput(session, "event_range", start = DEFAULT_EVENT_START, end = DEFAULT_EVENT_END)
    } else if (input$disruption_preset == "covid") {
      updateDateRangeInput(session, "event_range", start = COVID_EVENT_START, end = COVID_EVENT_END)
    }
  })

  # ---------------------------------------------------------------------------
  # Synthetic data (regenerate when disruption period changes)
  # ---------------------------------------------------------------------------
  synth_reactive <- reactive({
    req(input$event_range)
    generate_synthetic_patients(
      n_patients = 1200, seed = 42,
      event_start = as.Date(input$event_range[1]),
      event_end   = as.Date(input$event_range[2])
    )
  })
  patients_reactive    <- reactive(synth_reactive()$patients)
  appointments_reactive <- reactive(synth_reactive()$appointments)

  # ---------------------------------------------------------------------------
  # Filtered data (reactive)
  # ---------------------------------------------------------------------------
  filtered_patients <- reactive({
    req(patients_reactive(), input$date_range, input$region_filter)
    p <- patients_reactive()
    age_sel <- if (length(input$age_groups) == 0) levels(p$age_group) else input$age_groups
    health_sel <- if (length(input$health) == 0) levels(p$health) else input$health
    race_sel <- if (length(input$race_eth) == 0) levels(p$race_eth) else input$race_eth
    out <- p %>%
      filter(
        intake_date >= as.Date(input$date_range[1]),
        intake_date <= as.Date(input$date_range[2]),
        as.character(age_group) %in% age_sel,
        as.character(health) %in% health_sel,
        as.character(race_eth) %in% race_sel
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
    req(appointments_reactive(), filtered_patients())
    appointments_reactive() %>%
      filter(patient_id %in% filtered_patients()$patient_id)
  })

  # Survival data for KM (one row per patient: time_days, event as 0/1 integer for survival package)
  filtered_survival <- reactive({
    req(filtered_patients())
    filtered_patients() %>%
      mutate(
        time_days = as.numeric(difftime(
          pmin(coalesce(dropout_date, as.Date("2026-12-01")), as.Date("2026-12-01")),
          intake_date, units = "days"
        )),
        event = as.integer(!is.na(dropout_date))
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
    pct <- if (is.na(att)) "—" else scales::percent(round(att, 2), accuracy = 0.1)
    bslib::value_box(
      title = "Attendance rate",
      value = pct,
      theme = "success"
    )
  })
  output$vb_dropout_rate <- renderUI({
    p <- filtered_patients() %>% summarise(p = mean(!is.na(dropout_date))) %>% pull(p)
    pct <- if (is.na(p)) "—" else scales::percent(round(p, 2), accuracy = 0.1)
    bslib::value_box(
      title = "Dropout rate",
      value = pct,
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
    ev1 <- as.Date(req(input$event_range[1]))
    ev2 <- as.Date(req(input$event_range[2]))
    x_min <- min(c(d$month, ev1, ev2), na.rm = TRUE)
    x_max <- max(c(d$month, ev1, ev2), na.rm = TRUE)
    p <- ggplot(d, aes(month, rate)) +
      annotate("rect", xmin = ev1, xmax = ev2, ymin = -Inf, ymax = Inf,
               fill = "gray55", alpha = 0.5, inherit.aes = FALSE) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_x_date(limits = c(x_min, x_max), expand = expansion(mult = 0.02)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      labs(x = "Month", y = "Attendance rate", title = "Monthly attendance (gray band = disruption period)")
    fig <- ggplotly(p, tooltip = c("month", "rate", "n"))
    if (!identical(fig$x$data, list())) {
      fig <- fig %>% layout(
        shapes = list(
          list(type = "rect", x0 = as.character(ev1), x1 = as.character(ev2), y0 = 0, y1 = 1,
               yref = "paper", xref = "x", layer = "below",
               fillcolor = "gray", opacity = 0.4, line = list(width = 0))
        )
      )
    }
    fig
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
        attendance_pct = round(100 * mean(attended, na.rm = TRUE), 1),
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
  # Disparity: outcomes by race (for Disparities tab and overview by race)
  outcomes_by_race <- reactive({
    p <- filtered_patients()
    a <- filtered_appointments()
    if (nrow(p) == 0) return(data.frame())
    a %>%
      left_join(p %>% select(patient_id, race_eth), by = "patient_id") %>%
      group_by(race_eth) %>%
      summarise(
        n_patients = n_distinct(patient_id),
        n_appts = n(),
        n_attended = sum(attended, na.rm = TRUE),
        attendance_rate = mean(attended, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        p %>% mutate(dropped = !is.na(dropout_date)) %>%
          group_by(race_eth) %>% summarise(n_dropout = sum(dropped), .groups = "drop"),
        by = "race_eth"
      ) %>%
      mutate(
        n_dropout = replace_na(n_dropout, 0L),
        dropout_rate = n_dropout / n_patients,
        n_missed = n_appts - n_attended
      )
  })
  output$longitudinal_km <- renderPlot({
    df <- filtered_survival()
    if (nrow(df) == 0) return(plot(NULL, main = "No data"))
    km_curve_ggplot(df, group_var = km_strata_var(), title = "Patient retention (time to dropout)")
  }, res = 120)

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
    ev1 <- as.Date(req(input$event_range[1]))
    ev2 <- as.Date(req(input$event_range[2]))
    x_min <- min(c(d$month, ev1, ev2), na.rm = TRUE)
    x_max <- max(c(d$month, ev1, ev2), na.rm = TRUE)
    p <- ggplot(d, aes(month, attendance_rate)) +
      annotate("rect", xmin = ev1, xmax = ev2, ymin = -Inf, ymax = Inf,
               fill = "gray55", alpha = 0.5, inherit.aes = FALSE) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_x_date(limits = c(x_min, x_max), expand = expansion(mult = 0.02)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      labs(x = "Month", y = "Attendance rate", title = "Attendance over time (gray band = disruption period)")
    fig <- ggplotly(p, tooltip = c("month", "attendance_rate", "n_appts"))
    if (!identical(fig$x$data, list())) {
      fig <- fig %>% layout(
        shapes = list(
          list(type = "rect", x0 = as.character(ev1), x1 = as.character(ev2), y0 = 0, y1 = 1,
               yref = "paper", xref = "x", layer = "below",
               fillcolor = "gray", opacity = 0.4, line = list(width = 0))
        )
      )
    }
    fig
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
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
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
    content = function(file) {
      p <- km_plot_obj()
      if (inherits(p, "ggplot") && length(p$layers) > 0) ggsave(file, plot = p, device = "png", width = 8, height = 5, dpi = 150)
    }
  )
  output$download_km_pdf <- downloadHandler(
    filename = "kaplan_meier_retention.pdf",
    content = function(file) {
      p <- km_plot_obj()
      if (inherits(p, "ggplot") && length(p$layers) > 0) ggsave(file, plot = p, device = "pdf", width = 8, height = 5)
    }
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
        popup = ~paste0("Patient: ", patient_id, "<br>Intake: ", intake_date, "<br>Health: ", health, "<br>Race/ethnicity: ", race_eth)
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
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
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
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      labs(x = "General health", y = "Attendance rate", title = "Attendance by health status")
    ggplotly(p, tooltip = c("health", "rate", "n"))
  })

  output$disruption_timeseries <- renderPlotly({
    d <- long_timeseries_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(month, attendance_rate)) +
      geom_line(color = cb_palette[2], linewidth = 1) +
      geom_point(color = cb_palette[2], size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
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
      ) %>%
      mutate(rate = round(rate, 2), ci_low = round(ci_low, 2), ci_high = round(ci_high, 2))
  })
  output$disruption_attendance_ci <- renderPlotly({
    d <- disruption_ci_data()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(health, rate, fill = health)) +
      geom_col(show.legend = FALSE) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, linewidth = 0.8) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      labs(x = "General health", y = "Attendance rate (95% CI)", title = "Attendance with 95% confidence intervals")
    ggplotly(p, tooltip = c("health", "rate", "ci_low", "ci_high", "n"))
  })

  output$disruption_stats_table <- DT::renderDataTable({
    disruption_ci_data()
  }, options = list(pageLength = 10), rownames = FALSE)

  # ---------------------------------------------------------------------------
  # Disparities (by race/ethnicity)
  # ---------------------------------------------------------------------------
  output$disparity_attendance_race <- renderPlotly({
    d <- outcomes_by_race()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(race_eth, attendance_rate, fill = race_eth)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = mean(filtered_appointments()$attended, na.rm = TRUE), linetype = 2, linewidth = 0.8) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      coord_flip() +
      labs(x = NULL, y = "Attendance rate", title = "Attendance by race/ethnicity (dashed = overall)")
    ggplotly(p, tooltip = c("race_eth", "attendance_rate", "n_appts", "n_patients"))
  })
  output$disparity_dropout_race <- renderPlotly({
    d <- outcomes_by_race()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(race_eth, dropout_rate, fill = race_eth)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = mean(!is.na(filtered_patients()$dropout_date)), linetype = 2, linewidth = 0.8) +
      scale_fill_manual(values = cb_palette) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
      coord_flip() +
      labs(x = NULL, y = "Dropout rate", title = "Dropout by race/ethnicity (dashed = overall)")
    ggplotly(p, tooltip = c("race_eth", "dropout_rate", "n_dropout", "n_patients"))
  })
  disparity_table_data <- reactive({
    d <- outcomes_by_race()
    if (nrow(d) == 0) return(data.frame())
    ref_rate <- mean(filtered_appointments()$attended, na.rm = TRUE)
    d %>%
      mutate(
        attendance_pct = round(100 * attendance_rate, 1),
        dropout_pct = round(100 * dropout_rate, 1),
        disparity_ratio = round(attendance_rate / ref_rate, 2)
      ) %>%
      select(race_eth, n_patients, n_appts, n_missed, attendance_pct, dropout_pct, disparity_ratio)
  })
  output$disparity_table <- DT::renderDataTable({
    disparity_table_data()
  }, options = list(pageLength = 10), rownames = FALSE)

  # ---------------------------------------------------------------------------
  # Cost & Health Impact
  # ---------------------------------------------------------------------------
  cost_impact_data <- reactive({
    p <- filtered_patients()
    a <- filtered_appointments()
    n_missed <- sum(!a$attended, na.rm = TRUE)
    n_dropout <- sum(!is.na(p$dropout_date))
    cpm <- if (is.null(input$cost_per_missed) || is.na(input$cost_per_missed)) 0 else input$cost_per_missed
    cpd <- if (is.null(input$cost_per_dropout) || is.na(input$cost_per_dropout)) 0 else input$cost_per_dropout
    cost_missed <- cpm * n_missed
    cost_dropout <- cpd * n_dropout
    total_cost <- cost_missed + cost_dropout
    n_patients <- nrow(p)
    list(
      n_patients = n_patients,
      n_missed = n_missed,
      n_dropout = n_dropout,
      cost_per_missed = cpm,
      cost_per_dropout = cpd,
      cost_missed = cost_missed,
      cost_dropout = cost_dropout,
      total_cost = total_cost,
      missed_per_1k = if (n_patients > 0) round(1000 * n_missed / n_patients, 1) else 0,
      dropout_per_1k = if (n_patients > 0) round(1000 * n_dropout / n_patients, 1) else 0
    )
  })
  output$cost_summary <- renderUI({
    x <- cost_impact_data()
    fluidRow(
      column(4, bslib::value_box(title = "Missed appointments", value = format(x$n_missed, big.mark = ","), theme = "warning")),
      column(4, bslib::value_box(title = "Patients lost to follow-up", value = format(x$n_dropout, big.mark = ","), theme = "danger")),
      column(4, bslib::value_box(title = "Total estimated cost", value = paste0("$", format(round(x$total_cost, 0), big.mark = ",")), theme = "primary"))
    )
  })
  output$health_impact_summary <- renderUI({
    x <- cost_impact_data()
    tagList(
      p(strong("Health burden (filtered population):")),
      p("Missed care episodes (missed appointments): ", format(x$n_missed, big.mark = ",")),
      p("Missed appointments per 1,000 patients: ", x$missed_per_1k),
      p("Patients lost to follow-up: ", format(x$n_dropout, big.mark = ",")),
      p("Dropouts per 1,000 patients: ", x$dropout_per_1k)
    )
  })
  output$cost_breakdown_table <- DT::renderDataTable({
    x <- cost_impact_data()
    tibble(
      Component = c("Missed appointments", "Patients lost to follow-up", "Total"),
      Count = c(format(x$n_missed, big.mark = ","), format(x$n_dropout, big.mark = ","), ""),
      `Unit cost ($)` = c(format(round(x$cost_per_missed, 0), big.mark = ","), format(round(x$cost_per_dropout, 0), big.mark = ","), ""),
      `Subtotal ($)` = c(format(round(x$cost_missed, 0), big.mark = ","), format(round(x$cost_dropout, 0), big.mark = ","), format(round(x$total_cost, 0), big.mark = ","))
    )
  }, options = list(paging = FALSE), rownames = FALSE)

  # ---------------------------------------------------------------------------
  # Tell the Story (presentation tab: narrative + Sankey flows)
  # ---------------------------------------------------------------------------
  output$story_headline <- renderUI({
    p <- filtered_patients()
    a <- filtered_appointments()
    n_p <- nrow(p)
    n_a <- nrow(a)
    if (n_p == 0 || n_a == 0) return(p(em("No data for current filters.")))
    att_rate <- round(100 * mean(a$attended, na.rm = TRUE), 1)
    drop_rate <- round(100 * mean(!is.na(p$dropout_date)), 1)
    n_missed <- sum(!a$attended, na.rm = TRUE)
    div(
      class = "well",
      h4("In this population:", class = "text-primary"),
      p(strong(format(n_p, big.mark = ",")), " patients, ", strong(format(n_a, big.mark = ",")), " appointments. ",
        "Attendance rate ", strong(paste0(att_rate, "%")), " — ",
        strong(format(n_missed, big.mark = ",")), " missed. ",
        "Dropout rate ", strong(paste0(drop_rate, "%")), "."),
      p(em("Use the flow charts below to show who is affected and why."))
    )
  })

  # Sankey 1: Race → Attended / Missed (appointments)
  sankey_attendance_data <- reactive({
    a <- filtered_appointments() %>%
      left_join(filtered_patients() %>% select(patient_id, race_eth), by = "patient_id")
    if (nrow(a) == 0) return(list(nodes = character(0), source = integer(0), target = integer(0), value = numeric(0)))
    d <- a %>% count(race_eth, attended, name = "n") %>% filter(n > 0)
    races <- sort(unique(d$race_eth))
    nodes <- c(as.character(races), "Attended", "Missed")
    n_r <- length(races)
    idx_att <- n_r
    idx_miss <- n_r + 1L
    source <- integer(0)
    target <- integer(0)
    value <- numeric(0)
    for (i in seq_along(races)) {
      r <- races[i]
      n_att <- d %>% filter(race_eth == r, attended) %>% pull(n) %>% sum()
      n_mis <- d %>% filter(race_eth == r, !attended) %>% pull(n) %>% sum()
      if (n_att > 0) { source <- c(source, i - 1L); target <- c(target, idx_att); value <- c(value, n_att) }
      if (n_mis > 0) { source <- c(source, i - 1L); target <- c(target, idx_miss); value <- c(value, n_mis) }
    }
    list(nodes = nodes, source = source, target = target, value = value, n_race = n_r)
  })
  output$sankey_attendance <- renderPlotly({
    sk <- sankey_attendance_data()
    if (length(sk$nodes) == 0 || sum(sk$value) == 0) return(plotly_empty())
    n_r <- sk$n_race
    node_colors <- c(cb_palette[seq_len(n_r)], "#009E73", "#D55E00")
    node_colors <- rep(node_colors, length.out = length(sk$nodes))
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = sk$nodes,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "gray90", width = 0.5)
      ),
      link = list(
        source = sk$source,
        target = sk$target,
        value = sk$value
      )
    ) %>%
      layout(
        title = list(text = "Appointments: Race/ethnicity → Attended vs Missed", font = list(size = 14)),
        font = list(size = 12),
        margin = list(l = 20, r = 20, t = 40, b = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  # Sankey 2: Missed appointments → Reason
  sankey_reasons_data <- reactive({
    a <- filtered_appointments() %>% filter(!attended, !is.na(missed_reason))
    if (nrow(a) == 0) return(list(nodes = character(0), source = integer(0), target = integer(0), value = numeric(0)))
    d <- a %>% count(missed_reason, name = "n") %>% filter(n > 0)
    reasons <- as.character(d$missed_reason)
    nodes <- c("Missed", reasons)
    source <- rep(0L, length(reasons))
    target <- seq_len(length(reasons))
    value <- d$n
    list(nodes = nodes, source = source, target = target, value = value)
  })
  output$sankey_missed_reasons <- renderPlotly({
    sk <- sankey_reasons_data()
    if (length(sk$nodes) == 0 || sum(sk$value) == 0) return(plotly_empty())
    node_colors <- c("#D55E00", cb_palette[seq_len(length(sk$nodes) - 1)])
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = sk$nodes,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "gray90", width = 0.5)
      ),
      link = list(source = sk$source, target = sk$target, value = sk$value)
    ) %>%
      layout(
        title = list(text = "Missed appointments → Reason", font = list(size = 14)),
        font = list(size = 12),
        margin = list(l = 20, r = 20, t = 40, b = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  # Sankey 3: Race → Retained in care / Dropped out
  sankey_retention_data <- reactive({
    p <- filtered_patients()
    if (nrow(p) == 0) return(list(nodes = character(0), source = integer(0), target = integer(0), value = numeric(0)))
    p <- p %>% mutate(dropped = !is.na(dropout_date)) %>% count(race_eth, dropped, name = "n") %>% filter(n > 0)
    races <- sort(unique(p$race_eth))
    nodes <- c(as.character(races), "Retained in care", "Dropped out")
    n_r <- length(races)
    idx_ret <- n_r
    idx_drop <- n_r + 1L
    source <- integer(0)
    target <- integer(0)
    value <- numeric(0)
    for (i in seq_along(races)) {
      r <- races[i]
      n_ret <- p %>% filter(race_eth == r, !dropped) %>% pull(n) %>% sum()
      n_drop <- p %>% filter(race_eth == r, dropped) %>% pull(n) %>% sum()
      if (n_ret > 0) { source <- c(source, i - 1L); target <- c(target, idx_ret); value <- c(value, n_ret) }
      if (n_drop > 0) { source <- c(source, i - 1L); target <- c(target, idx_drop); value <- c(value, n_drop) }
    }
    list(nodes = nodes, source = source, target = target, value = value, n_race = n_r)
  })
  output$sankey_retention <- renderPlotly({
    sk <- sankey_retention_data()
    if (length(sk$nodes) == 0 || sum(sk$value) == 0) return(plotly_empty())
    n_r <- sk$n_race
    node_colors <- c(cb_palette[seq_len(n_r)], "#009E73", "#D55E00")
    node_colors <- rep(node_colors, length.out = length(sk$nodes))
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = sk$nodes,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "gray90", width = 0.5)
      ),
      link = list(source = sk$source, target = sk$target, value = sk$value)
    ) %>%
      layout(
        title = list(text = "Patients: Race/ethnicity → Retained in care vs Dropped out", font = list(size = 14)),
        font = list(size = 12),
        margin = list(l = 20, r = 20, t = 40, b = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  # ---------------------------------------------------------------------------
  # Reproducibility
  # ---------------------------------------------------------------------------
  output$repro_params <- renderPrint({
    list(
      n_patients = 1200L,
      seed = 42L,
      location = "Minneapolis–Saint Paul metro (synthetic coordinates)",
      intake_start = "2020-01-01",
      intake_end = "2024-06-30",
      follow_up_end = "2026-12-01",
      disruption_period = paste(as.character(input$event_range[1]), "to", as.character(input$event_range[2])),
      call = "generate_synthetic_patients(n_patients = 1200, seed = 42, event_start = ..., event_end = ...)"
    )
  })
}
