# =============================================================================
# Healthcare Access & Continuity Dashboard - Global Environment
# =============================================================================
# Loaded before ui.R and server.R. Contains:
#   - Package loading and options
#   - Synthetic data generation (reproducible)
#   - Reusable visualization and analysis helpers
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LIBRARIES AND SETUP
# -----------------------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(survival)   # Kaplan-Meier curves
library(leaflet)    # Interactive maps
library(sf)         # Simple features for spatial data
library(lubridate)  # Date handling
library(DT)         # Interactive tables (optional)

# Color-blind friendly palette (Okabe-Ito inspired + viridis for gradients)
# Suitable for academic presentation and projectors
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#999999"
)
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", hjust = 0.5)))

# -----------------------------------------------------------------------------
# 2. SYNTHETIC DATA GENERATION
# -----------------------------------------------------------------------------
# Generates HIPAA-compliant synthetic patient intake and appointment data.
# Clearly artificial; no real patient data. Reproducible via set.seed.

generate_synthetic_patients <- function(n_patients = 1200, seed = 42) {
  set.seed(seed)
  # Base date range for intake (e.g., 2 years of intake)
  start_intake <- as.Date("2022-01-01")
  end_intake   <- as.Date("2024-06-30")
  intake_dates <- sample(seq(start_intake, end_intake, by = "day"), n_patients, replace = TRUE)

  # Geographic coordinates (synthetic metro area: lat/lon bounds)
  # Represent a fictional region for mapping
  lat_range <- c(39.5, 40.2)
  lon_range <- c(-75.5, -74.8)
  latitude  <- runif(n_patients, lat_range[1], lat_range[2])
  longitude <- runif(n_patients, lon_range[1], lon_range[2])

  # Demographics
  age_groups <- sample(c("18-29", "30-44", "45-59", "60+"),
                       n_patients, replace = TRUE, prob = c(0.2, 0.35, 0.3, 0.15))
  health_indicator <- sample(c("Good", "Fair", "Poor"), n_patients, replace = TRUE,
                             prob = c(0.5, 0.35, 0.15))

  # Dropout: some patients "drop out" (no longer in care) after a period
  # Time to dropout in days (NA = still active)
  max_follow_up_days <- as.numeric(difftime(as.Date("2024-12-01"), start_intake), units = "days")
  dropout_rate_by_health <- c(Good = 0.15, Fair = 0.25, Poor = 0.45)
  p_dropout <- dropout_rate_by_health[health_indicator]
  will_dropout <- runif(n_patients) < p_dropout
  days_to_dropout <- ifelse(will_dropout,
                            pmin(rweibull(n_patients, shape = 1.2, scale = 180), max_follow_up_days),
                            NA_real_)
  intake_numeric <- as.numeric(intake_dates - start_intake)
  dropout_date <- if_else(!is.na(days_to_dropout),
                          start_intake + days_to_dropout,
                          as.Date(NA_character_))

  # Build patient-level dataset
  patients <- tibble(
    patient_id   = paste0("P", str_pad(seq_len(n_patients), width = 5, pad = "0")),
    intake_date  = intake_dates,
    latitude     = latitude,
    longitude    = longitude,
    age_group    = factor(age_groups, levels = c("18-29", "30-44", "45-59", "60+")),
    health       = factor(health_indicator, levels = c("Good", "Fair", "Poor")),
    dropout_date = dropout_date
  )

  # Appointment-level data: each patient has multiple appointments
  # attended = TRUE/FALSE, missed_reason when not attended
  missed_reasons <- c("access_barrier", "agoraphobia", "transportation", "scheduling", "other")
  n_appts_per_patient <- 1 + rpois(n_patients, 4)
  appointment_rows <- rep(seq_len(n_patients), n_appts_per_patient)
  n_appts <- length(appointment_rows)

  appt_patient_id <- patients$patient_id[appointment_rows]
  appt_intake     <- patients$intake_date[appointment_rows]
  appt_dropout    <- patients$dropout_date[appointment_rows]

  # Appointment date: between intake and dropout (or end of follow-up)
  end_follow <- as.Date("2024-12-01")
  max_appt_date <- pmin(coalesce(appt_dropout, end_follow), end_follow)
  min_appt_date <- appt_intake
  span <- as.numeric(max_appt_date - min_appt_date)
  span <- pmax(span, 1)
  appt_dates <- min_appt_date + runif(n_appts, 0, span) * 0.9
  appt_dates <- as.Date(appt_dates, origin = "1970-01-01")

  # Attendance: higher miss rate for "agoraphobia" and "access_barrier" segments
  # (synthetic association for demonstration)
  health_miss_mult <- c(Good = 0.8, Fair = 1.2, Poor = 1.5)
  base_miss <- 0.2
  p_miss <- pmin(0.85, base_miss * health_miss_mult[patients$health[appointment_rows]])
  p_miss <- p_miss * (0.7 + 0.3 * runif(n_appts))  # add noise
  attended <- runif(n_appts) > p_miss

  # Assign reason when missed (weighted toward access/agoraphobia for demo)
  reason_weights <- c(access_barrier = 0.25, agoraphobia = 0.2, transportation = 0.2,
                     scheduling = 0.2, other = 0.15)
  missed_reason <- rep(NA_character_, n_appts)
  missed_reason[!attended] <- sample(names(reason_weights), sum(!attended), replace = TRUE, prob = reason_weights)

  appointments <- tibble(
    patient_id    = appt_patient_id,
    appointment_date = appt_dates,
    attended      = attended,
    missed_reason = factor(missed_reason, levels = missed_reasons)
  ) %>%
    arrange(patient_id, appointment_date)

  list(patients = patients, appointments = appointments)
}

# Generate once at app load (reproducible)
synth <- generate_synthetic_patients(n_patients = 1200, seed = 42)
patients_df    <- synth$patients
appointments_df <- synth$appointments

# Combined long-format for time-to-dropout (one row per patient with time and event)
survival_df <- patients_df %>%
  mutate(
    time_days = as.numeric(difftime(pmin(coalesce(dropout_date, as.Date("2024-12-01")), as.Date("2024-12-01")), intake_date, units = "days")),
    event     = !is.na(dropout_date)
  )

# -----------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Build data frame from survfit for ggplot (no extra packages)
survfit_to_df <- function(fit) {
  if (is.null(fit$strata)) {
    return(tibble(
      time = c(0, fit$time),
      surv = c(1, fit$surv),
      lower = c(1, fit$lower),
      upper = c(1, fit$upper),
      strata = "Overall"
    ))
  }
  lens <- fit$strata
  names(lens) <- gsub(".*=", "", names(lens))
  ends <- cumsum(lens)
  starts <- c(1, ends[-length(ends)] + 1)
  out <- map_dfr(seq_along(lens), function(i) {
    idx <- seq(starts[i], ends[i])
    tibble(
      time = c(0, fit$time[idx]),
      surv = c(1, fit$surv[idx]),
      lower = c(1, fit$lower[idx]),
      upper = c(1, fit$upper[idx]),
      strata = names(lens)[i]
    )
  })
  out
}

# Kaplan-Meier curve as ggplot (patient retention / time to dropout)
km_curve_ggplot <- function(df, group_var = NULL, title = "Patient retention (time to dropout)") {
  if (is.null(group_var)) {
    fit <- survfit(Surv(time_days, event) ~ 1, data = df)
    f <- survfit_to_df(fit)
    p <- ggplot(f, aes(time, surv)) +
      geom_step(aes(color = "Overall"), linewidth = 1.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = cb_palette[1]) +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      labs(x = "Days since intake", y = "Retention probability", title = title, color = NULL) +
      scale_color_manual(values = cb_palette[1]) +
      theme(legend.position = "bottom")
    return(p)
  }
  group_var <- sym(group_var)
  fit <- survfit(Surv(time_days, event) ~ !!group_var, data = df)
  f <- survfit_to_df(fit)
  p <- ggplot(f, aes(time, surv, color = strata)) +
    geom_step(linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata), alpha = 0.15, colour = NA) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(x = "Days since intake", y = "Retention probability", title = title, color = NULL, fill = NULL) +
    scale_color_manual(values = cb_palette, labels = function(x) x) +
    scale_fill_manual(values = cb_palette, labels = function(x) x) +
    theme(legend.position = "bottom")
  p
}

# Summary statistics with optional confidence interval (Wilson for proportions)
summary_stats <- function(tbl, value_var, group_var = NULL) {
  value_var <- sym(value_var)
  if (is.null(group_var)) {
    tbl %>%
      summarise(
        n = n(),
        mean = mean(!!value_var, na.rm = TRUE),
        sd = sd(!!value_var, na.rm = TRUE),
        ci_low = mean(!!value_var, na.rm = TRUE) - 1.96 * sd(!!value_var, na.rm = TRUE) / sqrt(n()),
        ci_high = mean(!!value_var, na.rm = TRUE) + 1.96 * sd(!!value_var, na.rm = TRUE) / sqrt(n())
      )
  } else {
    group_var <- sym(group_var)
    tbl %>%
      group_by(!!group_var) %>%
      summarise(
        n = n(),
        mean = mean(!!value_var, na.rm = TRUE),
        sd = sd(!!value_var, na.rm = TRUE),
        ci_low = mean - 1.96 * sd / sqrt(n),
        ci_high = mean + 1.96 * sd / sqrt(n),
        .groups = "drop"
      )
  }
}

# Format numeric for display
fmt_pct <- function(x) percent(x, accuracy = 0.1)
fmt_int <- function(x) format(as.integer(x), big.mark = ",")
