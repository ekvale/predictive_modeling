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
# Default location: Minneapolis metro. Optional disruption period elevates
# dropout and missed appointments during that window (politically neutral
# "healthcare disruption" scenario for modeling).

# Default disruption window (used when event not set); Minneapolis-relevant.
DEFAULT_EVENT_START <- as.Date("2020-05-25")
DEFAULT_EVENT_END   <- as.Date("2020-06-30")

generate_synthetic_patients <- function(n_patients = 1200, seed = 42,
                                       event_start = NULL, event_end = NULL,
                                       event_dropout_boost = 0.35, event_miss_boost = 0.25) {
  set.seed(seed)
  # Intake range includes 2020 so disruption event can affect outcomes
  start_intake <- as.Date("2020-01-01")
  end_intake   <- as.Date("2024-06-30")
  intake_dates <- sample(seq(start_intake, end_intake, by = "day"), n_patients, replace = TRUE)

  # Minneapolis–Saint Paul metro (synthetic points within metro area)
  lat_range <- c(44.88, 45.15)
  lon_range <- c(-93.35, -93.00)
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

  # Optional: healthcare disruption period (e.g. reduced access) increases dropout during window
  if (!is.null(event_start) && !is.null(event_end) && event_dropout_boost > 0) {
    event_start <- as.Date(event_start)
    event_end   <- as.Date(event_end)
    at_risk_during_event <- intake_dates <= event_end & (is.na(dropout_date) | dropout_date > event_start)
    n_at_risk <- sum(at_risk_during_event)
    extra_dropout <- runif(n_at_risk) < event_dropout_boost
    event_dropout_dates <- as.Date(
      runif(n_at_risk, as.numeric(event_start), as.numeric(event_end)),
      origin = "1970-01-01"
    )
    dropout_date[at_risk_during_event][extra_dropout] <- event_dropout_dates[extra_dropout]
  }

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
  health_miss_mult <- c(Good = 0.8, Fair = 1.2, Poor = 1.5)
  base_miss <- 0.2
  p_miss <- pmin(0.85, base_miss * health_miss_mult[patients$health[appointment_rows]])
  p_miss <- p_miss * (0.7 + 0.3 * runif(n_appts))  # add noise
  # During disruption period, elevate miss probability (reduced access)
  if (!is.null(event_start) && !is.null(event_end) && event_miss_boost > 0) {
    event_start <- as.Date(event_start)
    event_end   <- as.Date(event_end)
    in_event <- appt_dates >= event_start & appt_dates <= event_end
    p_miss[in_event] <- pmin(0.95, p_miss[in_event] + event_miss_boost)
  }
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

# Generate once at app load with default disruption window (for UI defaults / levels)
synth <- generate_synthetic_patients(
  n_patients = 1200, seed = 42,
  event_start = DEFAULT_EVENT_START, event_end = DEFAULT_EVENT_END
)
patients_df    <- synth$patients
appointments_df <- synth$appointments

# -----------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Build data frame from survfit for ggplot (no extra packages)
# Handles NULL lower/upper when conf.int not computed.
survfit_to_df <- function(fit) {
  lo <- fit$lower
  hi <- fit$upper
  if (is.null(lo)) lo <- fit$surv
  if (is.null(hi)) hi <- fit$surv
  if (is.null(fit$strata)) {
    return(tibble(
      time = as.numeric(c(0, fit$time)),
      surv = as.numeric(c(1, fit$surv)),
      lower = as.numeric(c(1, lo)),
      upper = as.numeric(c(1, hi)),
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
      time = as.numeric(c(0, fit$time[idx])),
      surv = as.numeric(c(1, fit$surv[idx])),
      lower = as.numeric(c(1, lo[idx])),
      upper = as.numeric(c(1, hi[idx])),
      strata = as.character(names(lens)[i])
    )
  })
  out
}

# Kaplan-Meier curve as ggplot (patient retention / time to dropout)
# Uses numeric time/surv and character strata so plot renders reliably.
km_curve_ggplot <- function(df, group_var = NULL, title = "Patient retention (time to dropout)") {
  # Ensure numeric for Surv(); need at least one row
  df <- df %>% filter(!is.na(time_days), time_days >= 0)
  if (nrow(df) == 0) return(ggplot() + labs(title = "No data") + theme_minimal())
  pct_lab <- function(x) scales::percent(x, accuracy = 0.1)
  if (is.null(group_var)) {
    fit <- survfit(Surv(time_days, event) ~ 1, data = df)
    f <- survfit_to_df(fit)
    p <- ggplot(f, aes(time, surv)) +
      geom_step(aes(color = "Overall"), linewidth = 1.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = cb_palette[1]) +
      scale_y_continuous(labels = pct_lab, limits = c(0, 1)) +
      labs(x = "Days since intake", y = "Retention probability", title = title, color = NULL) +
      scale_color_manual(values = setNames(cb_palette[1], "Overall")) +
      theme(legend.position = "bottom")
    return(p)
  }
  group_var <- sym(group_var)
  fit <- survfit(Surv(time_days, event) ~ !!group_var, data = df)
  f <- survfit_to_df(fit)
  f$strata <- as.character(f$strata)
  strata_levels <- unique(f$strata)
  nlev <- length(strata_levels)
  if (nlev == 0) return(ggplot() + labs(title = "No strata") + theme_minimal())
  val_vec <- setNames(cb_palette[seq_len(nlev)], strata_levels)
  p <- ggplot(f, aes(time, surv, color = strata, fill = strata)) +
    geom_step(linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
    scale_y_continuous(labels = pct_lab, limits = c(0, 1)) +
    labs(x = "Days since intake", y = "Retention probability", title = title, color = NULL, fill = NULL) +
    scale_color_manual(values = val_vec, breaks = strata_levels) +
    scale_fill_manual(values = val_vec, breaks = strata_levels) +
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
