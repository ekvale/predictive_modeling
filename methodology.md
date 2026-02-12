# Methodology Note

## Synthetic Data

All data in this dashboard are **synthetic** and generated for demonstration only. No real patient data are used. The data are designed to be:

- **HIPAA-compliant**: No identifiers or real health information.
- **Realistic in structure**: Patient intake dates, appointment history, geographic coordinates, demographics, dropout, and missed-appointment reasons follow plausible distributions.
- **Clearly artificial**: Parameters and associations (e.g., higher dropout in "Poor" health, higher miss rates for certain reasons) are chosen for educational and presentation purposes.

## Data Generation

- **Patient-level**: Each record has an anonymized ID, intake date, latitude/longitude (within a fictional metro area), age group, general health indicator, and optional dropout date.
- **Appointment-level**: Each patient has multiple appointments with dates, attended (yes/no), and—when missed—a reason code (e.g., access barrier, agoraphobia, transportation, scheduling, other).
- **Dropout**: A subset of patients is assigned a "dropout" date (no longer in care); time to dropout is simulated (e.g., Weibull) and can vary by health status.
- **Reproducibility**: A fixed random seed ensures the same dataset is generated each run (see parameters below).

## Analyses

- **Overview**: Summary counts, attendance rate, dropout rate, and time trends.
- **Longitudinal**: Kaplan–Meier–style retention curves (time to dropout) and time series of attendance/dropout.
- **Geographic**: Map of intake locations and regional summaries (no real geography).
- **Disruption**: Missed-appointment reasons, adherence by group, and confidence intervals where appropriate.

This app is intended for the r/medicine conference as an educational tool for discussing healthcare access, continuity of care, and dropout modeling.
