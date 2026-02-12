# Methodology Note

This dashboard was built for **[R/Medicine 2026](https://rconsortium.github.io/RMedicine_website/)**, a conference for sharing R-based tools and approaches to analyze and gain insights from health data. It demonstrates analysis of healthcare access, continuity of care, and disparities using **R and Shiny** on synthetic data.

## Synthetic Data

All data are **synthetic** and generated for demonstration only. No real patient data are used. The default geographic setting is the **Minneapolis–Saint Paul metro area** (synthetic coordinates only). The data are designed to be:

- **HIPAA-compliant**: No identifiers or real health information.
- **Realistic in structure**: Patient intake dates, appointment history, geographic coordinates, demographics (age, health, race/ethnicity), dropout, and missed-appointment reasons follow plausible distributions.
- **Clearly artificial**: Parameters and associations are chosen for educational and presentation purposes (e.g., higher dropout in "Poor" health; differential outcomes by race/ethnicity to model disparities).

## Data Generation

- **Patient-level**: Anonymized ID, intake date, latitude/longitude, age group, general health indicator, **race/ethnicity** (synthetic categories aligned to Minneapolis–Saint Paul metro demographics, including White, Black, Somali, Asian, Hmong, Hispanic/Latino, Native American, Other), and optional dropout date.
- **Appointment-level**: Patient ID, appointment date, attended (yes/no), and—when missed—a reason code (e.g., access barrier, agoraphobia, transportation, scheduling, other).
- **Dropout**: A subset of patients is assigned a "dropout" date; time to dropout is simulated and can vary by health status and by race/ethnicity (to explore disparate impact).
- **Disruption scenario**: A **disruption period** (date range) can be set; during that window the model increases dropout and missed-appointment rates to simulate reduced access.
- **Reproducibility**: A fixed random seed ensures the same dataset is generated each run (see parameters in the Reproducibility tab).

## Analyses

- **Overview**: Summary counts, attendance rate, dropout rate, and time trends.
- **Longitudinal**: Kaplan–Meier–style retention curves (stratifiable by age, health, or race/ethnicity) and time series.
- **Geographic**: Map of intake locations and regional summaries.
- **Disruption**: Missed-appointment reasons, adherence by group, and confidence intervals.
- **Disparities**: Outcomes by race/ethnicity (attendance, dropout, disparity ratio) to explore how different demographic groups may be disproportionately affected.
- **Cost & Health Impact**: User-defined unit costs for missed appointments and dropouts; total cost and health burden (e.g., missed care per 1,000 patients) for the filtered population.

This app is intended for R/Medicine as an educational tool for healthcare access, continuity of care, disparity analysis, and cost/impact modeling—all on synthetic data.
