# Methodology Note

This dashboard was built for **[R/Medicine 2026](https://rconsortium.github.io/RMedicine_website/)** to help study **how a period of disruption—when access to care is reduced—affects patient attendance and retention**. It is intended for researchers, clinicians, and policymakers who want to explore these effects in a transparent, reproducible way using **R and Shiny**. All data are synthetic; no real patient information is used.

## Focus: Studying the Disruption Period

The main purpose of the app is to understand the impact of a **disruption period**: a defined window of time during which access to care is assumed to be lower (e.g. due to a community event, system shock, or policy change). You choose the start and end dates in the sidebar. The synthetic data are then generated so that during that window, missed appointments and dropout from care are elevated. Elsewhere in the app you can see where that window falls (shaded gray on time-series charts), compare outcomes across demographic groups, and estimate cost and health burden. Changing the disruption period lets you ask “what if” questions and compare scenarios.

## Synthetic Data

All data are **synthetic** and for demonstration only. The default setting is the **Minneapolis–Saint Paul metro area** (synthetic coordinates and demographics). The data are:

- **HIPAA-compliant**: No identifiers or real health information.
- **Structured realistically**: Intake dates, appointment history, geography, demographics (age, health, race/ethnicity), dropout, and reasons for missed appointments follow plausible distributions.
- **Clearly artificial**: Associations (e.g. higher dropout in poorer health, differential outcomes by race/ethnicity) are set for teaching and analysis, not to represent any real population.

## Data Generation

- **Patient-level**: Anonymized ID, intake date, latitude/longitude, age group, general health, **race/ethnicity** (Minneapolis-aligned categories: White, Black, Somali, Asian, Hmong, Hispanic/Latino, Native American, Other), and optional dropout date.
- **Appointment-level**: Patient ID, appointment date, attended (yes/no), and—when missed—a reason (e.g. access barrier, agoraphobia, transportation, scheduling, other).
- **Dropout**: Some patients are assigned a dropout date; the chance of dropping out varies by health and race/ethnicity to allow disparity analysis.
- **Disruption period**: When you set a date range for the disruption, the model increases the probability of dropout and of missing appointments during that window. The same seed is used so that changing only the disruption dates makes the scenario comparable.
- **Reproducibility**: A fixed random seed is used; see the Reproducibility tab for the exact call and parameters.

## What You Can Do in the App

- **Overview**: Key counts, attendance and dropout rates, and time trends. The **shaded area** on the attendance chart is the disruption period.
- **Longitudinal**: Retention (Kaplan–Meier–style) and time series, with the disruption window again shown in gray.
- **Geographic**: Where patients are located (synthetic) and regional summaries.
- **Disruption Modeling**: How the disruption shows up in missed appointments and reasons, and in attendance by health status.
- **Disparities**: Outcomes by race/ethnicity so you can see who is disproportionately affected.
- **Cost & Health Impact**: You supply unit costs; the app estimates total cost and burden (e.g. missed care per 1,000 patients) for the filtered population.
- **Tell the Story**: Flow charts for presenting the narrative to an audience.

The app is meant as an educational and analytical tool: to make the effects of a disruption period visible, comparable, and discussable—all on synthetic data.
