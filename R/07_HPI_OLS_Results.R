source("R/05_HPI_data_prep.R")

library(gt)

# --- Load coefficient tables generated from 05_HPI_data_prep.R ---
sev_coef   <- read_csv("data_interim/severity_ols_coefficients.csv", show_col_types = FALSE) %>% mutate(Model = "Composite Severity")
ed_coef    <- read_csv("data_interim/ed_ols_coefficients.csv", show_col_types = FALSE) %>% mutate(Model = "ED Visits")
hosp_coef  <- read_csv("data_interim/hosp_ols_coefficients.csv", show_col_types = FALSE) %>% mutate(Model = "Hospitalizations")
death_coef <- read_csv("data_interim/death_ols_coefficients.csv", show_col_types = FALSE) %>% mutate(Model = "Overdose Deaths")

# --- Combine into one dataframe ---
coef_all <- bind_rows(sev_coef, ed_coef, hosp_coef, death_coef) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate  = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 3),
    p.value   = signif(p.value, 3),
    p_adj_BH  = signif(p_adj_BH, 3),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  arrange(Model, p.value)

# --- Save combined coefficients for reference ---
write_csv(coef_all, "tables/table3_ols_results.csv")

cat("\n=== OLS Regression Results ===\n")
print(coef_all)

# --- Optional: Display pretty table ---
if (requireNamespace("gt", quietly = TRUE)) {
  coef_table <- coef_all %>%
    select(Model, term, estimate, std.error, statistic, p.value, Significance) %>%
    rename(
      Predictor = term,
      `Coefficient` = estimate,
      `Std. Error` = std.error,
      `t-statistic` = statistic,
      `p-value` = p.value,
      `Sig.` = Significance
    ) %>%
    gt() %>%
    tab_header(
      title = "Table 3. Multivariable Linear Regression Results: Association Between HPI Domains and Opioid Outcome Severity",
      subtitle = "Models fit using stepwise selection (stepAIC). All predictors standardized before modeling."
    ) %>%
    fmt_number(columns = c("Coefficient", "Std. Error"), decimals = 4) %>%
    fmt_number(columns = "t-statistic", decimals = 3) %>%
    fmt_scientific(columns = "p-value", decimals = 2)
  
  # Export HTML table for your manuscript or report
  tryCatch({
    gtsave(coef_table, "tables/table3_ols_results.html")
  }, error = function(e) {
    cat("Note: Could not save HTML table. GT package may not be fully available.\n")
  })
}

# --- Extract R-squared for each model ---
if (exists("sev_models") && exists("ed_models") && exists("hosp_models") && exists("death_models")) {
  model_summaries <- tibble(
    Model = c("Composite Severity", "ED Visits", "Hospitalizations", "Overdose Deaths"),
    R_squared = c(
      summary(sev_models$model_step)$r.squared,
      summary(ed_models$model_step)$r.squared,
      summary(hosp_models$model_step)$r.squared,
      summary(death_models$model_step)$r.squared
    ),
    Adj_R_squared = c(
      summary(sev_models$model_step)$adj.r.squared,
      summary(ed_models$model_step)$adj.r.squared,
      summary(hosp_models$model_step)$adj.r.squared,
      summary(death_models$model_step)$adj.r.squared
    ),
    n_predictors = c(
      length(coef(sev_models$model_step)) - 1,  # minus intercept
      length(coef(ed_models$model_step)) - 1,
      length(coef(hosp_models$model_step)) - 1,
      length(coef(death_models$model_step)) - 1
    )
  ) %>%
    mutate(
      R_squared = round(R_squared, 4),
      Adj_R_squared = round(Adj_R_squared, 4)
    )
  
  write_csv(model_summaries, "tables/table3_r2_summary.csv")
  
  cat("\n=== Model R-squared Summary ===\n")
  print(model_summaries)
} else {
  cat("\nNote: Model objects not found in environment. Run 05_HPI_data_prep.R first to generate R-squared values.\n")
}
