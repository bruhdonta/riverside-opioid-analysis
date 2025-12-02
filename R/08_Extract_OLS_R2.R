# Script to extract R-squared values from OLS models
source("R/05_HPI_data_prep.R")
library(dplyr)
library(readr)

# Try to load models if they exist in environment, otherwise calculate from coefficient files
# This assumes the models were already run in 05_HPI_data_prep.R

# Read coefficient files to get model info
sev_coef <- read_csv("data_interim/severity_ols_coefficients.csv", show_col_types = FALSE)
ed_coef <- read_csv("data_interim/ed_ols_coefficients.csv", show_col_types = FALSE)
hosp_coef <- read_csv("data_interim/hosp_ols_coefficients.csv", show_col_types = FALSE)
death_coef <- read_csv("data_interim/death_ols_coefficients.csv", show_col_types = FALSE)

# Create comprehensive results table
create_results_table <- function() {
  # Load all coefficient files
  sev_coef <- read_csv("data_interim/severity_ols_coefficients.csv", show_col_types = FALSE) %>%
    mutate(Model = "Composite Severity")
  
  ed_coef <- read_csv("data_interim/ed_ols_coefficients.csv", show_col_types = FALSE) %>%
    mutate(Model = "ED Visits")
  
  hosp_coef <- read_csv("data_interim/hosp_ols_coefficients.csv", show_col_types = FALSE) %>%
    mutate(Model = "Hospitalizations")
  
  death_coef <- read_csv("data_interim/death_ols_coefficients.csv", show_col_types = FALSE) %>%
    mutate(Model = "Overdose Deaths")
  
  # Combine
  coef_all <- bind_rows(sev_coef, ed_coef, hosp_coef, death_coef) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      estimate = round(estimate, 4),
      std.error = round(std.error, 4),
      statistic = round(statistic, 3),
      p.value = signif(p.value, 3),
      p_adj_BH = signif(p_adj_BH, 3),
      Significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(Model, term, estimate, std.error, statistic, p.value, p_adj_BH, Significance) %>%
    arrange(Model, p.value)
  
  return(coef_all)
}

results_table <- create_results_table()
write_csv(results_table, "tables/table3_ols_results_detailed.csv")

cat("\n=== OLS Regression Results Table Created ===\n")
print(results_table)

# If models exist in environment, extract R-squared
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
  cat("\nNote: Models not in environment. R-squared values need to be calculated from running 05_HPI_data_prep.R\n")
}

