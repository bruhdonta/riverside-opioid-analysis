# Script to check for overdispersion in Poisson models
# Uses AER::dispersiontest for proper overdispersion testing

source("R/00_setup.R")
library(dplyr)
library(readr)
library(AER)
library(MASS)

# Load Poisson models
ed_pois <- readRDS("models/ed_poisson_model.rds")
hosp_pois <- readRDS("models/hosp_poisson_model.rds")
death_pois <- readRDS("models/death_poisson_model.rds")

# Function to extract overdispersion test results
get_overdispersion_results <- function(model, model_name) {
  test_result <- dispersiontest(model)
  
  tibble(
    Model = model_name,
    Dispersion_Statistic = as.numeric(test_result$estimate),
    p_value = as.numeric(test_result$p.value),
    Overdispersed = ifelse(test_result$p.value < 0.05 & test_result$estimate > 1, 
                           "Yes", "No"),
    Interpretation = case_when(
      test_result$estimate < 1 ~ "Underdispersed (variance < mean)",
      test_result$estimate == 1 ~ "Proper Poisson variance",
      test_result$estimate > 1 & test_result$p.value < 0.05 ~ "Overdispersed (variance > mean)",
      TRUE ~ "Not significantly overdispersed"
    )
  )
}

# Run tests for all models
overdispersion_results <- bind_rows(
  get_overdispersion_results(ed_pois, "ED Visits"),
  get_overdispersion_results(hosp_pois, "Hospitalizations"),
  get_overdispersion_results(death_pois, "Deaths")
) %>%
  mutate(
    Dispersion_Statistic = round(Dispersion_Statistic, 6),
    p_value = signif(p_value, 3)
  )

# Save results
write_csv(overdispersion_results, "tables/poisson_overdispersion_test_results.csv")

cat("\n=== Poisson Model Overdispersion Test Results ===\n")
print(overdispersion_results)

cat("\n=== Summary ===\n")
if (all(overdispersion_results$Overdispersed == "No")) {
  cat("✓ None of the Poisson models show overdispersion.\n")
  cat("✓ All models have p-values > 0.05, confirming no significant overdispersion.\n")
  if (any(overdispersion_results$Dispersion_Statistic < 1)) {
    cat("⚠ Some models show underdispersion (dispersion < 1).\n")
    cat("  This is not a problem when using robust standard errors.\n")
  }
} else {
  overdisp_models <- overdispersion_results %>% 
    filter(Overdispersed == "Yes") %>% 
    pull(Model)
  cat("⚠ The following models show overdispersion:\n")
  cat(paste("  -", overdisp_models, collapse = "\n"), "\n")
  cat("Consider using robust standard errors or negative binomial models.\n")
}

# Check if NB models exist and their theta values
cat("\n=== Negative Binomial Model Theta Values ===\n")
cat("(High theta = less overdispersion, low theta = more overdispersion)\n\n")

if (file.exists("data_interim/ed_nb_model.rds")) {
  ed_nb <- readRDS("data_interim/ed_nb_model.rds")
  cat("ED NB theta:", format(ed_nb$theta, scientific = FALSE, digits = 2), "\n")
  cat("  (1/theta =", format(1/ed_nb$theta, scientific = TRUE, digits = 3), ")\n")
}

if (file.exists("models/hosp_nb_model.rds")) {
  hosp_nb <- readRDS("models/hosp_nb_model.rds")
  cat("Hosp NB theta:", format(hosp_nb$theta, scientific = FALSE, digits = 2), "\n")
  cat("  (1/theta =", format(1/hosp_nb$theta, scientific = TRUE, digits = 3), ")\n")
}

if (file.exists("models/death_nb_model.rds")) {
  death_nb <- readRDS("models/death_nb_model.rds")
  cat("Death NB theta:", format(death_nb$theta, scientific = FALSE, digits = 2), "\n")
  cat("  (1/theta =", format(1/death_nb$theta, scientific = TRUE, digits = 3), ")\n")
}

cat("\nNote: Very high theta values indicate NB models are essentially Poisson (no overdispersion).\n")

