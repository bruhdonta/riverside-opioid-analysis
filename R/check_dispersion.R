# Quick script to check dispersion from saved models
# Run this after running 03_poisson_models.R

source("R/01_functions.R")

# Load the saved models
ed_pois <- readRDS("models/ed_poisson_model.rds")
hosp_pois <- readRDS("models/hosp_poisson_model.rds")
death_pois <- readRDS("models/death_poisson_model.rds")

# Check dispersion using the updated function
cat("\n=== Dispersion Check ===\n")
cat("ED model:\n")
ed_disp <- dispersion(ed_pois)
print(ed_disp)
cat("  df.residual:", ed_pois$df.residual, "\n")
cat("  n observations:", nrow(ed_pois$model), "\n")
cat("  n parameters:", length(coef(ed_pois)), "\n\n")

cat("Hosp model:\n")
hosp_disp <- dispersion(hosp_pois)
print(hosp_disp)
cat("  df.residual:", hosp_pois$df.residual, "\n")
cat("  n observations:", nrow(hosp_pois$model), "\n")
cat("  n parameters:", length(coef(hosp_pois)), "\n\n")

cat("Death model:\n")
death_disp <- dispersion(death_pois)
print(death_disp)
cat("  df.residual:", death_pois$df.residual, "\n")
cat("  n observations:", nrow(death_pois$model), "\n")
cat("  n parameters:", length(coef(death_pois)), "\n\n")

# Alternative: Use summary() method directly
cat("\n=== Using summary() method ===\n")
sm_ed <- summary(ed_pois)
sm_hosp <- summary(hosp_pois)
sm_death <- summary(death_pois)

cat("ED dispersion (from summary):", sm_ed$deviance / sm_ed$df.residual, "\n")
cat("Hosp dispersion (from summary):", sm_hosp$deviance / sm_hosp$df.residual, "\n")
cat("Death dispersion (from summary):", sm_death$deviance / sm_death$df.residual, "\n")

