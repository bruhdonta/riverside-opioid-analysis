# ==== 03_poisson_models_post.R (patched) ====
source(here::here("R", "03_poisson_models.R"))
library(scales)
library(dplyr)
library(purrr)
library(gt)

# 1) Pull IRRs from fitted Poisson GLMs
irr_ed    <- pull_zip_irrs(ed_pois)    %>% rename(IRR_ed    = IRR)
irr_hosp  <- pull_zip_irrs(hosp_pois)  %>% rename(IRR_hosp  = IRR)
irr_death <- pull_zip_irrs(death_pois) %>% rename(IRR_death = IRR)

irr_wide <- irr_ed %>%
  full_join(irr_hosp,  by = "ZIP") %>%
  full_join(irr_death, by = "ZIP")

# 2) 3:2:1 weighted composite and global 0–1 normalization
severity_by_zip <- irr_wide %>%
  mutate(
    Severity_Score = 3*IRR_death + 2*IRR_hosp + 1*IRR_ed
  ) %>%
  mutate(
    Normalized_Severity = if (diff(range(Severity_Score, na.rm = TRUE)) > 0)
      rescale(Severity_Score, to = c(0,1))
    else 0
  ) %>%
  arrange(desc(Normalized_Severity))

# 3) Save severity CSV (base write.csv so no readr needed) + quick checks
if (!dir.exists("data_interim")) dir.create("data_interim", recursive = TRUE)
write.csv(severity_by_zip, "data_interim/zip_severity_from_irrs.csv", row.names = FALSE)

cat("Rows:", nrow(severity_by_zip),
    "| unique Severity_Score:", dplyr::n_distinct(severity_by_zip$Severity_Score),
    "| unique Normalized:",    dplyr::n_distinct(severity_by_zip$Normalized_Severity), "\n")
