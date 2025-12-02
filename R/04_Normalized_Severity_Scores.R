source("R/03_poisson_models.R")
library(scales)
library(dplyr)
library(purrr)
library(gt)
# Guards: need both the robust tables *and* the fitted models to get the reference ZIP
need <- function(x) !exists(x, inherits = TRUE) || is.null(get(x, inherits = TRUE))
if (need("ed_pois_rob") || need("hosp_pois_rob") || need("death_pois_rob"))
  stop("Missing ed_pois_rob / hosp_pois_rob / death_pois_rob in memory.")
if (need("ed_pois") || need("hosp_pois") || need("death_pois"))
  stop("Missing ed_pois / hosp_pois / death_pois (used to detect reference ZIP).")


  
  # keep only ZIP terms; clean code; keep IRR column
  zips <- rob_tbl %>%
    filter(grepl("^ZIP", term)) %>%
    transmute(
      ZIP = sub("^ZIP", "", term),
      IRR = IRR
    )
  
  ref <- tibble(ZIP = ref_zip, IRR = 1)
  {
  bind_rows(ref, zips) %>%
    mutate(ZIP = ifelse(nchar(ZIP) < 5, str_pad(ZIP, 5, pad = "0"), ZIP)) %>%
    arrange(ZIP)
}

irr_ed    <- pull_zip_irrs(ed_pois_rob,    ed_pois)    %>% rename(IRR_ed    = IRR)
irr_hosp  <- pull_zip_irrs(hosp_pois_rob,  hosp_pois)  %>% rename(IRR_hosp  = IRR)
irr_death <- pull_zip_irrs(death_pois_rob, death_pois) %>% rename(IRR_death = IRR)

# Put all outcomes in one place, compute summaries, and format
irr_summary_tbl <-
  list(
    `ED visits`        = irr_ed$IRR_ed,
    `Hospitalizations` = irr_hosp$IRR_hosp,
    `Overdose deaths`  = irr_death$IRR_death
  ) |> 
  imap_dfr(~ summarize_irr(.x) |> mutate(Outcome = .y)) |>
  relocate(Outcome) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write.csv(irr_summary_tbl, "tables/table2_irr_summary.csv", row.names = FALSE)

# table for Quarto/HTML/PDF
irr_summary_tbl |>
  gt() |>
  tab_header(
    title = "Range of Incidence Rate Ratios (IRRs) for Opioid Outcomes Across Riverside County ZIP Codes (2023)"
  )

# join wide; fill missing with neutral IRR=1
irr_wide <- irr_ed %>%
  full_join(irr_hosp,  by = "ZIP") %>%
  full_join(irr_death, by = "ZIP") %>%
  mutate(
    IRR_ed    = coalesce(IRR_ed,    1),
    IRR_hosp  = coalesce(IRR_hosp,  1),
    IRR_death = coalesce(IRR_death, 1)
  )

# 3:2:1 weighted composite and global 0–1 normalization
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

# save + quick checks
if (!dir.exists("data_interim")) dir.create("data_interim", recursive = TRUE)
write_csv(severity_by_zip, "data_interim/zip_severity_from_robust_irrs.csv")

cat("Rows:", nrow(severity_by_zip),
    "| unique Severity_Score:", dplyr::n_distinct(severity_by_zip$Severity_Score),
    "| unique Normalized:",    dplyr::n_distinct(severity_by_zip$Normalized_Severity), "\n")
head(severity_by_zip, 10)
