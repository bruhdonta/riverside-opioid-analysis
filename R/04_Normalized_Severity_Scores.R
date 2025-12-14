# ==== 03_poisson_models_post.R (patched) ====
source("R/03_poisson_models.R")
library(scales)
library(dplyr)
library(purrr)
library(gt)

# 1) Pull IRRs from fitted Poisson GLMs
irr_ed    <- pull_zip_irrs(ed_pois)    %>% rename(IRR_ed    = IRR)
irr_hosp  <- pull_zip_irrs(hosp_pois)  %>% rename(IRR_hosp  = IRR)
irr_death <- pull_zip_irrs(death_pois) %>% rename(IRR_death = IRR)

# 2) Build summary table (fix quantiles already handled in summarize_irr)
#    + fix "0.000" minimums to display as "<0.01" (by setting tiny mins to NA and formatting later)
irr_summary_tbl <-
  list(
    `ED visits`        = irr_ed$IRR_ed,
    `Hospitalizations` = irr_hosp$IRR_hosp,
    `Overdose deaths`  = irr_death$IRR_death
  ) |>
  imap_dfr(~ summarize_irr(.x) |> mutate(Outcome = .y)) |>
  relocate(Outcome) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  mutate(`Min IRR` = ifelse(`Min IRR` < 0.01, NA, `Min IRR`))

# Save CSV
if (!dir.exists("tables")) dir.create("tables", recursive = TRUE)
write.csv(irr_summary_tbl, "tables/table2_irr_summary.csv", row.names = FALSE)

# 3) Render + (optionally) export GT table (prevents cropped columns via large vwidth)
gt_tbl <- irr_summary_tbl |>
  gt() |>
  tab_header(
    title = md("**Table 2. IRR Summary by Outcome**"),
    subtitle = "Riverside County ZIP codes (2023)"
  ) |>
  cols_label(
    `Min IRR`      = "Min",
    `1st Quartile` = "Q1",
    Median         = "Median",
    Mean           = "Mean",
    `3rd Quartile` = "Q3",
    `Max IRR`      = "Max"
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 3) |>
  sub_missing(columns = `Min IRR`, missing_text = "<0.01") |>
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12)
  )

# Export (comment these out if you only want to view in RStudio)
gt::gtsave(gt_tbl, "tables/table2_irr_summary.png", vwidth = 1400, vheight = 500)
gt::gtsave(gt_tbl, "tables/table2_irr_summary.html")

# 4) Join wide (leave as-is; you chose to keep the extra columns)
#    IMPORTANT: removing coalesce avoids implying "missing == reference"
irr_wide <- irr_ed %>%
  full_join(irr_hosp,  by = "ZIP") %>%
  full_join(irr_death, by = "ZIP")

# 5) 3:2:1 weighted composite and global 0–1 normalization
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

# 6) Save severity CSV (base write.csv so no readr needed) + quick checks
if (!dir.exists("data_interim")) dir.create("data_interim", recursive = TRUE)
write.csv(severity_by_zip, "data_interim/zip_severity_from_irrs.csv", row.names = FALSE)

cat("Rows:", nrow(severity_by_zip),
    "| unique Severity_Score:", dplyr::n_distinct(severity_by_zip$Severity_Score),
    "| unique Normalized:",    dplyr::n_distinct(severity_by_zip$Normalized_Severity), "\n")
