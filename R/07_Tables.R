source(here::here("R", "05_HPI_OLS.R"))
#===Table 1: Opioid Crisis in Riverside 2023 Summary Table ===

zip_summary <- tibble::tibble(
  Outcome = c("ED visits", "Hospitalizations", "Overdose deaths"),
  Median = c(
    median(ed$Emergency_Department_Visits, na.rm = TRUE),
    median(hosp$Opioid_Overdose_Hospitalizations, na.rm = TRUE),
    median(death$Opioid_Overdose_Deaths, na.rm = TRUE)
  ),
  IQR = c(
    IQR(ed$Emergency_Department_Visits, na.rm = TRUE),
    IQR(hosp$Opioid_Overdose_Hospitalizations, na.rm = TRUE),
    IQR(death$Opioid_Overdose_Deaths, na.rm = TRUE)
  ),
  Range = c(
    paste0(min(ed$Emergency_Department_Visits, na.rm = TRUE), "–",
           max(ed$Emergency_Department_Visits, na.rm = TRUE)),
    paste0(min(hosp$Opioid_Overdose_Hospitalizations, na.rm = TRUE), "–",
           max(hosp$Opioid_Overdose_Hospitalizations, na.rm = TRUE)),
    paste0(min(death$Opioid_Overdose_Deaths, na.rm = TRUE), "–",
           max(death$Opioid_Overdose_Deaths, na.rm = TRUE))
  )
)

zip_summary <- zip_summary %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Distribution of Opioid Outcomes Across Riverside County ZIP Codes**")
  ) %>%
  cols_label(
    Outcome = "Outcome",
    Median = "Median",
    IQR = "IQR",
    Range = "Range"
  ) %>%
  tab_source_note(
    md("*Counts estimated from age-adjusted rates × ZIP code population.*")
  )

gtsave(zip_summary, "tables/Table1.docx")

#=== Table 2 IRR Summary Table ===
irr_summary_tbl <-
  list(
    `ED visits`        = irr_ed$IRR_ed,
    `Hospitalizations` = irr_hosp$IRR_hosp,
    `Overdose deaths`  = irr_death$IRR_death
  ) %>%
  imap_dfr(~ summarize_irr(.x) %>% mutate(Outcome = .y)) %>%
  relocate(Outcome) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  mutate(`Min IRR` = ifelse(`Min IRR` < 0.01, NA, `Min IRR`))

#Render + export GT table (prevents cropped columns via large vwidth)
irr_summary_tbl <- irr_summary_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. IRR Summary by Outcome**"),
    subtitle = "Riverside County ZIP codes (2023)"
  ) %>%
  cols_label(
    `Min IRR`      = "Min",
    `1st Quartile` = "Q1",
    Median         = "Median",
    Mean           = "Mean",
    `3rd Quartile` = "Q3",
    `Max IRR`      = "Max"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 3) %>%
  sub_missing(columns = `Min IRR`, missing_text = "<0.01") %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12)
  )
gtsave(irr_summary_tbl,"tables/Table2.docx")

#=== Table 3 Multivar Regression Summary ===

t3 <- sev_domains_ols$model_step %>% 
  tbl_regression() %>%
  bold_labels() %>%
  bold_p(t = 0.05) %>%
  modify_caption("**Table 3.** *Multivariable Regression of HPI Domains*")

t3 <- as_gt(t3)

gt::gtsave(
  t3,
  filename = "tables/Table3_HPI_Domains_Regression.docx"
)

#=== Table 4 Multivar Regression Summary ===

t4 <- sev_indicators_ols$model_step %>% 
  tbl_regression() %>%
  bold_labels() %>%
  bold_p(t = 0.05) %>%
  modify_caption("**Table 4.** *Multivariable Regression of HPI Indicators*")

t4 <- as_gt(t4)

gt::gtsave(
  t4,
  filename = "tables/Table4_HPI_Indicators_Regression.docx"
)

