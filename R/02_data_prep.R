source("R/01_functions.R")


# R/01_data_prep.R
# Outputs (saved for reuse):
#   data_interim/riv_ed_data_clean.rds
#   data_interim/riv_death_data_clean.rds
#   data_interim/filtered_riv_death_data_clean.rds
#   data_interim/riv_hosp_data_clean.rds

# ------------------------- READ RAW FILES ------------------------------------
# All files expected in data_raw
riv_ed_data  <- read.csv(here::here("data_raw","riv_ed_visits.csv"),
                           check.names = FALSE, stringsAsFactors = FALSE)
riv_pop_data   <- read.csv(here::here("data_raw","riv_pop_data.csv"),
                           check.names = FALSE, stringsAsFactors = FALSE)
riv_hosp_data  <- read.csv(here::here("data_raw","riv_hosp_data.csv"),
                           check.names = FALSE, stringsAsFactors = FALSE)
riv_death_data <- read.csv(here::here("data_raw","riv_death_data.csv"),
                           check.names = FALSE, stringsAsFactors = FALSE)

# ------------------------- ED DATA PREP --------------------------------------
colnames(riv_ed_data) <- riv_ed_data[1, ]

# Remove the first row (since it's now the header)
riv_ed_data <- riv_ed_data[-1, ]

# Reset row index
rownames(riv_ed_data) <- NULL

# Merge + counts for Poisson
riv_ed_data_clean <- merge(riv_ed_data, riv_pop_data, by = "ZIP", all.x = TRUE)


# Be flexible about the rate column name
rate_ed <- intersect(c("Age-Adjusted Rate","Age.Adjusted.Rate","Rate"), names(riv_ed_data_clean))[1]
riv_ed_data_clean[[rate_ed]] <- as.numeric(riv_ed_data_clean[[rate_ed]])
riv_ed_data_clean$Emergency_Department_Visits <-
  round((riv_ed_data_clean[[rate_ed]] / 100000) * riv_ed_data_clean$Count_Person)

# Keep original filter (drops NAs and zeros)
riv_ed_data_clean <- riv_ed_data_clean[
  !is.na(riv_ed_data_clean$Count_Person) &
    !is.na(riv_ed_data_clean[[rate_ed]]),
]
#drop non ZIP code data from df

riv_ed_data_clean <- drop_aggregate_rows(riv_ed_data_clean)


# 2) Set reference level to median
ed_ref_zip <- ref_zip(riv_ed_data_clean, "Emergency_Department_Visits")
message("ED reference ZIP: ", ed_ref_zip)

# 3) Relevel ZIP using that reference (and model with this object)
riv_ed_data_clean$ZIP <-
  stats::relevel(factor(riv_ed_data_clean$ZIP), ref = ed_ref_zip)

saveRDS(riv_ed_data_clean, here::here("data_interim","riv_ed_data_clean.rds"))

# ------------------------- DEATH DATA PREP -----------------------------------
# Merge with pop and compute counts from age-adjusted rate
riv_death_data_clean <- merge(riv_death_data, riv_pop_data, by = "ZIP", all.x = TRUE)
rate_death <- intersect(c("Age-Adjusted Rate","Age.Adjusted.Rate","Rate"), names(riv_death_data_clean))[1]
riv_death_data_clean[[rate_death]] <- as.numeric(riv_death_data_clean[[rate_death]])
riv_death_data_clean$Opioid_Overdose_Deaths <-
  round((riv_death_data_clean[[rate_death]] / 100000) * riv_death_data_clean$Count_Person)

riv_death_data_clean$ZIP <- as.factor(riv_death_data_clean$ZIP)

# Keep original filter (drops NAs and zeros)
riv_death_data_clean <- riv_death_data_clean[
  !is.na(riv_death_data_clean$Count_Person) &
    !is.na(riv_death_data_clean[[rate_death]]),
]



#filtered version for modeling 
riv_death_data_clean <- drop_aggregate_rows(riv_death_data_clean)
#Finding median to use as reference
death_ref_zip <- ref_zip(riv_death_data_clean, "Opioid_Overdose_Deaths")
message("Death reference ZIP: ", death_ref_zip)

riv_death_data_clean$ZIP <- factor(riv_death_data_clean$ZIP)
riv_death_data_clean$ZIP <- stats::relevel(riv_death_data_clean$ZIP, ref = death_ref_zip)

saveRDS(riv_death_data_clean, here::here("data_interim","riv_death_data_clean.rds"))

# ------------------------- HOSPITALIZATION DATA PREP -------------------------
#Merging Hospitalization data with population data

riv_hosp_data_clean <- merge(riv_hosp_data, riv_pop_data, by = "ZIP", all.x = TRUE)
rate_hosp <- intersect(c("Age-Adjusted Rate","Age.Adjusted.Rate","Rate"), names(riv_hosp_data_clean))[1]
# Calculate hospitalizations from rate
riv_hosp_data_clean <- riv_hosp_data_clean %>%
  dplyr::mutate(!!rate_hosp := as.numeric(.data[[rate_hosp]]),
                Opioid_Overdose_Hospitalizations =
                  round((.data[[rate_hosp]] / 100000) * Count_Person))

riv_hosp_data_clean$ZIP <- as.factor(riv_hosp_data_clean$ZIP)

# Keep original filter (drops NAs and zeros)
riv_hosp_data_clean <- riv_hosp_data_clean[
  !is.na(riv_hosp_data_clean$Count_Person) &
    !is.na(riv_hosp_data_clean[[rate_hosp]]),
]

# Drop non ZIP data
riv_hosp_data_clean <- drop_aggregate_rows(riv_hosp_data_clean)

hosp_ref_zip <- ref_zip(riv_hosp_data_clean, "Opioid_Overdose_Hospitalizations")
message("Hospitalizations reference ZIP: ", hosp_ref_zip)

riv_hosp_data_clean$ZIP <- stats::relevel(riv_hosp_data_clean$ZIP, ref = hosp_ref_zip)

saveRDS(riv_hosp_data_clean, here::here("data_interim","riv_hosp_data_clean.rds"))

# Read cleaned data (these include your estimated counts)
ed    <- readRDS(here::here("data_interim","riv_ed_data_clean.rds"))
hosp  <- readRDS(here::here("data_interim","riv_hosp_data_clean.rds"))
death <- readRDS(here::here("data_interim","riv_death_data_clean.rds"))

# Totals (A, B, C) and grand total X
A_ed    <- sum(ed$Emergency_Department_Visits, na.rm = TRUE)
B_hosp  <- sum(hosp$Opioid_Overdose_Hospitalizations, na.rm = TRUE)
C_death <- sum(death$Opioid_Overdose_Deaths, na.rm = TRUE)
X_total <- A_ed + B_hosp + C_death

# county population total (use one source to avoid double-counting)
county_pop <- sum(ed$Count_Person, na.rm = TRUE)

# Build a one-row table
tbl_overall <- tibble::tibble(
  `Total opioid-related incidents` = X_total,
  `ED visits`                      = A_ed,
  `Hospitalizations`               = B_hosp,
  `Overdose deaths`                = C_death
  # If you want to show population or per-100k totals, uncomment below:
  # `County population`                  = county_pop,
  # `Incidents per 100k`                 = 1e5 * X_total / county_pop
)

# Save a clean CSV (journal friendly)
fs::dir_create(here::here("tables"))
readr::write_csv(tbl_overall, here::here("tables","table1_overall_totals.csv"))

#Creating table with gt package
gt_overall <- tbl_overall %>%
  mutate(dplyr::across(everything(), scales::comma)) %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Overall Opioid-Related Incidents, Riverside County (2023)**")
  ) %>%
  tab_source_note(md("Counts are estimated from age-adjusted rates × ZIP population; aggregate rows excluded."))

# Export as PNG 
gtsave(gt_overall, here::here("tables","table1_overall_totals.png"))





message("Saved:",
        "\n  data_interim/riv_ed_data_clean.rds",
        "\n  data_interim/riv_death_data_clean.rds",
        "\n  data_interim/riv_hosp_data_clean.rds")
