# R/00_setup.R
set.seed(69)
pkgs <- c(
  "here","dplyr","readr","ggplot2","broom","car","MASS","stringr",
  "forcats","leaflet","sf","tigris","RColorBrewer","htmlwidgets","flextable","gt","mapview","webshot2"
)


missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall once in the Console:\ninstall.packages(c(", 
       paste(sprintf('"%s"', missing), collapse=", "), "))")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# Create folders used later (safe if they already exist)
for (p in here::here(c("data_raw","data_raw/HPI","data_interim","data_processed",
                       "figures","models","tables","reports"))) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# Keep geodata cached; avoid scientific notation
options(tigris_use_cache = TRUE, scipen = 999)


# Columns you plan to scale in HPI models 
scale_vars <- c(
  "housing","healthcare_access","insured","above_poverty","economic_domain",
  "education_domain","neighborhood_domain","homeownership","ownsevere",
  "social_domain","transportation"
)


message("Setup complete. Project root: ", here::here())


