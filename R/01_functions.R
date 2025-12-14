source("R/00_setup.R")
# ==== Utility helpers ====

# save_fig: Saves a ggplot object as a PNG file to the "figures" directory with specified width, height, and DPI.
save_fig <- function(plot, filename, w = 7, h = 5, dpi = 300) {
  ggplot2::ggsave(
    filename = here::here("figures", filename),
    plot     = plot,
    width    = w,
    height   = h,
    dpi      = dpi,
    bg       = "white"
  )
}

# zipify: Pads ZIP codes with leading zeros to ensure they are 5 digits long (e.g., 9251 becomes 09251).
zipify <- function(x) {
  x <- as.character(x)
  stringr::str_pad(x, width = 5, pad = "0")
}

# safe_nb: Tries fitting a negative binomial model using multiple initial values for theta. Returns NULL if all attempts fail.
safe_nb <- function(formula, data) {
  trials <- c(0.5, 1, 2, 5, 10, 20)
  for (th in trials) {
    fit <- try(
      MASS::glm.nb(
        formula,
        data       = data,
        init.theta = th,
        control    = glm.control(maxit = 200, trace = FALSE)
      ),
      silent = TRUE
    )
    if (!inherits(fit, "try-error") && is.finite(fit$theta)) {
      return(fit)
    }
  }
  return(NULL)
}

# drop_aggregate_rows: Filters out aggregate rows (e.g., rows with "California" or "Riverside") from the dataframe.
drop_aggregate_rows <- function(df, zip_col = "ZIP") {
  df[!(df[[zip_col]] %in% c("California", "Riverside")), , drop = FALSE]
}

# ref_zip: Sets the reference ZIP code for a model based on the ZIP with the closest estimated value for the outcome column (e.g., ED visits, hospitalizations).
ref_zip <- function(df, outcome_col, pop_col = "Count_Person", zip_col = "ZIP") {
  med <- stats::median(df[[outcome_col]], na.rm = TRUE)
  df |>
    dplyr::mutate(.dist = abs(.data[[outcome_col]] - med)) |>
    dplyr::slice_min(.dist, with_ties = TRUE) |>
    dplyr::arrange(dplyr::desc(.data[[pop_col]])) |>
    dplyr::slice(1) |>
    dplyr::pull(.data[[zip_col]]) |>
    as.character()
}

# ==== IRR + regression table helpers ====

# robust_irr_tbl: Calculates robust standard errors and confidence intervals for Poisson regression models and returns the results in a tidy tibble.
robust_irr_tbl <- function(fit, vcov_type = "HC0") {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = vcov_type))
  tibble::tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "z value"],
    p.value   = ct[, "Pr(>|z|)"],
    IRR       = exp(estimate),
    IRR_low   = exp(estimate - 1.96 * std.error),
    IRR_high  = exp(estimate + 1.96 * std.error)
  )
}

# dispersion: Calculates the dispersion parameter for a Poisson model by dividing the deviance by the residual degrees of freedom.
dispersion <- function(fit) {
  sm <- summary(fit)
  if (!is.null(sm$deviance) && !is.null(sm$df.residual)) {
    if (is.finite(sm$deviance) && is.finite(sm$df.residual) && sm$df.residual > 0) {
      return(sm$deviance / sm$df.residual)
    }
  }
  if (is.null(fit$df.residual) || !is.finite(fit$df.residual) || fit$df.residual <= 0) {
    warning("df.residual is invalid (", fit$df.residual, "). Model may be saturated. Returning NA.")
    return(NA_real_)
  }
  
  dev_resid <- residuals(fit, type = "deviance")
  if (any(!is.finite(dev_resid))) {
    warning("Non-finite deviance residuals detected")
    return(NA_real_)
  }
  
  sum(dev_resid^2) / fit$df.residual
}

# qb_tbl: Generates a tidy tibble from the output of a quantile regression (using `summary(fit_qb)`), including estimates, standard errors, and IRRs.
qb_tbl <- function(fit_qb) {
  sm <- summary(fit_qb)$coefficients
  tibble::tibble(
    term      = rownames(sm),
    estimate  = sm[, "Estimate"],
    std.error = sm[, "Std. Error"],
    statistic = sm[, "t value"],
    p.value   = sm[, "Pr(>|t|)"],
    IRR       = exp(estimate),
    IRR_low   = exp(estimate - 1.96 * std.error),
    IRR_high  = exp(estimate + 1.96 * std.error)
  )
}

# summarize_irr: Summarizes a vector of IRR values by calculating the minimum, quartiles, median, mean, and maximum.
summarize_irr <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  tibble::tibble(
    `Min IRR`       = min(x, na.rm = TRUE),
    `1st Quartile`  = as.numeric(stats::quantile(x, 0.25, names = FALSE)),
    `Median`        = stats::median(x),
    `Mean`          = mean(x),
    `3rd Quartile`  = as.numeric(stats::quantile(x, 0.75, names = FALSE)),
    `Max IRR`       = max(x)
  )
}

# pull_zip_irrs: Pulls incidence rate ratios (IRRs) for each ZIP code from a Poisson regression model, calculates 95% confidence intervals, and adds a reference ZIP.
pull_zip_irrs <- function(fit_model, zip_var = "ZIP", conf_level = 0.95) {
  if (!inherits(fit_model, "glm")) {
    stop("fit_model must be a fitted glm object.")
  }
  
  mm <- model.frame(fit_model)
  if (!zip_var %in% names(mm)) {
    stop(paste0("Variable '", zip_var, "' not found in model frame."))
  }
  
  z <- mm[[zip_var]]
  if (!is.factor(z)) {
    stop(paste0("Variable '", zip_var, "' must be a factor in the model (use factor()+relevel())."))
  }
  ref_zip <- levels(z)[1]
  
  sm <- summary(fit_model)$coefficients
  sm <- as.data.frame(sm)
  sm$term <- rownames(sm)
  
  prefix <- paste0("^", zip_var)
  zip_rows <- sm[grepl(prefix, sm$term), , drop = FALSE]
  
  alpha <- 1 - conf_level
  zcrit <- stats::qnorm(1 - alpha/2)
  
  out <- dplyr::tibble(
    term      = zip_rows$term,
    estimate  = zip_rows$Estimate,
    std.error = zip_rows$`Std. Error`,
    statistic = zip_rows$`z value`,
    p.value   = zip_rows$`Pr(>|z|)`,
    IRR       = exp(zip_rows$Estimate),
    IRR_low   = exp(zip_rows$Estimate - zcrit * zip_rows$`Std. Error`),
    IRR_high  = exp(zip_rows$Estimate + zcrit * zip_rows$`Std. Error`),
    ZIP       = sub(prefix, "", zip_rows$term)
  )
  
  ref_row <- dplyr::tibble(
    term      = paste0(zip_var, ref_zip, " (ref)"),
    estimate  = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value   = NA_real_,
    IRR       = 1,
    IRR_low   = 1,
    IRR_high  = 1,
    ZIP       = ref_zip
  )
  
  dplyr::bind_rows(ref_row, out) %>%
    dplyr::mutate(ZIP = as.character(ZIP)) %>%
    dplyr::arrange(desc(IRR))
}

# ==== Data cleaning for counts ====

# clean_counts: Cleans and filters data for Poisson / Negative Binomial GLMs: removes non-ZIP rows, coerces count and population columns to numeric, and keeps zeros.
clean_counts <- function(df, count_col, pop_col = "Count_Person", zip_col = "ZIP") {
  df %>%
    dplyr::filter(!.data[[zip_col]] %in% c("California", "Riverside")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(count_col, pop_col)),
        as.numeric
      )
    ) %>%
    dplyr::filter(
      !is.na(.data[[pop_col]]),
      .data[[pop_col]] > 0,
      !is.na(.data[[count_col]])
    ) %>%
    dplyr::mutate(
      !!zip_col := zipify(.data[[zip_col]])
    )
}

# ==== Read HPI-style indicators ====

# read_indicator: Reads HPI-style indicators (e.g., socioeconomic factors) from a CSV file, renames columns, and ensures ZIP codes are 5 digits long.
read_indicator <- function(filename, varname) {
  path <- file.path(base_dir, filename)
  if (!file.exists(path)) stop(paste0("Missing file: ", path))
  
  read.csv(path) %>%
    dplyr::select(geoid, value) %>%
    dplyr::rename(ZIP = geoid, !!varname := value) %>%
    dplyr::mutate(
      ZIP = as.character(ZIP),
      ZIP = dplyr::if_else(
        nchar(ZIP) < 5,
        stringr::str_pad(ZIP, 5, pad = "0"),
        ZIP
      )
    )
}

# ==== OLS block for HPI predictors ====

# run_ols_block: Runs an OLS regression model with HPI predictors for a given outcome, scales the predictors, and outputs coefficients and VIF.
run_ols_block <- function(outcome_df, outcome_col, out_prefix) {
  merged <- outcome_df %>%
    dplyr::left_join(hpi, by = "ZIP") %>%
    tidyr::drop_na(ZIP, dplyr::all_of(outcome_col), dplyr::all_of(preds))
  
  if (nrow(merged) == 0) {
    stop(paste0("No rows after merge for ", out_prefix, " — check ZIPs."))
  }
  
  merged_scaled <- merged %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(preds),
        ~ as.numeric(scale(.x)),
        .names = "{.col}"
      )
    )
  
  form_full <- stats::as.formula(
    paste(outcome_col, "~", paste(preds, collapse = " + "))
  )
  
  lm_full <- stats::lm(form_full, data = merged_scaled)
  lm_step <- suppressWarnings(
    MASS::stepAIC(lm_full, direction = "both", trace = FALSE)
  )
  
  fix_zip_char <- function(df) {
    stopifnot("ZIP" %in% names(df))
    df %>% mutate(
      ZIP = as.character(ZIP),
      ZIP = ifelse(nchar(ZIP) < 5, stringr::str_pad(ZIP, 5, pad = "0"), ZIP)
    )
  }
  
  vif_tab <- tryCatch({
    v <- car::vif(lm_step)
    tibble::tibble(term = names(v), VIF = as.numeric(v))
  }, error = function(e) {
    tibble::tibble(term = names(coef(lm_step))[-1], VIF = NA_real_)
  })
  
  sm <- summary(lm_step)$coefficients
  coef_tab <- tibble::tibble(
    term      = rownames(sm),
    estimate  = sm[, "Estimate"],
    std.error = sm[, "Std. Error"],
    statistic = sm[, "t value"],
    p.value   = sm[, "Pr(>|t|)"]
  ) %>%
    dplyr::mutate(
      p_adj_BH = p.adjust(p.value, method = "BH")
    )
  
  out_dir <- "data_interim"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  readr::write_csv(
    merged_scaled %>% dplyr::select(ZIP, dplyr::all_of(outcome_col), dplyr::all_of(preds)),
    file.path(out_dir, paste0(out_prefix, "_merged_scaled.csv"))
  )
  readr::write_csv(
    coef_tab,
    file.path(out_dir, paste0(out_prefix, "_ols_coefficients.csv"))
  )
  readr::write_csv(
    vif_tab,
    file.path(out_dir, paste0(out_prefix, "_ols_vif.csv"))
  )
  
  invisible(
    list(
      model_full = lm_full,
      model_step = lm_step,
      coef_tab   = coef_tab,
      vif_tab    = vif_tab
    )
  )
}