source("R/00_setup.R")
# R/01_Functions.R

# ==== Utility helpers ====

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

zipify <- function(x) {
  x <- as.character(x)
  stringr::str_pad(x, width = 5, pad = "0")
}

# Try negative binomial with multiple starting thetas; return NULL if all fail
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

# ==== IRR + regression table helpers ====

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

dispersion <- function(fit) {
  # Use summary() method which handles edge cases better
  sm <- summary(fit)
  
  # Check if we have valid deviance and df.residual
  if (!is.null(sm$deviance) && !is.null(sm$df.residual)) {
    if (is.finite(sm$deviance) && is.finite(sm$df.residual) && sm$df.residual > 0) {
      return(sm$deviance / sm$df.residual)
    }
  }
  
  # Fallback: manual calculation with checks
  if (is.null(fit$df.residual) || !is.finite(fit$df.residual) || fit$df.residual <= 0) {
    warning("df.residual is invalid (", fit$df.residual, 
            "). Model may be saturated. Returning NA.")
    return(NA_real_)
  }
  
  dev_resid <- residuals(fit, type = "deviance")
  if (any(!is.finite(dev_resid))) {
    warning("Non-finite deviance residuals detected")
    return(NA_real_)
  }
  
  sum(dev_resid^2) / fit$df.residual
}

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

summarize_irr <- function(x) {
  tibble::tibble(
    `Min IRR`       = min(x, na.rm = TRUE),
    `1st Quartile`  = stats::quantile(x, 0.25, na.rm = TRUE),
    `Median`        = median(x, na.rm = TRUE),
    `Mean`          = mean(x, na.rm = TRUE),
    `3rd Quartile`  = stats::quantile(x, 0.75, na.rm = TRUE),
    `Max IRR`       = max(x, na.rm = TRUE)
  )
}

# OPTIONAL: only keep this if you need ZIP-level IRRs
# pull_zip_irrs <- function(rob_tbl, fit_model, zip_var = "ZIP") {
#   mm <- model.frame(fit_model)
#   if (!zip_var %in% names(mm)) {
#     stop(paste0("Variable '", zip_var, "' not found in model frame."))
#   }
#   zips <- levels(mm[[zip_var]])
#   if (is.null(zips)) {
#     stop(paste0("Variable '", zip_var, "' is not a factor in the model."))
#   }
#   ref_zip <- zips[1]
#   prefix <- paste0(zip_var)
#   zip_rows <- rob_tbl %>%
#     dplyr::filter(startsWith(term, prefix)) %>%
#     dplyr::mutate(
#       ZIP = gsub(paste0("^", prefix), "", term)
#     )
#   ref_row <- tibble::tibble(
#     term      = paste0(zip_var, ref_zip, " (ref)"),
#     estimate  = 0,
#     std.error = NA_real_,
#     statistic = NA_real_,
#     p.value   = NA_real_,
#     IRR       = 1,
#     IRR_low   = 1,
#     IRR_high  = 1,
#     ZIP       = ref_zip
#   )
#   dplyr::bind_rows(ref_row, zip_rows)
# }

# ==== Data cleaning for counts ====

# Drop bad rows but KEEP zero counts
# zip_col, count_col, pop_col are column names (zip_col & pop_col as strings)
# Clean data for Poisson / NB GLM: drop non-ZIP rows, coerce to numeric, keep zeros

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
