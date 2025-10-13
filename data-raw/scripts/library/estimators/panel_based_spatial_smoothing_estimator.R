#' Panel-based spatial smoothing estimator
#'
#' This function
#'   1. Constructs spatially-varying treatment interactions (one variable per spatial unit)  
#'   2. Applies within-panel/time fixed-effects demeaning to both outcome and interactions  
#'   3. Fits an OLS model by hand (\code{lm.fit}) to recover one coefficient per spatial unit  
#'
#' @param data      A \code{data.table} or \code{data.frame} containing panel data.
#' @param output    Name of the outcome variable (character scalar).
#' @param treatment Name of the treatment variable whose spatial effects we estimate (character scalar).
#' @param time      Name of the time variable (character scalar).
#' @param panel     Name(s) of the panel identifier variable(s) (character vector).
#' @param spatialvar Name of the spatial grouping variable (e.g. county FIPS; character scalar).
#'
#' @return A \code{data.table} with columns:
#'   - \code{estimate}: the estimated spatial-unit coefficient  
#'   - \code{county_fips}: the spatial unit identifier (5-digit FIPS)  
#'   - \code{state_code}, \code{county_code}: parsed FIPS components  
#'
#' @details
#' Internally, we  
#'   1. Build \code{treatment_code} = \code{I(spatialvar==code) * treatment} for each spatial unit code.  
#'   2. Call \code{fixed_effect_model_data_prep()} to demean the outcome and all \code{treatment_code} variables.  
#'   3. Assemble the design matrix \code{X = [output_mean_i, treatment_*]} and response \code{y}.  
#'   4. Solve \eqn{\hat\beta = (\tilde X'\tilde X)^{-1}\tilde X'\tilde y} via \code{lm.fit}.  
#'   5. Return a row per spatial unit with its coefficient.  
#' @family Estimators panel models
#' @import data.table
#' @importFrom stats lm.fit
#' @export
panel_based_spatial_smoothing_estimator <- function(
    data,
    output,
    treatment,
    time,
    panel,
    spatialvar){
  
  # 1) create treatment-by-spatial-unit interaction columns
  spatialvar_list <- data[, unique(get(spatialvar))]
  data[ , paste0(treatment, '_', spatialvar_list) := 
          lapply(spatialvar_list, function(code) 
            as.integer(get(spatialvar) == code) * get(treatment)
          )
  ]
  
  # 2) demean outcome & interactions via panel/time fixed effects
  fe_prep <- fixed_effect_model_data_prep(
    data    = data,
    varlist = names(data)[grepl(paste0(treatment, '_'), names(data))],
    panel   = panel,
    time    = time,
    wvar    = spatialvar,
    output  = output
  )
  data <- fe_prep$data
  
  # 3) build response vector y and design matrix X
  y <- data[[output]]
  X <- as.matrix(data[, c(
    paste0(output, '_mean_i'),
    names(data)[grepl(paste0(treatment, '_'), names(data))]
  ), with = FALSE])
  
  # free memory
  rm(data); gc()
  
  # 4) fit OLS by hand using QR (lm.fit)
  coeffs <- lm.fit(X, y)$coefficients
  
  # 5) assemble results
  fit_df <- data.table(
    estimate     = coeffs,
    county_fips  = gsub(paste0(treatment, '_'), '', names(coeffs))
  )
  
  # keep only valid spatial units
  fit_df <- fit_df[county_fips %in% spatialvar_list]
  fit_df <- fit_df[complete.cases(fit_df)]
  
  return(fit_df)
}
