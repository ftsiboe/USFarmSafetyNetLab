# =========================================================
# BFR Trends & Outlook: Shares-based Entry/Transition + Forecast Demo
# =========================================================
# What this script does
# ---------------------
# 1) Generates toy Beginning Farmer & Rancher (BFR) data on insured ACRES by BFR group (1–10),
#    plus system-wide covariates Z_t (premium rate, crop price, rental rate, planted & insured acres).
# 2) Converts BFR acres to SHARES s_{g,t} = a_{g,t} / insured_total_t (exposure-adjusted).
# 3) Option A (Section 2A): computes adjacent-year share ratios r_{g,t} = s_{g+1,t+1}/s_{g,t}.
# 4) State-space (Section 3): fits one model per step g (1..9):
#       logit(r_{g,t}) = level_t + beta_g' Z_t + noise
#    with a local-level state and static betas on standardized Z.
# 5) Forecasts 2025–2035 using user-provided annual growth rates for Z_t.
# 6) Projects BFR SHARES then converts to ACRES, for both Option A and State-space models.
# 7) Plots observed (2010–2024) and forecasted (2025–2035) BFR acres by group in two panels.
#
# Acknowledgment of prompts / provenance
# --------------------------------------
# This script was iteratively designed with ChatGPT’s assistance based on prompts to:
# - lay out cohort accounting identities and estimators for BFR flows (entries, g->g+1, and exit after g=10),
# - demonstrate Option A (no-late-entry) and a state-space alternative,
# - switch to data.table, add covariates (Z_t), and work in SHARES rather than raw acres,
# - compare models, interpret outputs, and finally build multi-year forecasts and plots.
#
# Notes
# -----
# - The toy data intentionally includes small late entry into groups 2..5 to stress-test Option A.
# - Group 1 share is held constant in forecasts for clarity (can be extended with a simple entry model).
# - All Z_t used in the state-space are standardized during fitting; forecasts standardize using
#   the observed-sample mean/sd to stay on the same scale.
# =========================================================

rm(list = ls(all = TRUE)); gc()
suppressPackageStartupMessages({
  library(data.table)
  library(KFAS)
  library(ggplot2)
})

# ---------- small utilities ----------
#' Logit transform
#' @param p numeric vector in (0,1)
#' @return numeric log-odds
logit  <- function(p) log(p/(1-p))

#' Inverse-logit (logistic) transform
#' @param x numeric vector
#' @return numeric vector in (0,1)
ilogit <- function(x) 1/(1+exp(-x))

# =========================================================
# 0) DATA GENERATION (toy truth)
# =========================================================

#' Generate toy BFR dataset with system-wide covariates and shares
#'
#' @title Generate toy BFR data
#' @description Creates synthetic BFR acres by group and year using cohort-flow identities,
#' adds market/system covariates (premium rate, crop price, rental rate, planted & insured acres),
#' and computes exposure-adjusted BFR shares s_{g,t} = a_{g,t} / insured_total_t.
#' @param seed integer random seed
#' @param years integer vector of years (default 2010:2024)
#' @param G integer number of BFR groups (default 10)
#' @return list with:
#' \itemize{
#'   \item dt: long data.table (year, group, acres_bfr, Z_t, share_bfr)
#'   \item dtZ: data.table of system covariates by year
#'   \item G: number of groups
#' }
#' @examples
#' toy <- gen_toy_data(); str(toy$dt)
gen_toy_data <- function(seed = 42, years = 2010:2024, G = 10) {
  set.seed(seed)
  Tn <- length(years)
  
  # true step-wise retention (for simulation only)
  r_true <- c(0.80, 0.82, 0.83, 0.84, 0.85, 0.86, 0.88, 0.90, 0.92)
  
  # entries with small late-entry in g=2..5
  E_true <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
  E_true[1, ] <- round(2000 * exp(-0.02 * (0:(Tn-1))) + rnorm(Tn, 0, 80))
  E_true[1, ] <- pmax(E_true[1, ], 400)
  for (g in 2:5) E_true[g, ] <- pmax(round(runif(Tn, 40, 150) + 25*(g-2)), 0)
  
  # cohort accounting -> acres a_{g,t}
  A <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
  A[1, ] <- E_true[1, ]
  for (t in 2:Tn) for (g in 2:G) A[g, t] <- round(r_true[g-1] * A[g-1, t-1] + E_true[g, t])
  
  # system-wide Z_t
  dtZ <- data.table(year = years)
  dtZ[, crop_price   := 4.5 + cumsum(rnorm(.N, 0, 0.08))]
  dtZ[, crop_price   := pmax(crop_price, 3.2)]
  dtZ[, rental_rate  := 180 + 2*(0:(.N-1)) + rnorm(.N, 0, 4)]
  dtZ[, premium_rate := plogis(-2 + 0.25*scale(crop_price)) * 0.18]   # ~0.05–0.14
  dtZ[, planted_total:= round(8e6 + 1.2e5*(0:(.N-1)) + 1.2e5*rnorm(.N), -3)]
  dtZ[, insured_total:= round(0.78*planted_total * (1 + 0.03*scale(crop_price) - 0.02*scale(premium_rate))
                              + 2e5*rnorm(.N), -3)]
  dtZ[insured_total < 4e6, insured_total := 4e6]
  
  # long table: acres + Z + shares
  dtA <- as.data.table(as.table(A))
  setnames(dtA, c("V1","V2","N"), c("group","year","acres_bfr"))
  dtA[, group := as.integer(sub("^g","", group))]
  dtA[, year  := as.integer(as.character(year))]
  dt <- dtA[dtZ, on = "year"]
  dt[, share_bfr := acres_bfr / insured_total]
  
  list(dt = dt, dtZ = dtZ, G = G)
}

# =========================================================
# 1) OPTION A (adjacent-year share ratios)
# =========================================================

#' Option A: compute adjacent-year share ratios and exit share
#'
#' @title Option A rates & exits
#' @description Computes r_{g,t} = s_{g+1,t+1}/s_{g,t} on shares (guarding against zeros)
#' and the proxy exit share from group 10 as s_{10,t+1}.
#' @param dt long data.table with share_bfr and year, group
#' @param G integer number of groups
#' @return list with:
#' \itemize{
#'   \item dt_r_A: data.table (g, year, r_A)
#'   \item dt_exit_A: data.table (year, exit_share_A)
#' }
#' @examples
#' optA <- optionA_rates(toy$dt, toy$G); head(optA$dt_r_A)
optionA_rates <- function(dt, G) {
  rhs <- copy(dt)[, .(group = group - 1L, year = year - 1L, s_next = share_bfr)]
  setkey(dt, group, year); setkey(rhs, group, year)
  dt_r_A <- dt[rhs, nomatch = 0L][
    group %between% c(1L, G-1L) & year < max(year)
  ][
    , .(g = group, year,
        r_A = fifelse(share_bfr > 0, pmin(pmax(s_next / share_bfr, 0), 5), NA_real_))
  ]
  dt_exit_A <- dt[group==10][order(year)][, .(year, exit_share_A = shift(share_bfr, type = "lead"))]
  list(dt_r_A = dt_r_A, dt_exit_A = dt_exit_A)
}

# =========================================================
# 2) STATE-SPACE (all steps)
# =========================================================

#' Fit state-space models for all transitions g=1..9
#'
#' @title State-space fit for step-wise share transitions
#' @description For each step g, fits a single-series local-level + static-regression model on
#' standardized Z_t: logit(r_{g,t}) = level_t + beta_g' Z_t + eps_t. Returns smoothed r_{g,t}
#' and beta_g estimates.
#' @param dt long data.table with shares (share_bfr), year, group
#' @param dtZ data.table of Z covariates by year
#' @param dt_r_A data.table of observed Option A ratios (g, year, r_A)
#' @param G integer number of groups
#' @return list with:
#' \itemize{
#'   \item dt_r_SS: data.table (g, year, r_SS) smoothed transitions
#'   \item ss_results: list per step g with elements \code{r_ss} and \code{beta}
#'   \item Zcols: character vector of Z column names used
#' }
#' @examples
#' ss_fit <- fit_statespace(toy$dt, toy$dtZ, optA$dt_r_A, toy$G)
fit_statespace <- function(dt, dtZ, dt_r_A, G) {
  dt_r_obs <- copy(dt_r_A)[, .(g, year, r_obs = pmin(pmax(r_A, 1e-4), 1-1e-4))]
  Zcols <- c("premium_rate","crop_price","rental_rate","planted_total","insured_total")
  dtZ_std <- copy(dtZ)
  for (zc in Zcols) dtZ_std[, (zc) := as.numeric(scale(get(zc)))]
  dt_r_obs <- dt_r_obs[dtZ_std, on = "year"]
  
  fit_step <- function(g, d) {
    d <- d[complete.cases(d[, c("r_obs", Zcols), with = FALSE])]
    if (nrow(d) < 5) return(NULL)
    y <- logit(d$r_obs)
    Xdf <- as.data.frame(d[, ..Zcols])
    form <- as.formula(paste("~ -1 +", paste(colnames(Xdf), collapse = " + ")))
    model <- SSModel(
      y ~ SSMtrend(1, Q = list(NA)) +
        SSMregression(form, data = Xdf, Q = matrix(0, ncol(Xdf), ncol(Xdf))),
      H = NA
    )
    init_vals <- log(rep(var(y, na.rm = TRUE) * 0.1, 2))
    fit <- fitSSM(model, inits = init_vals, method = "BFGS")
    kfs <- KFS(fit$model, smoothing = c("state","signal"))
    r_ss <- ilogit(as.numeric(kfs$muhat))
    p <- ncol(Xdf)
    alpha_last <- as.numeric(kfs$alphahat[nrow(kfs$alphahat), ])
    beta_hat   <- alpha_last[2:(1+p)]
    list(r_ss = r_ss, beta = setNames(beta_hat, colnames(Xdf)))
  }
  
  by_step <- split(dt_r_obs, by = "g", keep.by = FALSE)
  ss_results <- vector("list", length = G-1)
  names(ss_results) <- paste0("g",1:(G-1))
  for (gg in 1:(G-1)) {
    dgg <- by_step[[as.character(gg)]][order(year)]
    ss_results[[gg]] <- fit_step(gg, dgg)
  }
  dt_r_SS <- rbindlist(lapply(1:(G-1), function(gg){
    dgg <- by_step[[as.character(gg)]][order(year)]
    if (is.null(ss_results[[gg]])) return(NULL)
    data.table(g = gg, year = dgg$year, r_SS = ss_results[[gg]]$r_ss)
  }), use.names = TRUE, fill = TRUE)
  
  list(dt_r_SS = dt_r_SS, ss_results = ss_results, Zcols = Zcols)
}

# =========================================================
# 3) FUTURE Z PATHS
# =========================================================

#' Build future Z_t by deterministic growth
#'
#' @title Grow Z series forward
#' @description Extends Z_t from the last observed year through \code{years_fore} with
#' compound annual growth for each variable.
#' @param dtZ observed Z data.table
#' @param years_fore integer vector of future years
#' @param growth named list of growth rates (e.g., list(insured_total=0.06, ...))
#' @return data.table Z_all with observed and future years
#' @examples
#' Z_all <- build_future_Z(toy$dtZ, 2025:2030, list(insured_total=0.06, premium_rate=0.002,
#'                                                  crop_price=0.08, rental_rate=0.09,
#'                                                  planted_total=0.06))
build_future_Z <- function(dtZ, years_fore, growth) {
  last_y <- max(dtZ$year)
  Z_future <- copy(dtZ[year == last_y])
  for (y in years_fore) {
    Z_future <- rbind(
      Z_future,
      data.table(
        year = y,
        insured_total = Z_future$insured_total[.N] * (1 + growth$insured_total),
        premium_rate  = Z_future$premium_rate[.N]  * (1 + growth$premium_rate),
        crop_price    = Z_future$crop_price[.N]    * (1 + growth$crop_price),
        rental_rate   = Z_future$rental_rate[.N]   * (1 + growth$rental_rate),
        planted_total = Z_future$planted_total[.N] * (1 + growth$planted_total)
      )
    )
  }
  Z_future <- Z_future[-1]
  rbind(dtZ, Z_future)
}

# =========================================================
# 4) FORECAST SHARES/ACRES
# =========================================================

#' Forecast shares & acres under Option A (average step ratios)
#'
#' @title Forecast with Option A
#' @description Projects BFR shares 2025–2035 by holding group-1 share constant and
#' applying average historical r_A per step to propagate shares (g-1 -> g). Converts shares
#' to acres using forecasted insured_total.
#' @param dt long data.table (observed), must include share_bfr, year, group
#' @param dt_r_A Option A ratios table (g, year, r_A)
#' @param Z_all data.table of Z (with insured_total) for observed+future years
#' @param G number of groups
#' @param years_fore vector of future years
#' @return data.table with year, group, share_bfr, acres_bfr, model="Option A"
#' @examples
#' fc_A <- forecast_optionA(toy$dt, optA$dt_r_A, Z_all, toy$G, 2025:2030)
forecast_optionA <- function(dt, dt_r_A, Z_all, G, years_fore) {
  rA_avg <- dt_r_A[, .(rA_avg = mean(r_A, na.rm = TRUE)), by = g][order(g)]
  last_y <- max(dt$year)
  fc <- dt[year == last_y, .(year = last_y, group, share_bfr)]
  for (yr in years_fore) {
    prev <- fc[year == last_y][order(group), share_bfr]
    next_sh <- numeric(G)
    next_sh[1] <- prev[1]
    next_sh[2:G] <- prev[1:(G-1)] * rA_avg$rA_avg
    fc <- rbind(fc, data.table(year = yr, group = 1:G, share_bfr = next_sh), use.names = TRUE)
    last_y <- yr
  }
  fc <- fc[Z_all, on = "year"][, acres_bfr := share_bfr * insured_total][, model := "Option A"]
  fc[]
}

#' Forecast shares & acres under the State-space model
#'
#' @title Forecast with State-space
#' @description Uses average smoothed base rates per step and average betas across steps to
#' adjust the base rate by forecast Z_t (standardized with observed-sample mean/sd),
#' then propagates shares forward and converts to acres with insured_total.
#' @param dt long data.table (observed), must include share_bfr, year, group
#' @param Z_all data.table of Z (with insured_total) for observed+future years
#' @param ss_results list of per-step state-space results (from \code{fit_statespace})
#' @param dtZ observed Z table (used to standardize future Z)
#' @param years_fore vector of future years
#' @param G number of groups
#' @return data.table with year, group, share_bfr, acres_bfr, model="State-space"
#' @examples
#' fc_SS <- forecast_statespace(toy$dt, Z_all, ss_fit$ss_results, toy$dtZ, 2025:2030, toy$G)
forecast_statespace <- function(dt, Z_all, ss_results, dtZ, years_fore, G) {
  r_base <- sapply(1:(G-1), function(g) mean(ss_results[[g]]$r_ss, na.rm = TRUE))
  beta_mat <- do.call(rbind, lapply(1:(G-1), function(g) ss_results[[g]]$beta))
  beta_avg <- colMeans(beta_mat, na.rm = TRUE)
  
  Zcols <- names(beta_avg)
  Z_means <- sapply(dtZ[, ..Zcols], mean)
  Z_sds   <- sapply(dtZ[, ..Zcols], sd)
  
  last_y <- max(dt$year)
  fc <- dt[year == last_y, .(year = last_y, group, share_bfr)]
  for (yr in years_fore) {
    Zt <- unlist(Z_all[year == yr, ..Zcols])
    Z_std <- (Zt - Z_means) / Z_sds
    adj_logit <- qlogis(pmin(pmax(r_base, 1e-6), 1-1e-6)) + sum(beta_avg * Z_std)
    r_adj <- plogis(adj_logit)
    
    prev <- fc[year == last_y][order(group), share_bfr]
    next_sh <- numeric(G)
    next_sh[1] <- prev[1]
    next_sh[2:G] <- prev[1:(G-1)] * r_adj
    fc <- rbind(fc, data.table(year = yr, group = 1:G, share_bfr = next_sh), use.names = TRUE)
    last_y <- yr
  }
  fc <- fc[Z_all, on = "year"][, acres_bfr := share_bfr * insured_total][, model := "State-space"]
  fc[]
}

# =========================================================
# 5) PLOTTER
# =========================================================

#' Plot observed & forecasted BFR acres by group in two panels
#'
#' @title Plot observed vs. forecasted acres
#' @description Creates a two-panel ggplot: one panel for Option A and one for State-space,
#' each showing observed (solid) and forecasted (dashed) BFR acres by group.
#' @param dt observed long table with shares & insured_total
#' @param fc_A forecast table from \code{forecast_optionA}
#' @param fc_SS forecast table from \code{forecast_statespace}
#' @return a ggplot object
#' @examples
#' p <- plot_two_panels(toy$dt, fc_A, fc_SS); print(p)
plot_two_panels <- function(dt, fc_A, fc_SS) {
  observed <- dt[, .(year, group, acres_bfr = share_bfr * insured_total)]
  plot_dt <- rbindlist(list(
    cbind(observed, model = "Option A",    series = "Observed"),
    cbind(fc_A[, .(year, group, acres_bfr)], model = "Option A",    series = "Forecast"),
    cbind(observed, model = "State-space", series = "Observed"),
    cbind(fc_SS[, .(year, group, acres_bfr)], model = "State-space", series = "Forecast")
  ), use.names = TRUE)
  
  ggplot(plot_dt, aes(x = year, y = acres_bfr, color = factor(group), linetype = series)) +
    geom_line() +
    facet_wrap(~ model, ncol = 1, scales = "free_y") +
    scale_linetype_manual(values = c(Observed = "solid", Forecast = "dashed")) +
    labs(title = "Observed & Forecasted BFR Acres by Group (2010–2035)",
         subtitle = "Top: Option A (avg step ratios). Bottom: State-space (avg betas + Z growth).",
         y = "BFR Acres", color = "Group", linetype = "") +
    theme_minimal()
}

# =========================================================
# RUN PIPELINE
# =========================================================
toy      <- gen_toy_data()
dt       <- toy$dt
dtZ      <- toy$dtZ
G        <- toy$G

optA       <- optionA_rates(dt, G)
dt_r_A     <- optA$dt_r_A
dt_exit_A  <- optA$dt_exit_A

ss_fit     <- fit_statespace(dt, dtZ, dt_r_A, G)
dt_r_SS    <- ss_fit$dt_r_SS
ss_results <- ss_fit$ss_results
Zcols      <- ss_fit$Zcols

# quick peeks (optional)
dt_r_compare <- merge(dt_r_A, dt_r_SS, by = c("g","year"), all = TRUE)
dt_r9_SS <- dt_r_SS[g==9, .(year, r9_SS = r_SS)]
dt_s9    <- dt[group==9, .(year, s9 = share_bfr)]
dt_exit_SS <- merge(dt_r9_SS, dt_s9, by="year", all=TRUE)[
  , .(year, exit_share_SS = r9_SS * shift(s9, type="lag"))]
dt_exit_compare <- merge(dt_exit_A, dt_exit_SS, by="year", all=TRUE)

cat("\n--- Transition rates on SHARES (sample) ---\n")
print(head(dt_r_compare[, .(g, year, r_A = round(r_A,3), r_SS = round(r_SS,3))], 8))
cat("\n--- Exit SHARE from G10 (sample) ---\n")
print(head(dt_exit_compare, 8))

# Forecast settings
years_fore <- 2025:2035
growth_assum <- list(
  insured_total = 0.06,  # +6%/yr
  premium_rate  = 0.002, # +0.2%/yr
  crop_price    = 0.08,  # +8%/yr
  rental_rate   = 0.09,  # +9%/yr
  planted_total = 0.06   # +6%/yr
)
Z_all <- build_future_Z(dtZ, years_fore, growth_assum)

# Forecasts
fc_A  <- forecast_optionA(dt, dt_r_A, Z_all, G, years_fore)
fc_SS <- forecast_statespace(dt, Z_all, ss_results, dtZ, years_fore, G)

# Plot
p <- plot_two_panels(dt, fc_A, fc_SS)
print(p)

# =========================================================
# EXAMPLE INTERPRETATIONS (numeric)
# =========================================================
# 1) Transition rates (example: step 3->4 in 2018)
ex1 <- dt_r_compare[g==3 & year==2018, .(r_A = round(r_A,3), r_SS = round(r_SS,3))]
if (nrow(ex1)) {
  cat("\nIn 2018, r_A(3->4)=", ex1$r_A, " vs r_SS(3->4)=", ex1$r_SS,
      " meaning the raw adjacent-year ratio suggests ~", round(ex1$r_A*100),
      "% of the share moves 3->4, while the smoothed, Z-adjusted model suggests ~",
      round(ex1$r_SS*100), "%.\n", sep="")
}

# 2) Exit share from group 10 in 2020 (and acres conversion)
ex2 <- dt_exit_compare[year==2020]
if (nrow(ex2)) {
  insured_2020 <- dtZ[year==2020, insured_total]
  cat("In 2020, exit_share_A=", round(ex2$exit_share_A, 5),
      " vs exit_share_SS=", round(ex2$exit_share_SS, 5),
      "; acres ≈ ", round(ex2$exit_share_A * insured_2020), " (A) vs ",
      round(ex2$exit_share_SS * insured_2020), " (SS).\n", sep="")
}

# 3) Z-effects (betas) translation: +1 SD crop_price for step 4->5
g_demo <- 4
if (!is.null(ss_results[[g_demo]])) {
  bet <- ss_results[[g_demo]]$beta
  r_base_demo <- median(dt_r_SS[g==g_demo, r_SS], na.rm=TRUE)
  bump <- ilogit(qlogis(r_base_demo) + bet["crop_price"]) - r_base_demo
  cat("Step ", g_demo, "->", g_demo+1, ": +1 SD crop_price changes log-odds by ",
      round(bet["crop_price"],3), " (~", round(100*bump,1),
      " p.p. around baseline ", round(100*r_base_demo,1), "%).\n", sep="")
}
