# =========================================================
# BFR demo with EXPANDED toy data & SHARES (R + data.table)
# - §1 identities on acres to build toy truth
# - Convert to SHARES s_{g,t} = a_{g,t} / TotalInsured_t  (exposure)
# - §2A Option A (no late entry) using shares
# - §3 Tiny state-space for ALL steps with covariates Z_t
# =========================================================
rm(list = ls(all = TRUE));gc()
suppressPackageStartupMessages({
  library(data.table)
  library(KFAS)
})

set.seed(42)

# ---------------------------
# 0) Core setup & "true" acres (as before; §1 identities)
# ---------------------------
years <- 2010:2024                     # 15 years to give the DLM some runway
G     <- 10
Tn    <- length(years)

# "True" transition probs to simulate BFR acres (g=1..9)
r_true <- c(0.80, 0.82, 0.83, 0.84, 0.85, 0.86, 0.88, 0.90, 0.92)

# Entry processes (allow some late entry g>1 so Option A is misspecified by design)
E_true <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
E_true[1, ] <- round(2000 * exp(-0.02 * (0:(Tn-1))) + rnorm(Tn, 0, 80))
E_true[1, ] <- pmax(E_true[1, ], 400)
for (g in 2:5) {
  E_true[g, ] <- pmax(round(runif(Tn, 40, 150) + 25*(g-2)), 0)
}

# Cohort accounting to get BFR ACRES a_{g,t}
A <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
A[1, ] <- E_true[1, ]
for (t in 2:Tn) for (g in 2:G) {
  A[g, t] <- round(r_true[g-1] * A[g-1, t-1] + E_true[g, t])
}

# ---------------------------
# 1) Add SYSTEM-WIDE covariates Z_t and exposures
# ---------------------------
# Build stylized macro/market series with mild trends & shocks
dtZ <- data.table(year = years)

# Crop price (e.g., $/bu) as a mean-reverting RW
dtZ[, crop_price := 4.5 + cumsum(rnorm(.N, 0, 0.08))]
dtZ[, crop_price := pmax(crop_price, 3.2)]

# Rental rate (e.g., $/acre), gentle uptrend
dtZ[, rental_rate := 180 + 2*(0:(.N-1)) + rnorm(.N, 0, 4)]

# Premium rate (avg) between 0.05 and 0.15, slightly procyclical with price
dtZ[, premium_rate := plogis(-2 + 0.25*scale(crop_price)) * 0.18]   # ~ 0.05–0.14

# Total planted acres (all producers)
dtZ[, planted_total := round(8e6 + 1.2e5*(0:(.N-1)) + 1.2e5*rnorm(.N), -3)]

# Total insured acres — comove with planted and premium/prices
dtZ[, insured_total := round(
  0.78*planted_total * (1 + 0.03*scale(crop_price) - 0.02*scale(premium_rate)) +
    2e5*rnorm(.N), -3)]
dtZ[insured_total < 4e6, insured_total := 4e6]   # floor

# ---------------------------
# 2) Convert BFR ACRES -> BFR SHARES by exposure (insured_total)
# ---------------------------
dtA <- as.data.table(as.table(A))
setnames(dtA, c("V1","V2","N"), c("group","year","acres_bfr"))
dtA[, group := as.integer(sub("^g","", group))]
dtA[, year  := as.integer(as.character(year))]

dt <- dtA[dtZ, on = "year"]
dt[, share_bfr := acres_bfr / insured_total]   # s_{g,t}

# ---------------------------
# 3) OPTION A (Section 2A) on SHARES
#     r̂_{g,t} = s_{g+1,t+1} / s_{g,t}
#     Exit share (10 -> out): s_{10,t+1}  (analogue to acres version)
# ---------------------------
# Precompute (g,t) and (g+1,t+1) pairs by shifting
rhs <- copy(dt)[, .(group = group - 1L, year = year - 1L, s_next = share_bfr)]
setkey(dt, group, year); setkey(rhs, group, year)

dt_r_A <- dt[rhs, nomatch = 0L][
  group %between% c(1L, G-1L) & year < max(year)
][
  , .(g = group,
      year,
      r_A = fifelse(share_bfr > 0, pmin(pmax(s_next / share_bfr, 0), 5), NA_real_)) # guard divide-by-0
]
# Exit (share) from 10 under Option A analogue
dt_exit_A <- dt[group==10][order(year)][
  , .(year, exit_share_A = shift(share_bfr, type="lead"))]

# ---------------------------
# 4) STATE-SPACE for ALL transitions (Section 3)
#     Build observed ratios on shares:
#       r_obs_{g,t} = s_{g+1,t+1} / s_{g,t}
#     Model:
#       logit(r_obs_{g,t}) = alpha_{g,t} + beta_g' Z_t + eps_t
#       alpha_{g,t} = alpha_{g,t-1} + w_t   (local level)
#     Z_t = [premium_rate, crop_price, rental_rate, planted_total, insured_total]
#     We standardize Z to help optimizer.
# ---------------------------
# 4a) Construct r_obs for all g and t using the same join as Option A
dt_r_obs <- copy(dt_r_A)[, .(g, year, r_obs = pmin(pmax(r_A, 1e-4), 1-1e-4))]  # clamp to (0,1)

# 4b) Build a Z matrix (standardized) by year, then merge in
Zcols <- c("premium_rate","crop_price","rental_rate","planted_total","insured_total")
dtZ_std <- copy(dtZ)
for (zc in Zcols) dtZ_std[, (zc) := as.numeric(scale(get(zc)))]
dt_r_obs <- dt_r_obs[dtZ_std, on = "year"]

# 4c) Fit DLM for each step g = 1..9
logit  <- function(p) log(p/(1-p))
ilogit <- function(x) 1/(1+exp(-x))

fit_step_ss <- function(g, dt_step, Zcols) {
  # keep complete cases only
  dt_step <- dt_step[complete.cases(dt_step[, c("r_obs", Zcols), with = FALSE])]
  if (nrow(dt_step) < 5) return(NULL)
  
  # response on logit scale
  y <- logit(dt_step$r_obs)
  
  # regression design via formula (single-series -> single formula)
  Xdf <- as.data.frame(dt_step[, ..Zcols])
  form <- as.formula(paste("~ -1 +", paste(colnames(Xdf), collapse = " + ")))
  
  # local level + static betas (Q=0 for regression part)
  model <- SSModel(
    y ~ SSMtrend(1, Q = list(NA)) +
      SSMregression(form, data = Xdf, Q = matrix(0, ncol(Xdf), ncol(Xdf))),
    H = NA
  )
  
  # estimate variances (level noise & obs noise)
  init_vals <- log(rep(var(y, na.rm = TRUE) * 0.1, 2))
  fit <- fitSSM(model, inits = init_vals, method = "BFGS")
  
  kfs <- KFS(fit$model, smoothing = c("state","signal"))
  
  # Smoothed mean on logit scale -> back to probability
  r_smoothed <- ilogit(as.numeric(kfs$muhat))
  
  # Static betas live in the state vector after the local level:
  p <- ncol(Xdf)
  # take the last smoothed state (constant betas) for beta-hat
  alpha_last <- as.numeric(kfs$alphahat[nrow(kfs$alphahat), ])
  beta_hat   <- alpha_last[2:(1+p)]
  
  list(
    r_ss = r_smoothed,
    coef_beta = setNames(beta_hat, colnames(Xdf))
  )
}

# Split data by step g and fit
all_steps <- split(dt_r_obs, by = "g", keep.by = FALSE)
ss_results <- vector("list", length = G-1)
names(ss_results) <- paste0("g",1:(G-1))

for (gg in 1:(G-1)) {
  dgg <- all_steps[[as.character(gg)]][order(year)]
  ss_results[[gg]] <- fit_step_ss(gg, dgg, Zcols)
}

# Collect smoothed r_{g,t}
dt_r_SS <- rbindlist(lapply(1:(G-1), function(gg) {
  dgg <- all_steps[[as.character(gg)]][order(year)]
  if (is.null(ss_results[[gg]])) return(NULL)
  data.table(g = gg, year = dgg$year, r_SS = ss_results[[gg]]$r_ss)
}), use.names = TRUE, fill = TRUE)

# ---------------------------
# 5) COMPARISONS on SHARES
# ---------------------------
# Transition rates: Option A vs State-space
dt_r_compare <- merge(dt_r_A, dt_r_SS, by = c("g","year"), all = TRUE)
setorder(dt_r_compare, g, year)

cat("\n=== Transition rates on SHARES — Option A vs State-space ===\n")
print(dt_r_compare[, .(g, year, r_A = round(r_A,4), r_SS = round(r_SS,4))])

# Exit (share) from group 10:
#   - Option A analogue: s_{10,t+1}
#   - SS-implied (using step g=9): r_SS_{9,t} * s_{9,t}
dt_r9_SS <- dt_r_SS[g==9, .(year, r9_SS = r_SS)]
dt_s9    <- dt[group==9, .(year, s9 = share_bfr)]
dt_exit_SS <- merge(dt_r9_SS, dt_s9, by="year", all=TRUE)[
  , .(year, exit_share_SS = r9_SS * shift(s9, type="lag"))]  # multiply by s_{9,t}; aligns with t+1

dt_exit_compare <- merge(dt_exit_A, dt_exit_SS, by="year", all=TRUE)
cat("\n=== Exit SHARE from group 10 — Option A vs State-space ===\n")
print(dt_exit_compare[, lapply(.SD, function(z) if(is.numeric(z)) round(z,6) else z)])

# ---------------------------
# 6) What you get & how to use it
# ---------------------------
# - dt_r_compare: time series of step-wise transition rates on shares:
#       r_A (naive §2A) vs r_SS (smoothed + Z-adjusted §3)
# - dt_exit_compare: exit share from 10 under both approaches.
# - ss_results[[g]]$coef_beta: the estimated impact of Z on logit(r_{g,t})
#   for each step g (static betas); interpret as ↑Z -> ↑/↓ transition propensity.
#
# Tip: to inspect Z effects for, say, g=4:
if (!is.null(ss_results[[4]])) {
  cat("\n=== Z-effects (betas on logit scale) for step 4->5 ===\n")
  print(ss_results[[4]]$coef_beta)
}


library(data.table)
library(ggplot2)

# ===== 1) Set forecast horizon and Z growth assumptions =====
years_fore <- 2025:2035
growth_assum <- list(
  insured_total = 0.06,
  premium_rate  = 0.002,
  crop_price    = 0.08,
  rental_rate   = 0.09,
  planted_total = 0.06
)

# ===== 2) Prepare baseline observed values at end of sample =====
# We'll forecast from the last observed year forward.
last_obs_year <- max(dt$year)
last_obs <- dt[year == last_obs_year, .(group, share_bfr, insured_total, premium_rate,
                                        crop_price, rental_rate, planted_total)]
setkey(last_obs, group)

# Store observed total insured_total for scaling back to acres
insured_total_obs <- last_obs$insured_total[1]  # same for all groups

# ===== 3) Average parameters for each model =====

## Option A: average r_A over all years for each g
rA_avg <- dt_r_A[, .(rA_avg = mean(r_A, na.rm=TRUE)), by = g]
setkey(rA_avg, g)

## State-space: average coefficients across steps
# We'll use ss_results[[g]]$coef_beta and the average smoothed level for alpha.
rSS_avg <- data.table(g = 1:(G-1))
beta_mat <- matrix(NA, nrow = G-1, ncol = length(Zcols),
                   dimnames = list(NULL, Zcols))

for (gg in 1:(G-1)) {
  if (is.null(ss_results[[gg]])) next
  # average smoothed r over time
  rSS_avg[g==gg, rSS_avg := mean(ss_results[[gg]]$r_ss, na.rm=TRUE)]
  beta_mat[gg, ] <- ss_results[[gg]]$coef_beta
}
beta_avg <- colMeans(beta_mat, na.rm=TRUE)  # average across steps

# ===== 4) Build future Z_t paths =====
Z_future <- copy(dtZ[year == last_obs_year])
for (y in years_fore) {
  Z_future <- rbind(
    Z_future,
    data.table(
      year = y,
      insured_total = Z_future$insured_total[nrow(Z_future)] * (1 + growth_assum$insured_total),
      premium_rate  = Z_future$premium_rate[nrow(Z_future)]  * (1 + growth_assum$premium_rate),
      crop_price    = Z_future$crop_price[nrow(Z_future)]    * (1 + growth_assum$crop_price),
      rental_rate   = Z_future$rental_rate[nrow(Z_future)]   * (1 + growth_assum$rental_rate),
      planted_total = Z_future$planted_total[nrow(Z_future)] * (1 + growth_assum$planted_total)
    )
  )
}
Z_future <- Z_future[-1]  # drop duplicate 2024
Z_all <- rbind(dtZ, Z_future)

# ===== 5) Forecast with Option A =====
last_obs_year <- max(dt$year)
forecast_A <- dt[year == last_obs_year, .(year, group, share_bfr)]

for (cur_year in years_fore) {
  # previous year's shares as a vector ordered by group
  prev_shares <- forecast_A[year == last_obs_year][order(group), share_bfr]
  
  # next shares: hold group 1 constant; groups 2..10 = prev(g-1) * avg r_A
  next_shares <- numeric(G)
  next_shares[1] <- prev_shares[1]
  next_shares[2:G] <- prev_shares[1:(G-1)] * rA_avg[order(g), rA_avg]
  
  # append
  forecast_A <- rbind(
    forecast_A,
    data.table(year = cur_year, group = 1:G, share_bfr = next_shares),
    use.names = TRUE
  )
  
  last_obs_year <- cur_year
}

# convert shares -> acres with forecasted insured_total
forecast_A <- merge(forecast_A, Z_all[, .(year, insured_total)], by = "year", all.x = TRUE)
forecast_A[, acres_bfr := share_bfr * insured_total]
forecast_A[, model := "Option A"]

# ===== 6) Forecast with State-space =====
# Re-init last observed year and starting shares
last_obs_year <- max(dt$year)
forecast_SS <- dt[year == last_obs_year, .(year, group, share_bfr)]

# Precompute training means/sds for standardizing Z (same as in fitting)
Z_means <- sapply(dtZ[, ..Zcols], mean)
Z_sds   <- sapply(dtZ[, ..Zcols], sd)

# Base (average) state-space transition rates by step g=1..9
r_base <- rSS_avg[order(g), rSS_avg]  # length 9

for (cur_year in years_fore) {
  # Standardize Z_t for this year
  Zt <- unlist(Z_all[year == cur_year, ..Zcols])
  Z_std <- (Zt - Z_means) / Z_sds
  
  # Adjust base rates by average betas (single set across steps)
  # r_adj_g = logistic(logit(r_base_g) + beta_avg · Z_std)
  adj_logit <- qlogis(pmin(pmax(r_base, 1e-6), 1 - 1e-6)) + sum(beta_avg * Z_std)
  r_adj <- plogis(adj_logit)  # length 9 for steps 1..9
  
  # previous year's shares (vector by group)
  prev_shares <- forecast_SS[year == last_obs_year][order(group), share_bfr]
  
  # next shares: hold group 1 constant; 2..10 from prev * r_adj
  next_shares <- numeric(G)
  next_shares[1] <- prev_shares[1]
  next_shares[2:G] <- prev_shares[1:(G-1)] * r_adj
  
  # append
  forecast_SS <- rbind(
    forecast_SS,
    data.table(year = cur_year, group = 1:G, share_bfr = next_shares),
    use.names = TRUE
  )
  
  last_obs_year <- cur_year
}

# convert shares -> acres with forecasted insured_total
forecast_SS <- merge(forecast_SS, Z_all[, .(year, insured_total)], by = "year", all.x = TRUE)
forecast_SS[, acres_bfr := share_bfr * insured_total]
forecast_SS[, model := "State-space"]


# Observed series (acres) to overlay
observed <- dt[, .(year, group, acres_bfr = share_bfr * insured_total)]

plot_dt <- rbindlist(list(
  cbind(observed, model = "Option A",    series = "Observed"),
  cbind(forecast_A[, .(year, group, acres_bfr)], model = "Option A",    series = "Forecast"),
  cbind(observed, model = "State-space", series = "Observed"),
  cbind(forecast_SS[, .(year, group, acres_bfr)], model = "State-space", series = "Forecast")
), use.names = TRUE)

ggplot(plot_dt, aes(x = year, y = acres_bfr, color = factor(group), linetype = series)) +
  geom_line() +
  facet_wrap(~ model, ncol = 1, scales = "free_y") +
  scale_linetype_manual(values = c(Observed = "solid", Forecast = "dashed")) +
  labs(title = "Observed & Forecasted BFR Acres by Group (2010–2035)",
       subtitle = "Top: Option A (avg step ratios). Bottom: State-space (avg betas + Z growth).",
       y = "BFR Acres", color = "Group", linetype = "") +
  theme_minimal()



