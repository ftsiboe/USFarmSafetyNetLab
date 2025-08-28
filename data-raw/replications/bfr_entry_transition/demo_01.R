# =========================================================
# BFR entry/transition demo on TOY DATA (R + data.table)
# Maps to the earlier outline:
#   §1 variables/identities, §2A quick estimator, §2B late-entry + NNLS, §3 state-space flavor
# =========================================================
rm(list = ls(all = TRUE));gc()
suppressPackageStartupMessages({
  library(data.table)
  library(nnls)    # for non-negative least squares (Option B)
  library(KFAS)    # for tiny state-space demo (Bonus)
})

set.seed(42)

# ---------------------------
# 0) Create toy data a_{g,t}  (§1 setup)
# ---------------------------
years <- 2015:2024          # 10 annual observations
G     <- 10                 # groups 1..10
Tn    <- length(years)

# "True" transition probabilities r_g (constant by step; used only to make toy data)
r_true <- c(0.80, 0.82, 0.83, 0.84, 0.85, 0.86, 0.88, 0.90, 0.92)  # g=1..9

# True entries E_{g,t}, allowing late entry into g>1 (so §2A will be misspecified)
E_true <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
E_true[1, ] <- round(2000 * exp(-0.03 * (0:(Tn-1))) + rnorm(Tn, 0, 50))
E_true[1, ] <- pmax(E_true[1, ], 300)
for (g in 2:5) {
  E_true[g, ] <- pmax(round(runif(Tn, 30, 120) + 20*(g-2)), 0)
}

# Generate acres A[g,t] from cohort accounting (§1 identities)
A <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
A[1, ] <- E_true[1, ]                       # group 1 is pure entry (§1)
for (t in 2:Tn) {
  for (g in 2:G) {
    A[g, t] <- round(r_true[g-1] * A[g-1, t-1] + E_true[g, t])  # survivors + late entry (§1)
  }
}

# Implied exits from 10 (movement 10 -> out) (§1 / §4)
X10_true <- c(NA, r_true[9] * A[9, 1:(Tn-1)])  # aligns to years 2016..2024

# Stack to a long data.table: year, group, acres
dtA <- as.data.table(as.table(A))
setnames(dtA, c("V1","V2","N"), c("group","year","acres"))
dtA[, group := as.integer(sub("^g","", group))]
dtA[, year  := as.integer(as.character(year))]
setorder(dtA, group, year)

# Peek
dtA[1:12]

# =========================================================
# Option A (Section §2A): "No late entry" quick estimator
# - E_{1,t} = a_{1,t}; E_{g>1,t} = 0
# - r_hat_{g,t} = a_{g+1,t+1} / a_{g,t}
# - X10_hat_t   = a_{10,t+1}
# =========================================================

# Build a self-join to produce (a_{g,t}, a_{g+1,t+1}) pairs
# 1) Make a RHS table that already has (group-1, year-1) as keys
rhs <- copy(dtA)[
  , .(group = group - 1L, year = year - 1L, acres_next = acres)
]

# 2) Join on plain columns, keep only valid g=1..9 and years excluding the last
setkey(dtA, group, year)
setkey(rhs, group, year)

dt_ratiosA <- dtA[rhs, nomatch = 0L][
  group >= 1L & group <= (G - 1L) & year < max(years)
][
  , .(g = group, year, r_hat = fifelse(acres > 0, acres_next / acres, NA_real_))
]

dt_ratiosA[]

# Entries under §2A
dt_EhatA <- data.table(expand.grid(group = 1:G, year = years))
dt_EhatA[, Ehat := 0.0]
dt_EhatA[group == 1, Ehat := dtA[group == 1, acres]]

# Exit from 10 (a_{10,t+1})
dt_X10_A <- data.table(year = years, X10_hat = c(NA_real_, dtA[group==10 & year>min(years), acres]))

cat("\n=== Option A: sample of transition ratios r_hat_{g,t} ===\n")
print(dt_ratiosA[1:10])

cat("\n=== Option A: Exit from group 10 (acres), by year ===\n")
print(dt_X10_A)

# =========================================================
# Option B (Section §2B): Allow late entry via pooled OLS + NNLS
# - Estimate constant r_g by pooled slope through the origin:
#     a_{g+1,t+1} = r_g * a_{g,t} + error
# - Recover E_{g,t} as residuals:
#     E_{1,t}=a_{1,t}; E_{g>=2,t} = a_{g,t} - r_{g-1} * a_{g-1,t-1}
# - Enforce E_{g,t} >= 0 via Non-Negative Least Squares (NNLS) re-fit
# - Exit: X10_t = r_9 * a_{9,t}
# =========================================================

# Prepare stepwise (x=a_{g,t}, y=a_{g+1,t+1}) pairs for all g=1..9
# We'll use joins to align indices.
make_step_xy_DT <- function(g) {
  # left table: a_{g,t} for t <= T-1
  L <- dtA[group == g & year < max(years), .(year, x = acres)]
  # right table: a_{g+1,t+1}
  R <- dtA[group == (g+1) & year > min(years), .(year_next = year, y = acres)]
  setkey(L, year); setkey(R, year_next)
  # join on year_next == year + 1 (non-equi join)
  # data.table doesn't join with expressions directly; shift years
  L[, year_next := year + 1L]
  ans <- L[R, on = .(year_next), nomatch = 0L][, .(t = year, x, y)]
  ans[]
}

steps_xy <- lapply(1:(G-1), make_step_xy_DT)

# Through-origin OLS slope for each step
rhat_B_OLS <- sapply(steps_xy, function(d) {
  if (nrow(d) == 0) return(NA_real_)
  coef(lm(y ~ x - 1, data = d))
})
names(rhat_B_OLS) <- paste0("r_", 1:(G-1))

# Recover entries as residuals with OLS r_g
# E_{1,t} = a_{1,t}; for g>=2: E_{g,t} = a_{g,t} - r_{g-1} * a_{g-1,t-1}
dt_EhatB <- copy(dtA)[, .(group, year, acres)]
dt_EhatB[, Ehat := 0.0]
dt_EhatB[group == 1, Ehat := acres]  # identity (§2B)

# For g>=2, join a_{g,t} with lagged a_{g-1,t-1}
lagDT <- copy(dtA)[, .(group_lag = group, year_lag = year, acres_lag = acres)]
setkey(lagDT, group_lag, year_lag)
tmp <- copy(dtA)[group >= 2]
tmp[, `:=`(group_lag = group - 1L, year_lag = year - 1L)]
tmp <- tmp[lagDT, on = .(group_lag, year_lag)]
# Add r_{g-1} (OLS)
tmp[, r_hat := rhat_B_OLS[group_lag]]
tmp[, E_calc := acres - r_hat * acres_lag]
# write back
dt_EhatB[tmp, on = .(group, year), Ehat := pmax(E_calc, NA_real_)]  # may be negative here; next we NNLS

# NNLS re-fit per step g>=2 to ensure E >= 0 and r in [0,1]
rhat_B_NNLS <- rep(NA_real_, G-1)
for (g in 2:G) {
  # Build y = a_{g,t}, x = a_{g-1,t-1} for t>=2
  y <- dtA[group == g & year >= (min(years)+1), acres]
  x <- dtA[group == (g-1) & year <= (max(years)-1), acres]
  # Align lengths (both should be Tn-1)
  if (length(y) == 0 || length(x) == 0) next
  fit <- nnls(as.matrix(x), y)        # min ||y - x*b|| s.t. b>=0
  rhat_B_NNLS[g-1] <- as.numeric(coef(fit))
  # Replace Ehat using NNLS residuals (nonnegative by construction)
  resid <- as.numeric(y - x * coef(fit))
  dt_EhatB[group == g & year >= (min(years)+1), Ehat := resid]
}
# Clip r to [0,1]
rhat_B_NNLS <- pmin(pmax(rhat_B_NNLS, 0), 1)
names(rhat_B_NNLS) <- paste0("r_", 1:(G-1))

# Exit after 10 (movement 10 -> out): X10_t = r_9 * a_{9,t}
dt_X10_B <- dtA[group==9, .(year, X10_hat = c(NA_real_, rhat_B_NNLS[9] * acres[-.N]))]
# Re-align to full set of years
dt_X10_B <- rbindlist(list(
  data.table(year = years[1], X10_hat = NA_real_),
  data.table(year = years[-1], X10_hat = rhat_B_NNLS[9] * dtA[group==9 & year < max(years), acres])
))

cat("\n=== Option B: pooled slopes r_g (OLS through origin) ===\n")
print(round(rhat_B_OLS, 4))

cat("\n=== Option B: pooled slopes r_g (NNLS, clipped to [0,1]) ===\n")
print(round(rhat_B_NNLS, 4))

cat("\n=== Option B: sample of recovered entries E_{g,t} (acres) ===\n")
print(dcast(dt_EhatB[group <= 5 & year %between% c(2019, 2024)], group ~ year, value.var = "Ehat"))

cat("\n=== Option B: Exit from group 10 (acres), by year ===\n")
print(dt_X10_B)

# =========================================================
# (Bonus) Tiny state-space flavor (Section §3):
#   Single step (3 -> 4): r_{3,t} evolves (local level).
#   Observation: a_{4,t} = r_{3,t-1} * a_{3,t-1} + noise  (late entry bundled in noise here)
#   For a full build, stack all steps and optionally explicit E_{g,t} states.
# =========================================================

g_demo <- 3
y <- dtA[group == (g_demo+1) & year >= (min(years)+1), acres]  # a_{4,t}, t=2016..2024
x <- dtA[group == g_demo       & year <= (max(years)-1), acres] # a_{3,t-1}, aligned

# State-space: y_t = x_t * r_t + v_t, r_t = r_{t-1} + w_t
Zt <- array(x, dim = c(1,1,length(x)))
Tt <- 1; Rt <- 1
Qt <- array(NA_real_, c(1,1))
Ht <- matrix(NA_real_)

init_r <- if (is.finite(rhat_B_NNLS[g_demo])) rhat_B_NNLS[g_demo] else 0.8
model <- SSModel(y ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = matrix(init_r,1,1)), H = Ht)
fit   <- fitSSM(model, inits = log(c(var(y)*0.01, var(y)*0.1)), method = "BFGS")
kfs   <- KFS(fit$model, smoothing = "state")
r_smoothed <- as.numeric(kfs$alphahat)

cat("\n=== State-space demo (step 3->4): smoothed r_{3,t} ===\n")
print(data.table(year = years[-1], r3_smoothed = round(r_smoothed, 4)))

# ============================================
# COMPARISON TABLES (Options A vs B, plus SS)
# ============================================

# -- 1) Transition rates r_{g,t} by step & year
# Option A is year-specific ratio; Option B is pooled constant r_g repeated across years.
# (§2A vs §2B; SS shown later for one step)
dt_r_A <- copy(dt_ratiosA)[, .(g, year, r_A = r_hat)]
# Build a table with Option B's r_g repeated across the same (g, year) support as A
dt_r_B <- dt_r_A[, .(g, year)][, r_B := rhat_B_NNLS[g], by = g]

dt_r_compare <- merge(dt_r_A, dt_r_B, by = c("g","year"), all = TRUE)
setorder(dt_r_compare, g, year)

cat("\n=== Transition rates r_{g,t} — Option A vs Option B ===\n")
print(dt_r_compare[, .(g, year, r_A = round(r_A,4), r_B = round(r_B,4))])

# If you also want the OLS (through-origin) version next to NNLS:
dt_r_compare_OLS <- dt_r_compare[, r_B_OLS := rhat_B_NNLS[g]*NA_real_][, r_B_OLS := NA_real_]
dt_r_compare_OLS[, r_B_OLS := sapply(g, function(gg) rhat_B_OLS[gg])]
cat("\n=== Transition rates r_{g,t} — Option A vs Option B (NNLS and OLS) ===\n")
print(dt_r_compare[, .(g, year,
                       r_A   = round(r_A,4),
                       r_B   = round(r_B,4),
                       r_B_OLS = round(dt_r_compare_OLS$r_B_OLS,4))])

# -- 2) Exit from group 10 by year (movement 10 -> out)
# (§2A uses a_{10,t+1}; §2B uses r_9 * a_{9,t})
dt_exit_compare <- merge(
  copy(dt_X10_A)[, .(year, X10_A = X10_hat)],
  copy(dt_X10_B)[, .(year, X10_B = X10_hat)],
  by = "year", all = TRUE
)
# If you generated X10_true in the toy data section, include it too:
if (exists("X10_true")) {
  dt_exit_compare[, X10_true := X10_true]
}

setorder(dt_exit_compare, year)
cat("\n=== Exit from group 10 — Option A vs Option B (acres) ===\n")
print(dt_exit_compare[, lapply(.SD, function(z) if(is.numeric(z)) round(z,1) else z)])

# -- 3) Entries E_{g,t} by group/year
# Option A: only group 1 has entries; Option B: recovered entries for all groups.
dt_E_A <- dcast(dt_EhatA, group ~ year, value.var = "Ehat")
setnames(dt_E_A, old = setdiff(names(dt_E_A), "group"),
         new = paste0("A_", setdiff(names(dt_E_A), "group")))

dt_E_B <- dcast(dt_EhatB, group ~ year, value.var = "Ehat")
setnames(dt_E_B, old = setdiff(names(dt_E_B), "group"),
         new = paste0("B_", setdiff(names(dt_E_B), "group")))

dt_E_compare <- merge(dt_E_A, dt_E_B, by = "group", all = TRUE)

cat("\n=== Entries E_{g,t} — Option A vs Option B (acres) ===\n")
# Printing all years can be wide; feel free to subset years if desired.
print(dt_E_compare[, lapply(.SD, function(z) if(is.numeric(z)) round(z,1) else z)])

# -- 4) (Bonus) State-space comparison for step 3 -> 4
# Shows Option A r_{3,t}, Option B r_3 (constant), and smoothed r_{3,t} from KFAS (§3).
g_demo <- 3
years_t <- dt_r_compare[g == g_demo, year]
rA_3    <- dt_r_compare[g == g_demo, r_A]
rB_3    <- dt_r_compare[g == g_demo, r_B][1L]  # constant across years
if (exists("r_smoothed")) {
  # r_smoothed aligns with years[-1] in the earlier code
  dt_ss <- data.table(year = years[-1], r_SS = as.numeric(r_smoothed))
} else {
  dt_ss <- data.table(year = years_t, r_SS = NA_real_)
}

dt_step3 <- data.table(
  year = years_t,
  r_A  = rA_3,
  r_B  = rB_3
)[dt_ss, on = "year"]

cat("\n=== Step 3->4: r_{3,t} — Option A vs Option B vs State-space ===\n")
print(dt_step3[, .(year,
                   r_A = round(r_A,4),
                   r_B = round(r_B,4),
                   r_SS = round(r_SS,4))])
