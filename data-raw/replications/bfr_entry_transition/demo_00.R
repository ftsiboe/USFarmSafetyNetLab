# =========================================================
# BFR entry/transition demo on TOY DATA (R)
# Maps to the earlier outline:
#   - §1 variables, §2A quick estimator, §2B late-entry estimator, §3 state-space flavor
# =========================================================

set.seed(42)

# ---------------------------
# 0) Create toy data a_{g,t}
# ---------------------------
years  <- 2015:2024              # 10 annual observations
G      <- 10                     # groups 1..10
Tn     <- length(years)

# "True" transition probabilities r_g (constant by step; §2B / §3 base)
r_true <- c(0.80, 0.82, 0.83, 0.84, 0.85, 0.86, 0.88, 0.90, 0.92)  # for g=1..9

# Build true entry processes E_{g,t}; allow some late entry into g>1 (as in §2B)
E_true <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))
# baseline entry into group 1 declines a bit, with noise
E_true[1, ] <- round(2000 * exp(-0.03 * (0:(Tn-1))) + rnorm(Tn, 0, 50))
E_true[1, ] <- pmax(E_true[1, ], 300)

# sprinkle small late entry into groups 2..5, mimicking program changes
for (g in 2:5) {
  E_true[g, ] <- pmax(round(runif(Tn, 30, 120) + 20*(g-2)), 0)
}

# Generate acres a_{g,t} using the cohort accounting from §1
A <- array(0, dim = c(G, Tn), dimnames = list(paste0("g",1:G), years))

# group 1 is purely entry (§1)
A[1, ] <- E_true[1, ]

# groups 2..10 aggregate prior survivors + new late entrants (§1)
for (t in 2:Tn) {
  for (g in 2:G) {
    A[g, t] <- round(r_true[g-1] * A[g-1, t-1] + E_true[g, t])
  }
}

# The implied exit from group 10 each year t (movement 10 -> out; §1 and §4)
X10_true <- c(NA, r_true[9] * A[9, 1:(Tn-1)])   # aligns with years 2016..2024

# stack to a tidy data.frame (year, group, acres)
library(dplyr)
library(tidyr)

df <- as.data.frame(A) |>
  mutate(group = 1:G) |>
  relocate(group) |>
  pivot_longer(-group, names_to = "year", values_to = "acres") |>
  mutate(year = as.integer(year))

head(df, 12)



# =========================================================
# Option A (Section 2A): "No late entry" quick estimator
# - E_{1,t} = a_{1,t}; E_{g>1,t} = 0
# - r_hat_{g,t} = a_{g+1, t+1} / a_{g,t}
# - X10_hat_t = a_{10, t+1}  (or r_hat_9,t * a_{9,t})
# =========================================================

# Helper to get a_{g,t}
a <- function(g, y) A[g, which(years == y)]

# Compute step-by-step ratios for g=1..9 and t where defined
ratios_A <- expand.grid(g = 1:(G-1), year = years[-length(years)]) |>
  mutate(r_hat = mapply(function(g, y) {
    num <- a(g+1, y+1)
    den <- a(g,   y)
    if (den <= 0) return(NA_real_) else return(num / den)
  }, g, year))

# Entry estimates (under no late entry)
EhatA <- matrix(0, nrow = G, ncol = Tn, dimnames = list(paste0("g",1:G), years))
EhatA[1, ] <- A[1, ]

# Exit from 10
X10_hat_A <- c(NA, A[10, 2:Tn])  # equals a_{10, t+1}; aligns with 2016..2024

cat("\n=== Option A: sample of transition ratios r_hat_{g,t} ===\n")
print(head(ratios_A, 10))

cat("\n=== Option A: Exit from group 10 (acres), by year ===\n")
print(data.frame(year = years, X10_hat = X10_hat_A))



# =========================================================
# Option B (Section 2B): Allow late entry via pooled OLS + NNLS
# - Estimate constant r_g by pooled slope through the origin:
#     a_{g+1, t+1} = r_g * a_{g,t} + error
# - Recover E_{g,t} as residuals:
#     E_{1,t}=a_{1,t}; E_{g>=2,t} = a_{g,t} - r_{g-1} * a_{g-1, t-1}
# - Enforce E_{g,t} >= 0 via Non-Negative Least Squares (nnls) re-fit
# - Exit: X10_t = r_9 * a_{9,t}
# =========================================================

# Build pooled regressions for each g=1..9
# Create data for each step: y = a_{g+1,t+1}, x = a_{g,t}
make_step_xy <- function(g) {
  # t runs where both sides observed
  t_ix <- 1:(Tn-1)
  y <- A[g+1, t_ix + 1]
  x <- A[g,   t_ix]
  data.frame(x = x, y = y, t = years[t_ix])
}

steps <- lapply(1:(G-1), make_step_xy)

# OLS through origin for each step
rhat_B <- sapply(steps, function(d) {
  # lm(y ~ x - 1) is through-origin
  fit <- lm(y ~ x - 1, data = d)
  unname(coef(fit))
})

names(rhat_B) <- paste0("r_", 1:(G-1))
rhat_B

# Recover entries as residuals (before NNLS enforcement)
EhatB <- matrix(0, nrow = G, ncol = Tn, dimnames = list(paste0("g",1:G), years))
EhatB[1, ] <- A[1, ]  # §2B identity

for (t in 2:Tn) {
  for (g in 2:G) {
    EhatB[g, t] <- A[g, t] - rhat_B[g-1] * A[g-1, t-1]
  }
}

# Some EhatB[g,t] can be negative due to noise; enforce >=0 using NNLS
# We re-fit r_g stepwise with nonnegative E's by solving:
#   For each (g>=2), for all t>=2: A[g,t] = r_{g-1} * A[g-1,t-1] + E[g,t], E[g,t]>=0
# That is NNLS of y on x, then residuals define E >= 0 automatically.

library(nnls)

rhat_B_nnls <- numeric(G-1)
names(rhat_B_nnls) <- paste0("r_", 1:(G-1))

for (g in 2:G) {
  t_ix <- 2:Tn
  y <- A[g,   t_ix]
  x <- A[g-1, t_ix - 1]
  # nnls solves min ||y - X*b|| s.t. b>=0; but we need slope unconstrained in [0,1]
  # Trick: we don't constrain to <=1 here; can clip after. Use 1-column X.
  fit <- nnls(as.matrix(x), y)
  rhat_B_nnls[g-1] <- coef(fit)
  # The implied Ehat are the nonnegative residuals:
  EhatB[g, t_ix] <- y - as.vector(as.matrix(x) %*% coef(fit))
}

# Optional: clip r in [0,1]
rhat_B_nnls <- pmin(pmax(rhat_B_nnls, 0), 1)

cat("\n=== Option B: pooled slopes r_g (OLS) ===\n")
print(round(rhat_B, 4))

cat("\n=== Option B: pooled slopes r_g (NNLS, clipped to [0,1]) ===\n")
print(round(rhat_B_nnls, 4))

# Exit after 10 (movement 10 -> out): X10_t = r_9 * a_{9,t}
X10_hat_B <- c(NA, rhat_B_nnls[9] * A[9, 1:(Tn-1)])

# Summaries
cat("\n=== Option B: sample of recovered entries E_{g,t} (acres) ===\n")
print(round(EhatB[1:5, 5:10], 1))  # show groups 1..5 and mid years

cat("\n=== Option B: Exit from group 10 (acres), by year ===\n")
print(data.frame(year = years, X10_hat = round(X10_hat_B,1)))


# =========================================================
# (Bonus) Tiny state-space flavor (Section 3 idea):
#   For one step (say 3 -> 4), let r_{3,t} evolve as a local level,
#   and treat E_{4,t} as a nonnegative disturbance captured in the observation.
#   Observation: a_{4,t} = r_{3,t-1} * a_{3,t-1} + E_{4,t} + eps_t
#   Since a_{3,t-1} is known, this is linear in the time-varying state r_{3,t}.
#   We'll fit a simple DLM: y_t = F_t * theta_t + v_t, with theta_t random walk.
#   NOTE: This “bundles” late entry into residuals; for full multi-group SSM,
#   you’d stack states for all r_g and (optionally) explicit E_g,t processes.
# =========================================================

library(KFAS)

g <- 3  # model the transition 3 -> 4
t_ix <- 2:Tn
y <- A[g+1, t_ix]                 # a_{4,t}
x <- A[g,   t_ix - 1]             # known regressor a_{3,t-1}

# Build state-space with time-varying regression: y_t = x_t * r_t + noise
# State: r_t = r_{t-1} + w_t (local level)
Zt <- array(x, dim = c(1,1,length(x)))  # Z_t multiplies the state
Tt <- 1                                  # state evolution
Rt <- 1
Qt <- array(NA, c(1,1))                  # variance to be estimated
Ht <- matrix(NA)                         # obs variance to be estimated

model <- SSModel(y ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = matrix(rhat_B_nnls[g],1,1)) , H = Ht)

fit <- fitSSM(model, inits = log(c(var(y)*0.01, var(y)*0.1)), method = "BFGS")  # init variances
kfs <- KFS(fit$model, smoothing = "state")

r_smoothed <- as.numeric(kfs$alphahat)  # smoothed path of r_{3,t}
years_t    <- years[t_ix]

cat("\n=== State-space demo (step 3->4): smoothed r_{3,t} over time ===\n")
print(data.frame(year = years_t, r3_smoothed = round(r_smoothed,4)))

# With a full build, you’d do this for g=1..9 and then reconstruct
# E_{g,t} = a_{g,t} - r_{g-1,t-1} * a_{g-1,t-1}, possibly modeling E explicitly.



# =========================================================
# Quick comparison vs. truth (since this is toy data)
# =========================================================

cat("\n=== True r_g vs Option B (NNLS) estimates ===\n")
print(data.frame(
  step = paste0(1:(G-1),"->",2:G),
  r_true = round(r_true,3),
  r_hat_B_NNLS = round(rhat_B_nnls,3)
))

cat("\n=== True exit from 10 vs. Option B exit ===\n")
print(data.frame(
  year = years,
  X10_true = round(X10_true,1),
  X10_hat_B = round(X10_hat_B,1)
))






