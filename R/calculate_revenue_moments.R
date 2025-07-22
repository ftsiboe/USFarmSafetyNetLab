#' Calculate key distributional moments for a numeric vector of revenues
#'
#' @param x Numeric vector of revenues (e.g. per-draw revenues)
#' @return A one-row data.frame with:
#'   - mu:      mean
#'   - md:      median
#'   - sd:      standard deviation
#'   - cv:      coefficient of variation (sd / mu)
#'   - vr:      variance
#'   - sk:      skewness
#'   - ku:      kurtosis
#'   - lapv:    mean squared loss deviations below the mean
#'   - lrpv:    lapv / probability of loss
#'   - nlapv:   lapv normalized by mu
#'   - nlrpv:   lrpv normalized by mu
#'   - lres2:   same as lapv (for clarity)
#'   - cdf:     empirical probability of loss (P(X < mu))
#' @import moments
#' @importFrom stats median var
#' @export
calculate_revenue_moments <- function(x) {
  mu    <- mean(x,     na.rm = TRUE)
  md    <- median(x,   na.rm = TRUE)
  sd    <- sd(x,       na.rm = TRUE)
  cv    <- sd / mu
  vr    <- var(x,      na.rm = TRUE)
  sk    <- moments::skewness(x, na.rm = TRUE)
  ku    <- moments::kurtosis(x, na.rm = TRUE)
  res   <- x - mu
  lres2 <- mean(ifelse(res > 0, NA, res^2), na.rm = TRUE)
  cdf   <- mean(ifelse(res < 0, 1, 0), na.rm = TRUE)
  lapv  <- lres2
  lrpv  <- lapv / cdf
  nlapv <- lapv / mu
  nlrpv <- lrpv / mu
  
  data.frame(
    mu    = mu,
    md    = md,
    sd    = sd,
    cv    = cv,
    vr    = vr,
    sk    = sk,
    ku    = ku,
    lapv  = lapv,
    lrpv  = lrpv,
    nlapv = nlapv,
    nlrpv = nlrpv,
    lres2 = lres2,
    cdf   = cdf,
    stringsAsFactors = FALSE
  )
}