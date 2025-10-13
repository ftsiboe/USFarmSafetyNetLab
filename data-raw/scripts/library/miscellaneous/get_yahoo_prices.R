#' Get Historical Prices from Yahoo Finance
#'
#' @param sym Character. Ticker symbol (e.g., "ZCZ25.CBT" for Dec 2025 corn).
#' @param window Integer. Number of most recent trading days to return (default = 30).
#' @param from Optional start date (string or Date). Ignored if NULL and window is used.
#' @param to Optional end date (string or Date). Defaults to Sys.Date().
#' @return A data.frame with Date, OHLCV, and Adjusted Close.
#' @export
get_yahoo_prices <- function(sym, window = 30, from = NULL, to = Sys.Date()) {
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Package 'quantmod' must be installed. Run install.packages('quantmod').")
  }
  library(quantmod)

  # If user specified from/to, honor those; else default to ~3× window back
  if (is.null(from)) {
    from <- Sys.Date() - window * 3
  }

  # pull data
  getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = TRUE)
  px <- get(sym)

  # keep last N days if window given
  if (!is.null(window)) {
    px <- tail(px, window)
  }

  # return as data.frame
  data.frame(
    date       = index(px),
    open       = as.numeric(px[,1]),
    high       = as.numeric(px[,2]),
    low        = as.numeric(px[,3]),
    close      = as.numeric(px[,4]),
    volume     = as.numeric(px[,5]),
    adj_close  = as.numeric(px[,6]),
    row.names  = NULL
  )
}


