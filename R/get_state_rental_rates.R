#' Get State Rental Rates for Cropland
#'
#' @param dir_source `character(1)`
#'   Path to the directory where Quick Stats files are stored.
#'   Defaults to `"./data-raw/fastscratch/nass/"`.
#'   
#' Approximate per-acre cost of crop production using state-level rental rates
#' retrieved from USDA NASS Quick Stats.  This function:
#' \enumerate{
#'   \item Loads and aggregates NASS asset values and cash rents by state and year.
#'   \item Joins the two series, excludes non-contiguous states/territories.
#'   \item Estimates missing rents via a panel regression on log asset values, then corrects for systematic bias.
#'   \item Interpolates any remaining missing values using a 5-nearest-neighbor spatial average, iterated twice.
#' }
#'
#' @return A \code{data.frame} with columns:
#'   \describe{
#'     \item{\code{NAME}}{State name}
#'     \item{\code{state_code}}{Numeric state FIPS code}
#'     \item{\code{commodity_year}}{Year of the observation}
#'     \item{\code{rent}}{Adjusted per-acre rent ($/acre)}
#'   }
#'
#' @details
#' Internally this function relies on:
#' \itemize{
#'   \item \code{process_nass_dataset()} to pull NASS Quick Stats.
#'   \item \code{plm::pdata.frame()} for panel data setup.
#'   \item \code{lm()} to fit a state-fixed-effects trend model.
#'   \item \code{spdep} to compute spatial lags (5-nearest neighbors).
#'   \item \code{doBy::summaryBy()} for error-ratio corrections.
#'   \item \code{terra} and \code{tigris} to obtain state geometries.
#' }
#'
#' @importFrom data.table rbindlist as.data.table
#' @importFrom plm pdata.frame
#' @importFrom doBy summaryBy
#' @importFrom stats as.formula lm predict
#' @importFrom terra vect geom centroids
#' @importFrom tigris states
#' @importFrom sp SpatialPointsDataFrame CRS
#' @importFrom spdep knearneigh knn2nb nb2mat
#' @export
get_state_rental_rates <- function(dir_source = "./data-raw/fastscratch/nass/"){
  
  # Load and process NASS dataset to get average cropland asset value ($/acre)
  land_value <- process_nass_dataset(
    dir_source = dir_source,
    large_dataset = "economics",
    nassqs_params = list(
      short_desc = "AG LAND, CROPLAND - ASSET VALUE, MEASURED IN $ / ACRE",
      agg_level_desc = "STATE",
      freq_desc = "ANNUAL",
      domain_desc = "TOTAL",
      reference_period_desc="YEAR",
      commodity_desc="AG LAND"
    )
  )[
    , .(ag_land = mean(value, na.rm = TRUE)),
    by = c("commodity_year","state_code")
  ]
  
  # Load and process NASS dataset to get average cropland rent expense ($/acre)
  rents <- process_nass_dataset(
    dir_source = dir_source,
    large_dataset = "economics",
    nassqs_params = list(
      short_desc = "RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE",
      agg_level_desc = "STATE",
      freq_desc = "ANNUAL",
      domain_desc = "TOTAL",
      reference_period_desc="YEAR",
      commodity_desc="RENT"
    )
  )[
    , .(land_rent = mean(value, na.rm = TRUE)),
    by = c("commodity_year","state_code")
  ]
  
  # Join asset values and rents
  df <- dplyr::full_join(land_value, rents, by = c("state_code","commodity_year"))
  
  # Exclude non-contiguous states/territories
  dfx <- df[! state_code %in% c(9,15,23,25,33,44,50,98)]
  
  # Prepare for panel regression
  dfx[,year := commodity_year]
  dfx[,state := state_code]
  dfx <- plm::pdata.frame(dfx,index = c("state","year"),drop.index = TRUE,row.names = TRUE)
  
  # Create 4 lags of ag_land
  for(lag in 1:4){dfx[, paste0("LV_",lag)] <- lag(dfx$ag_land, lag)}
  
  # Fit state-fixe--effects trend model on log asset values
  fit.rent <- lm(
    as.formula(
      paste0(
        "log(ag_land) ~ 1 + commodity_year * factor(state_code) + factor(state_code) - commodity_year + ",
        paste0(names(dfx)[grepl("LV_", names(dfx))], collapse = "+")
      )
    ),
    data = dfx
  )
  summary(fit.rent)
  
  # Predict and back-transform
  dfx$land_rent_hat <- exp(predict(fit.rent, dfx))
  
  # Compute error ratios and correct systematic bias
  dfx <- as.data.frame(dfx)
  dfx$error <- dfx$land_rent / dfx$land_rent_hat
  dfx$state_code <- as.numeric(as.character(dfx$state_code))
  dfy <- doBy::summaryBy(error ~ state_code, data = dfx, FUN = mean, na.rm = TRUE)
  dfx <- dplyr::inner_join(
    dfx,
    doBy::summaryBy(error ~ state_code, data = dfx, FUN = mean, na.rm = TRUE),
    by = "state_code"
  )
  dfx$land_rent_hat_adj <- ifelse(
    is.na(dfx$land_rent),
    dfx$land_rent_hat * dfx$error.mean,
    dfx$land_rent
  )
  
  # Clean up and select output columns
  dfx <- as.data.frame(dfx)
  dfx <- lapply(dfx, function(x) { attr(x, "index") <- NULL; x })
  dfx <- data.frame(dfx)
  dfx$state_code     <- as.numeric(as.character(dfx$state_code))
  dfx$commodity_year <- as.numeric(as.character(dfx$commodity_year))
  
  df <- dfx[c("state_code","commodity_year","land_rent_hat_adj")]
  names(df) <- c("state_code","commodity_year","rent")
  
  # Load state geometries and compute centroids
  States <- terra::vect(tigris::states(cb = TRUE))
  States$state_code <- as.numeric(States$STATEFP)
  centroids <- as.data.frame(terra::geom(terra::centroids(States)))[c("x","y")]
  
  # Build spatial neighbor matrix (5-nearest neighbors)
  nbmat <- sp::SpatialPointsDataFrame(
    cbind(centroids$x, centroids$y),
    data = as.data.frame(States),
    proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  )
  nbmat <- spdep::knearneigh(nbmat, k = 5, longlat = TRUE, use_kd_tree = TRUE)
  nbmat <- spdep::knn2nb(nbmat, row.names = States$state_code, sym = FALSE)
  nbmat <- spdep::nb2mat(nbmat, style = "B")
  
  # Two-round spatial interpolation of missing rents
  for(i in 1:2){
    df <- as.data.frame(
      data.table::rbindlist(
        lapply(
          unique(df$commodity_year),
          function(yr) {
            data <- df[df$commodity_year == yr, c("state_code","commodity_year","rent")]
            data <- data[!is.na(data$rent), ]
            data <- dplyr::full_join(as.data.frame(States), data, by = "state_code")[c("NAME","state_code","rent")]
            data$Rent.spat <- data$rent
            data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN), 0, data$Rent.spat)
            data$Rent.spat <- c(t(t(data$Rent.spat) %*% t(nbmat)) / rowSums(nbmat))
            data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN), NA, data$Rent.spat)
            data$commodity_year <- yr
            data
          }
        ),
        fill = TRUE
      )
    )
    df$Error <- df$rent / df$Rent.spat
    df <- dplyr::inner_join(
      df,
      doBy::summaryBy(Error ~ state_code, data = df, FUN = mean, na.rm = TRUE),
      by = "state_code"
    )
    df$rent <- ifelse(
      is.na(df$rent) & !df$Error.mean %in% c(0,NA,NaN,Inf,-Inf),
      df$Rent.spat * df$Error.mean,
      df$rent
    )
    df$rent <- ifelse(df$rent %in% c(0,NA,NaN,Inf,-Inf), df$Rent.spat, df$rent)
    df <- df[c("NAME","state_code","commodity_year","rent")]
  }
  
  df <- as.data.table(df)
  
  return(df)
}