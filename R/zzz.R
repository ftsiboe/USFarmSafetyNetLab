.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 3600)

  calculate_mode <- function(x,na.rm = T){ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # options(progressr.enable = TRUE)
  # progressr::handlers(global = TRUE)

  # memoise functions
  # locate_download_link <<- memoise::memoise(locate_download_link)


}

