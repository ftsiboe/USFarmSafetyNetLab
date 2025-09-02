#' Setup Project Environment
#'
#' Loads required libraries, initializes directories, R options, random seed,
#' and analysis year range for the supplemental protection project.
#'
#' @param year_beg Integer. Beginning year of the analysis (default: 2015).
#' @param year_end Integer. Ending year of the analysis (default: 2024).
#' @param seed Integer. Random seed for reproducibility (default: 1980632).
#'
#' @details
#' This function:
#' \itemize{
#'   \item Loads the following packages:
#'         - \code{future.apply}
#'         - \code{rfcip}
#'         - \code{data.table}
#'         - \code{rfcipCalcPass}
#'   \item Detects the operating system and sets the root `fastscratch` directory:
#'         - Windows → `"C:/fastscratch/"`
#'         - Linux/Mac → `"/fastscratch/<username>"`
#'   \item Creates subdirectories for simulation, expected values, draw-farm,
#'         and draw-cost outputs.
#'   \item Ensures `data/` and `output/` directories exist in the project root.
#'   \item Sets R options:
#'         - `scipen = 999` → disables scientific notation
#'         - `future.globals.maxSize` → increases memory for the `future` package
#'         - `dplyr.summarise.inform = FALSE` → suppresses summarise messages
#'   \item Sets a random seed for reproducibility.
#' }
#'
#' @return A named list containing: 
#' \describe{
#'   \item{wd}{List of working directory paths (fastscratch + subfolders).}
#'   \item{year_beg}{The starting year.}
#'   \item{year_end}{The ending year.}
#' }
#'
#' @examples
#' env <- setup_environment(year_beg = 2015, year_end = 2024, seed = 42)
#' env$wd$dir_sim
#' env$year_beg
#'
#' @export
setup_environment <- function(year_beg = 2015, year_end = 2024, seed = 1980632) {
  # ---- Load Required Libraries ----
  suppressPackageStartupMessages({
    library(future.apply)
    library(rfcip)
    library(data.table)
    library(rfcipCalcPass)
  })
  
  # ---- Set Root fastscratch Directory ----
  fastscratch <- ifelse(
    Sys.info()['sysname'] %in% "Windows",
    "C:/fastscratch/",
    paste0("/fastscratch/", Sys.info()['user'])
  )
  
  # ---- Define Subdirectories ----
  wd <- list(
    fastscratch = fastscratch,
    dir_sim      = paste0(fastscratch, "/hidden_safetynet/output/sims/"),
    dir_expected = paste0(fastscratch, "/hidden_safetynet/output/expected/"),
    dir_drawfarm = paste0(fastscratch, "/hidden_safetynet/output/draw_farm/"),
    dir_drawcost = paste0(fastscratch, "/hidden_safetynet/output/draw_cost/")
  )
  
  # ---- Create Directories ----
  for (i in wd) {
    if (!dir.exists(i)) {
      dir.create(i, recursive = TRUE)
    }
  }
  
  if (!dir.exists("data")) dir.create("data")
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("data/agentdata")) dir.create("data/agentdata")
  
  # ---- Set Options ----
  options(scipen = 999)
  options(future.globals.maxSize = 8000 * 10240^2)
  options(dplyr.summarise.inform = FALSE)
  
  # ---- Random Seed ----
  set.seed(seed)
  
  # ---- Return Environment Settings ----
  return(list(
    wd = wd,
    year_beg = year_beg,
    year_end = year_end
  ))
}
