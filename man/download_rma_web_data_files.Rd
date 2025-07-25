% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_fcip_data.R
\name{download_rma_web_data_files}
\alias{download_rma_web_data_files}
\title{Download and Process RMA Web Data Files}
\usage{
download_rma_web_data_files(
  years,
  file_name,
  dest = "./data-raw/data_release",
  url_rma_web_data_files = "https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files"
)
}
\arguments{
\item{years}{Integer vector of crop or livestock years to fetch.}

\item{file_name}{Character; one of
\code{"sobcov"}, \code{"sobtpu"}, \code{"lgm"}, \code{"lrp"}, or \code{"colsom"}.
Determines both the subdirectory and the column schema.}

\item{dest}{Character path where the final \code{.rds} files will go.
Defaults to \code{"./data-raw/data_release"}.}

\item{url_rma_web_data_files}{Base URL for USDA RMA web data.
Defaults to the official RMA Web_Data_Files root.}
}
\value{
Invisibly returns \code{NULL}. Side effect: one \code{.rds}
per year is written under \code{dest}, named \code{<file_name>_<year>.rds}.
}
\description{
Fetches one or more years of USDA RMA Summary of Business data
(state-county-crop & livestock participation series), unzips,
reads the pipe-delimited text, applies the correct column names,
and saves each year as an \code{.rds} under \code{dest}.
}
\examples{
\dontrun{
# Get sobtpu data for 2018-2022
download_rma_web_data_files(2018:2022, "sobtpu")
}
}
