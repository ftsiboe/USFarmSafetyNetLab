% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{build_internal_datasets}
\alias{build_internal_datasets}
\title{Build and Document Helper Datasets}
\usage{
build_internal_datasets(
  dir_source = "./data-raw/internal_datasets",
  size_threshold = 1
)
}
\arguments{
\item{dir_source}{\code{character(1)}
Path to the top-level directory containing raw yearly \code{.rds} files.
Defaults to \code{"./data-raw/internal_datasets"}.}

\item{size_threshold}{\code{numeric(1)}
Maximum file size (in megabytes) allowed for inclusion.  Any dataset whose
largest yearly file exceeds this threshold is skipped entirely.  Defaults to \code{1}.}
}
\value{
Invisibly returns \code{NULL}. Side effects include creation of \code{.rda} files in \verb{./data/}
and generation of \code{R/helper_data.R} containing roxygen entries.
}
\description{
\code{build_internal_datasets()} processes all raw \code{.rds} files found under a specified
directory, harmonizes and combines them across years, applies type conversions,
writes compressed \code{.rda} files into \verb{./data/}, and generates or updates
\code{R/helper_data.R} with roxygen documentation for each combined dataset.
}
\examples{
\dontrun{
  # Combine raw data in "./data-raw", excluding files >1 MB:
  build_fcipSim_helper_datasets(dir = "./data-raw", size_threshold = 1)
}
}
\keyword{internal}
