
# Run in your project:
rm(list=ls(all=TRUE));gc()
devtools::document()

# Build/check your package to ensure everything is wired up:
devtools::document()
devtools::check_man()
devtools::build_manual()

# Build/check your package to ensure everything is wired up:
devtools::document()
devtools::check()

# tools::showNonASCIIfile("R/prep_nass_data.R")

devtools::build(path ="data-raw/fastscratch")
remotes::install_local("C:/GitHub/rfcipSim/fastscratch/rfcipSim_0.0.0.9001.tar.gz",
                       upgrade="never")




















