# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
# check this  
# Run in your project:
rm(list=ls(all=TRUE));gc()
# library(rmaADM)
# devtools::load_all("C:/GitHub/rfcipCalcPass")
# devtools::load_all("C:/GitHub/rfcipCalibrate")
unlink(c("NAMESPACE",list.files("./man", full.names = TRUE)))
#source("data-raw/internal_datasets.R")
devtools::document()
for(i in list.files("R",full.names = T)){
  print(paste0("********************",i,"********************"))
  tools::showNonASCIIfile(i)
}
devtools::check_man()
devtools::build_manual(path = getwd())
devtools::test()
devtools::check()


