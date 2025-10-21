
# remotes::install_github("dylan-turner25/rmaADM", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfcip", force = TRUE,upgrade="never")
# remotes::install_github("dylan-turner25/rfsa", force = TRUE,upgrade="never")
library(rfcip)

mode <- function(x,na.rm = T) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}

dir_data_release = "data-raw/release"
if(Sys.info()['sysname'] %in% "Windows"){
  farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
}else{
  farmpolicylab <- paste0("~/Database/USA/USDA/")
}

# Create the cache directory if it does not already exist
if (!dir.exists(dir_data_release)) {
  dir.create(dir_data_release, recursive = TRUE)
}

for(dir in c("sob","col","adm","nass","fsa","adm_legacy")){
  if (!dir.exists(paste0(dir_data_release,"/",dir))){
    dir.create(paste0(dir_data_release,"/",dir), recursive = TRUE)
  }
}

dir_fastscratch <- "./data-raw/fastscratch"

if (!dir.exists(dir_fastscratch)) {
  dir.create(dir_fastscratch, recursive = TRUE)
}


for(dir in c("nass")){
  if (!dir.exists(paste0(dir_fastscratch,"/",dir))){
    dir.create(paste0(dir_fastscratch,"/",dir), recursive = TRUE)
  }
}

