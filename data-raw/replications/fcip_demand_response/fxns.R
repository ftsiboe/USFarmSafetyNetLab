rm(list=ls(all=TRUE))
devtools::document()
dir_nass_qs = "./data-raw/fastscratch/nass_qs"









demean_data <- function(data,vr.names){
  vr.names <- vr.names[vr.names %in% names(data)]
  data <- data[c("pool","crop_yr",vr.names)]
  
  pool_means  <- doBy::summaryBy(list(vr.names,"pool"),FUN=mean,data=data,na.rm=T)
  summary(pool_means$Price.mean)
  data <- dplyr::inner_join(data,pool_means,by="pool")
  
  data$ALL <- 1
  mean_pool_means <- doBy::summaryBy(list(paste0(vr.names,".mean"),"ALL"),FUN=mean,data=data,na.rm=T)
  summary(mean_pool_means$Price.mean.mean)
  data <- dplyr::inner_join(data,mean_pool_means,by="ALL")
  for(vr in vr.names){
    data[,vr] <- data[,vr] - data[,paste0(vr,".mean")] + data[,paste0(vr,".mean.mean")]
  }
  
  data <- data[c("pool","crop_yr",vr.names)]
  data <- as.data.frame(data)
  data <- data[complete.cases(data),]
  
  return(list(data=data,NFE=nrow(pool_means)))
}