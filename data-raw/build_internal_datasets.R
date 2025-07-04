
# load package
rm(list=ls(all=TRUE))
devtools::document()
devtools::load_all()

current_year <- as.numeric(format(Sys.Date(),"%Y"))-2

dir_fastscratch <- "./data-raw/fastscratch"

# Create target directory if needed
if (!dir.exists(dir_fastscratch)) {
  dir.create(dir_fastscratch, recursive = TRUE)
}
if (!dir.exists("./data-raw/internal_datasets")) {
  dir.create("./data-raw/internal_datasets", recursive = TRUE)
}

if(Sys.info()['sysname'] %in% "Windows"){
  farmpolicylab <- paste0(gsub("OneDrive","Dropbox",Sys.getenv("OneDriveConsumer")),"/farmpolicylab/database/")
}else{
  farmpolicylab <- paste0("~/Database/USA/USDA/")
}
Keep.List<-c("Keep.List",ls())
#---------------------------------------------------------
# Download and cache USDA NASS Quick Stats             ####
download_raw_data(dir_fastscratch=dir_fastscratch);gc()
#---------------------------------------------------------
# Contiguous county                                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
contiguous_county <- rmaADM:::clean_data(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/2025/2025_A01230_ContiguousCounty_YTD.rds")))
contiguous_county <- as.data.table(contiguous_county)
contiguous_county[, data_source := "USDA-RMA, Actuarial Data Master - A0123"]
saveRDS(contiguous_county,file="./data-raw/internal_datasets/contiguous_county.rds");rm(contiguous_county);gc()
#---------------------------------------------------------
# Harmonize and summarize crop type                    ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
type_recodes <- harmonize_crop_type_codes()
saveRDS(type_recodes,file="./data-raw/internal_datasets/rma_type_recodes.rds");gc()
#---------------------------------------------------------
# INDEX FOR PRICE RECEIVED, 2011                       ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <- process_nass_dataset(
  dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"),
  large_dataset = "economics",
  nassqs_params = list(short_desc = "COMMODITY TOTALS - INDEX FOR PRICE RECEIVED, 2011",
                       freq_desc = "ANNUAL"))
df <- df[freq_desc %in% "ANNUAL",
         .(index_for_price_recived = mean(value, na.rm = TRUE)),
         by = c("commodity_year")]
df[, data_source := "USDA NASS Quick Stats"]
saveRDS(df,file="./data-raw/internal_datasets/index_for_price_recived.rds");rm(df);gc()
#---------------------------------------------------------
# Get Marketing Year Average Price                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
df <-  get_marketing_year_avg_price(
  dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"),
  agg_level_desc = c("NATIONAL","STATE"),
  short_desc = c(
    "OATS - PRICE RECEIVED, MEASURED IN $ / BU",
    "RYE - PRICE RECEIVED, MEASURED IN $ / BU",
    "TOBACCO - PRICE RECEIVED, MEASURED IN $ / LB",
    "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
    "FLAXSEED - PRICE RECEIVED, MEASURED IN $ / BU",
    "BARLEY - PRICE RECEIVED, MEASURED IN $ / BU",
    "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRICE RECEIVED, MEASURED IN $ / CWT",
    "HAY - PRICE RECEIVED, MEASURED IN $ / TON",
    "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU",
    "COTTON - PRICE RECEIVED, MEASURED IN $ / LB",
    "SORGHUM, GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARBEETS - PRICE RECEIVED, MEASURED IN $ / TON",
    "PEAS, DRY EDIBLE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "SUNFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT",
    "RICE - PRICE RECEIVED, MEASURED IN $ / CWT",
    "PEANUTS - PRICE RECEIVED, MEASURED IN $ / LB",
    "CANOLA - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MAPLE SYRUP - PRICE RECEIVED, MEASURED IN $ / GALLON",
    "RICE, LONG GRAIN - PRICE RECEIVED, MEASURED IN $ / CWT",
    "MILLET, PROSO - PRICE RECEIVED, MEASURED IN $ / BU",
    "SUGARCANE - PRICE RECEIVED, MEASURED IN $ / TON",
    "SAFFLOWER - PRICE RECEIVED, MEASURED IN $ / CWT"))
saveRDS(df,file="./data-raw/internal_datasets/marketing_year_avg_price.rds");rm(df);gc()

#---------------------------------------------------------
# Land values/rent                                     ####
rm(list= ls()[!(ls() %in% c(Keep.List))])

land_value <- process_nass_dataset(
  dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"),
  large_dataset = "economics",
  nassqs_params = list(
    short_desc = "AG LAND, CROPLAND - ASSET VALUE, MEASURED IN $ / ACRE",
    agg_level_desc = "STATE",
    freq_desc = "ANNUAL",
    domain_desc = "TOTAL",
    reference_period_desc="YEAR",
    commodity_desc="AG LAND"))[
      ,.(ag_land = mean(value, na.rm = TRUE)), by = c("commodity_year","state_code")]


rents <- process_nass_dataset(
  dir_nass_qs = paste0(dir_fastscratch,"/nass_qs"),
  large_dataset = "economics",
  nassqs_params = list(
    short_desc = "RENT, CASH, CROPLAND - EXPENSE, MEASURED IN $ / ACRE",
    agg_level_desc = "STATE",
    freq_desc = "ANNUAL",
    domain_desc = "TOTAL",
    reference_period_desc="YEAR",
    commodity_desc="RENT"))[
      ,.(ag_land = mean(value, na.rm = TRUE)), by = c("commodity_year","state_code")]
land_value <- land_value[,.(land_rent = mean(value, na.rm = TRUE)), by = c("commodity_year","state_code")]

land_value <- as.data.frame(dplyr::full_join(land_value,rents,by=c("state_code","commodity_year")))

land_valuex <- land_value[! state_code %in% c(9,15,23,25,33,44,50,98),]




land_valuex$year  <- land_valuex$commodity_year
land_valuex$state <- land_valuex$state_code
land_valuex <- plm::pdata.frame(land_valuex, index=c("state","year"), drop.index=TRUE, row.names=TRUE)
for(lag in 1:4){
  land_valuex[,paste0("LV_",lag)] <- lag(land_valuex$Land.Value,lag)
}
fit.rent <- lm(as.formula(
  paste0("log(Land.Rent)~1+commodity_year*factor(state_code)+factor(state_code)-commodity_year+",
         paste0(names(land_valuex)[grepl("LV_",names(land_valuex))],collapse = "+"))
), data = land_valuex)

summary(fit.rent)



Landx$Rent.hat <- exp(predict(fit.rent,Landx))
Landx$Error <- Landx$Land.Rent/Landx$Rent.hat
Landx <- dplyr::inner_join(Landx,doBy::summaryBy(Error~state_code,data=Landx,FUN=mean,na.rm=T),by="state_code")

Landx$Rent.hat.adj <- ifelse(Landx$Land.Rent %in% NA, Landx$Rent.hat*Landx$Error.mean,Landx$Land.Rent)
Landx <- as.data.frame(Landx)
Landx <- lapply(Landx, function(x){attr(x, "index") <- NULL; x})
Landx <- data.frame(Landx)
Landx$state_cd <-as.numeric(as.character(Landx$state_cd))
Landx$crop_yr <- as.numeric(as.character(Landx$crop_yr))

Land <- Landx[c("state_cd","crop_yr","Rent.hat.adj")]
names(Land) <- c("state_cd","crop_yr","Rent")

States <- vect(paste0(Dr.POLYG,"/USA_States.shp"))
States$state_cd <- as.numeric(States$STATEFP)
centroids<-as.data.frame(geom(terra::centroids(States)))[c("x", "y")]

nbmat <- sp::SpatialPointsDataFrame(cbind(centroids$x, centroids$y),data = as.data.frame(States),
                                    proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

nbmat <- spdep::knearneigh(nbmat, k=5, longlat = TRUE, use_kd_tree=TRUE)

nbmat <- spdep::knn2nb(nbmat, row.names = States$state_cd, sym = FALSE)

nbmat <- spdep::nb2mat(nbmat, style="B")

for(i in 1:2){
  Land <- as.data.frame(
    data.table::rbindlist(
      lapply(
        unique(Land$crop_yr),
        function(crop_yr) {
          done <- NULL
          #tryCatch({
          # crop_yr <- 2001
          data <- Land[Land$crop_yr %in% crop_yr,c("state_cd","crop_yr","Rent")]
          data <- data[!data$Rent %in% NA,]
          data <- dplyr::full_join(as.data.frame(States),data,by="state_cd")[c("NAME","state_cd","Rent")]
          data$Rent.spat <- data$Rent
          data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN),0,data$Rent.spat)
          data$Rent.spat <- c(t(t(data$Rent.spat)%*%t(nbmat))/rowSums(nbmat))
          data$Rent.spat <- ifelse(data$Rent.spat %in% c(0,NA,Inf,-Inf,NaN),NA,data$Rent.spat)
          data$crop_yr <- crop_yr
          done <- data
          #}, error=function(e){})
          return(done)
        }), fill = TRUE))
  
  Land$Error <- Land$Rent/Land$Rent.spat
  Land <- dplyr::inner_join(Land,doBy::summaryBy(Error~state_cd,data=Land,FUN=mean,na.rm=T),by="state_cd")
  
  Land$Rent <- ifelse(Land$Rent %in% NA & ! Land$Error.mean %in% c(0,NA,NaN,Inf,-Inf), Land$Rent.spat*Land$Error.mean,Land$Rent)
  Land$Rent <- ifelse(Land$Rent %in% c(0,NA,NaN,Inf,-Inf), Land$Rent.spat,Land$Rent)
  Land <- Land[c("NAME","state_cd","crop_yr","Rent")]
}

Keep.List<-c("Land",Keep.List)

#---------------------------------------------------------
#  build the helper data sets                          ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
build_internal_datasets(dir_source = "./data-raw/internal_datasets", size_threshold = 1)
#---------------------------------------------------------




