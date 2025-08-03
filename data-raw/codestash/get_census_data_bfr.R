library('magrittr');library('data.table')
dir_ag_census <- "./data-raw/fastscratch/nass/"
brf_census <- data.table::rbindlist(
  lapply(
    list.files(dir_ag_census,pattern = "qs.census",full.names = T), 
    function(census_file){ 
      tryCatch({ 
        data <-  as.data.frame(data.table::fread(census_file))
        
        bfr <- data[data$DOMAIN_DESC %in% c("PRODUCERS, ON ANY OPERATION","TOTAL"),]
        bfr <- bfr[bfr$DOMAINCAT_DESC %in% c("PRODUCERS, ON ANY OPERATION: (LESS THAN 11 YEARS)","NOT SPECIFIED"),]
        bfr <- bfr[bfr$AGG_LEVEL_DESC %in% c("STATE","NATIONAL"),]
        
        bfr <- bfr[bfr$SHORT_DESC %in% c(
          "AG LAND, OWNED, IN FARMS - NUMBER OF OPERATIONS",
          "AG LAND, OWNED, IN FARMS - ACRES",
          "AG LAND, RENTED FROM OTHERS, IN FARMS - NUMBER OF OPERATIONS",
          "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES",
          "GOVT PROGRAMS, FEDERAL - OPERATIONS WITH RECEIPTS",
          "GOVT PROGRAMS, FEDERAL - RECEIPTS, MEASURED IN $") ,]
        
        names(bfr) <- tolower(names(bfr))
        bfr$value <- as.numeric(gsub(",","",as.character(bfr$value)))
        bfr <- bfr[! bfr$value %in% c(0,NA,Inf,-Inf,NaN),]
        
        table(bfr$short_desc,bfr$commodity_desc)
        
        bfr <- doBy::summaryBy(value~year+state_fips_code+state_alpha+agg_level_desc+unit_desc+commodity_desc+domaincat_desc+short_desc
                               ,FUN=mean,na.rm=T,data=bfr,keep.names = T)
        
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, OWNED, IN FARMS - NUMBER OF OPERATIONS","operations_owned",NA)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, OWNED, IN FARMS - ACRES","acres_owned",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - NUMBER OF OPERATIONS","operations_rented",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "AG LAND, RENTED FROM OTHERS, IN FARMS - ACRES","acres_rented",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "GOVT PROGRAMS, FEDERAL - OPERATIONS WITH RECEIPTS","govt_receipts_operations",bfr$variable)
        bfr$variable <- ifelse(bfr$short_desc %in% "GOVT PROGRAMS, FEDERAL - RECEIPTS, MEASURED IN $","govt_receipts_value",bfr$variable)
        
        bfr$variable <- paste0(ifelse(bfr$domaincat_desc %in% "NOT SPECIFIED","all","bfr"),"_",bfr$variable)
        bfr$state_code <- bfr$state_fips_code
        bfr <- doBy::summaryBy(value~year+state_code+state_alpha+variable
                               ,FUN=sum,na.rm=T,data=bfr,keep.names = T)
        bfr <- bfr |> tidyr::spread(variable, value)
        gc()
        
        return(bfr)
      }, error = function(e){return(NULL)})
    }),fill = T)
brf_census[, data_source := "USDA NASS Quick Stats"]
saveRDS(brf_census,file = "./data-raw/internal_datasets/census_state_beginning_farmer_and_rancher_data.rds")