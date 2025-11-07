
source("data-raw/scripts/repo_workflow/environment_setup.R")

devtools::document()

library(tidyverse);library("rvest");library("tidyr");library(data.table)

data <- data.table::rbindlist(
  lapply(
    2011:as.numeric(format(Sys.Date(), "%Y")),
    function(year){
      tryCatch({
        if(year>=2018){
          df <- as.data.table(rfcip::get_adm_data(dataset="A01220_GuaranteeAdjustment",year=year))
          df[,commodity_year := year]
          df[,state_code := as.numeric(as.character(state_code))]
          df[,county_code := as.numeric(as.character(county_code))]
          df[,commodity_code := as.numeric(as.character(commodity_code))]
          df[,type_code := as.numeric(as.character(type_code))]
          df[,practice_code := as.numeric(as.character(practice_code))]
          df[,insurance_plan_code := as.numeric(as.character(insurance_plan_code))]
          df[,insurance_option_code := as.character(insurance_option_code)]
          df[,prevented_planting_guarantee_adjustment_factor := as.numeric(as.character(prevented_planting_guarantee_adjustment_factor))]
          df <- df[
            , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
            by = c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code","insurance_plan_code","insurance_option_code"),
            .SDcols = c("prevented_planting_guarantee_adjustment_factor")]
        } 
        if(year<2018){
          df <- tempfile(fileext = ".pdf")
          
          # year <- 2017
          # PDF <- tempfile(fileext = ".pdf")
          # download.file( paste0("https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/",year,
          #                       "/Approved/P11_5_Prevented_Planting_Guarantee_Adjustment_Factor_Exhibit.pdf"),
          #                destfile=PDF,mode="wb")
          
          if(year %in% 2013:2017){
            download.file( paste0("https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/",year,
                                  "/Approved/P11_5_Prevented_Planting_Guarantee_Adjustment_Factor_Exhibit.pdf"),
                           destfile=df,mode="wb")
          }
          
          if(year %in% 2001:2012){
            download.file( paste0("https://pubfs-rma.fpac.usda.gov/pub/Publications/M13_Handbook/",max(c(year,2011)),
                                  "/Approved/P11_5_PREVENTED_PLANTING_GUARANTEE_ADJUSTMENT_FACTOR_EXHIBIT.PDF"),
                           destfile=df,mode="wb")
          }
          
          df <- pdftools::pdf_text(df)
          
          df <- map(df, ~ str_split(.x, "\\n") |> unlist())
          df <- reduce(df, c)
          df <- df[grepl("0[.]",toupper(df))]
          df <- gsub("0[.]","|0.",df)
          df <- data.frame(text=df)
          df <- tidyr::separate(df,"text",into=c(paste0("data_",1:4)),sep="[|]")
          df$data_1 <- as.numeric(gsub("[^0-9]","",df$data_1))
          for(xx in 2:ncol(df)){df[,xx] <- as.numeric(df[,xx])}
          names(df)[names(df) %in% "data_1"] <- "commodity_code"
          df <- df |>  tidyr::gather(insurance_option_code, prevented_planting_guarantee_adjustment_factor, 2:ncol(df)) 
          df$insurance_option_code <- as.character(factor(df$insurance_option_code,levels = c("data_2","data_3","data_4"),labels = c("","PF","PT")))
          df <- as.data.table(df[!df$prevented_planting_guarantee_adjustment_factor %in% NA,])
          
          current_shell <- as.data.table(rfcip::get_sob_data(sob_version = "sobtpu",year=year))
          current_shell <- unique(current_shell[
            ,  c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code",
                 "insurance_plan_code"), with = FALSE])
  
          df <- df[current_shell,on = intersect(names(df), names(current_shell)),nomatch = 0, allow.cartesian=TRUE]
          df <- df[
            , lapply(.SD, function(x) mean(x, na.rm = TRUE)),
            by = c("commodity_year","state_code","county_code","commodity_code","type_code","practice_code","insurance_plan_code","insurance_option_code"),
            .SDcols = c("prevented_planting_guarantee_adjustment_factor")]
        }
        df
      }, error=function(e){NULL})
    }), fill = TRUE)

