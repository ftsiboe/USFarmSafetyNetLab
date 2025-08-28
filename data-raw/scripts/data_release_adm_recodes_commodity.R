
# https://www.ams.usda.gov/services/grants/scbgp/specialty-crop

source("data-raw/scripts/environment_setup.R")

devtools::document()

sob <- as.data.frame(
  data.table::rbindlist(
    list(
      unique(as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobtpu_all.rds")))[
      c("commodity_year","commodity_code","commodity_name")]),
      
      unique(as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobcov_all.rds")))[
        c("commodity_year","commodity_code","commodity_name")]),
      
      unique(as.data.frame(readRDS(paste0(dir_data_release,"/sob/sobscc_all.rds")))[
        c("commodity_year","commodity_code","commodity_name")])), fill = TRUE))

sob <- unique(sob[c("commodity_year","commodity_code","commodity_name")])

names(sob) <- c("crop_yr","crop_cd","crop")

sob$crop_yr <- as.numeric(as.character(sob$crop_yr))
sob$crop_cd <- as.numeric(as.character(sob$crop_cd))
sob$crop <- toupper(sob$crop)


adm <- as.data.frame(
  data.table::rbindlist(
    lapply(
      2011:as.numeric(format(Sys.Date(), "%Y")),
      function(year) {
        tryCatch({ 
          df <- get_adm_data(year = year,dataset = "A00420_Commodity")
          df$commodity_year <- year
          return(df)
        }, error = function(e){return(NULL)})
      }), fill = TRUE))

adm <- unique(adm[c("commodity_year","commodity_code","commodity_name")])
names(adm) <- c("crop_yr","crop_cd","crop")
adm$crop_yr <- as.numeric(as.character(adm$crop_yr))
adm$crop_cd <- as.numeric(as.character(adm$crop_cd))

adm <- rbind(unique(readRDS(paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/1995-2010 clean/CROSS_REFERENCE_DATA_1997_2010.rds"))[names(adm)]),adm)
adm$crop <- toupper(adm$crop)

adm <- adm[c("crop_yr","crop_cd","crop")]
names(adm) <- c("crop_yr","crop_cd","crop_adm")

adm <- unique(dplyr::full_join(adm,sob,by=c("crop_yr","crop_cd")))
rm(sob)

adm$crop <- ifelse(adm$crop_adm %in% NA,adm$crop,adm$crop_adm)
adm <- adm[complete.cases(adm),]
adm <- adm[!adm$crop_cd %in% 9999,]
adm <- adm[!adm$crop %in% c("COMBINED CROP","PREVENTED PLANTING","PREVENTED PLANTING ENDORSE"),]
adm$crop <- ifelse(adm$crop %in% "PECANS","PECAN",adm$crop)
adm$crop <- ifelse(adm$crop %in% "ORANGES","ORANGE",adm$crop)
adm$crop <- ifelse(adm$crop %in% "APPLES","APPLE",adm$crop)
adm$crop <- ifelse(adm$crop %in% "AVOCADOS","AVOCADO",adm$crop)
adm$crop <- ifelse(adm$crop %in% "LEMONS","LEMON",adm$crop)
adm$crop <- ifelse(adm$crop %in% "LIMES","LIME",adm$crop)
adm$crop <- ifelse(adm$crop %in% "MANDARINS/TANGERINES","MANDARIN/TANGERINE",adm$crop)

adm <- unique(adm[c("crop_yr","crop_cd","crop")])
adm <- adm[order(adm$crop_cd),]
row.names(adm) <- 1:nrow(adm)
adm$flag_crop <- ifelse(adm$crop_cd != lag(adm$crop_cd),adm$crop,NA)
adm$flag_crop[1] <- adm$crop[1]
adm$flag_crop <-zoo::na.locf(adm$flag_crop,na.rm = F)

flaged_crops <- adm[!adm$flag_crop == adm$crop,]

for(ii in 1:nrow(flaged_crops)){
  adm$crop <- ifelse(adm$crop_cd %in% flaged_crops$crop_cd[ii],
                     mode(adm[adm$crop %in% c(flaged_crops$crop[ii],flaged_crops$flag_crop[ii]),"crop"]),adm$crop)
}

adm <- unique(adm[c("crop_yr","crop_cd","crop")])
adm$keep <- 1

agrcomm <-  as.data.frame(
  data.table::rbindlist(
    lapply(
      list.files(path=paste0(farmpolicylab,"rmaFCIPdata/rmaActuarialDataMaster/Archive/"),
                 full.names = T,recursive = T,pattern = "_AgrCommodity_YTD.rds"),
      function(file){
        #print(file)
        #file <- "./data-raw/rmaFCIPdata/rmaActuarialDataMaster/Archive//2011/2011_A00400_AgrCommodity_YTD.rds"
        agrcomm <- readRDS(file)
        agrcomm$crop_yr <- agrcomm$Reinsurance.Year
        agrcomm$crop <- as.character(agrcomm$AGR.Commodity.Name)
        tryCatch({ 
          agrcomm$annual <- as.numeric(agrcomm$Annual.Planting.Code %in% "A")
          agrcomm$perennial <- as.numeric(agrcomm$Annual.Planting.Code %in% "P")
        }, error=function(e){})
        agrcomm$annimal <- as.numeric(agrcomm$Animal.Flag %in% "Y")
        agrcomm <- doBy::summaryBy(list(names(agrcomm)[names(agrcomm) %in% c("annimal","annual","perennial")],
                                        c("crop","crop_yr")),data=agrcomm,FUN=max,na.rm=T,keep.names = T)
        return(agrcomm)
      }), fill = TRUE))

agrcomm$annimal <- ifelse(agrcomm$annimal %in% NA,0,agrcomm$annimal)
agrcomm$annual <- ifelse(agrcomm$annual %in% NA,0,agrcomm$annual)
agrcomm$perennial <- ifelse(agrcomm$perennial %in% NA,0,agrcomm$perennial)
agrcomm <- doBy::summaryBy(list(names(agrcomm)[names(agrcomm) %in% c("annimal","annual","perennial")],
                                c("crop")),data=agrcomm,FUN=max,keep.names = T,na.rm=T)
agrcomm$crop <- toupper(agrcomm$crop)

adm <- dplyr::full_join(adm,agrcomm,by="crop")
adm <- adm[adm$keep %in% 1,]
rm(agrcomm)

adm$crop_gr <- NA

# Nursery
adm$crop_gr <- ifelse(grepl("NURSERY",adm$crop),"Nursery:Nursery",adm$crop_gr)

# Controlled Environment
adm$crop_gr <- ifelse(adm$crop %in% c("CONTROLLED ENVIRONMENT"),"Controlled Environment:Controlled Environment",adm$crop_gr)

# Whole farm
adm$crop_gr <- ifelse(grepl("GROSS",adm$crop),"Farm:Farm",adm$crop_gr)
adm$crop_gr <- ifelse(grepl("WHOLE FARM",adm$crop),"Farm:Farm",adm$crop_gr)
adm$crop_gr <- ifelse(grepl("MICRO FARM",adm$crop),"Farm:Farm",adm$crop_gr)

# Forage Crops
adm$crop_gr <- ifelse(adm$crop %in% c("FORAGE PRODUCTION","FORAGE SEEDING","PASTURE,RANGELAND,FORAGE",
                                      "ALFALFA SEED","ANNUAL FORAGE","GRASS SEED","GRP FORAGE PRODUCTION","SILAGE SORGHUM","RYE"),
                      "Forages:Forages",adm$crop_gr)

# Field Crops
adm$crop_gr <- ifelse(grepl("TOBACCO",adm$crop),"Field Crops:Major",adm$crop_gr)

for(mfc in c("CORN","SOYBEANS","SUGARCANE","WHEAT","PEANUTS","RICE","CULTIVATED WILD RICE","BARLEY","POTATOES",
             "CANOLA","OATS","COTTON","COTTON EX LONG STAPLE","MILLET"," FLAX","SUNFLOWERS",
             "GRAIN SORGHUM","SORGHUM","SILAGE SORGHUM","SUGAR BEETS","POPCORN","SAFFLOWER",
             "BUCKWHEAT","HEMP","FLAX","CAMELINA","SESAME")){
  adm$crop_gr <- ifelse(
    adm$crop %in% c(mfc,
                    paste0("HYBRID ",mfc," SEED"),
                    paste0("HYBRID SEED ",mfc),
                    paste0("GRP ",mfc),
                    paste0("REVENUE COVERAGE ",mfc),
                    paste0("INCOME PROTECTION ",mfc)),
    "Field Crops:Other",adm$crop_gr)
}

adm$crop_gr <- ifelse(adm$crop %in% c(
  "BARLEY","CORN","PEANUTS","POTATOES","RICE","SOYBEANS","WHEAT","OATS","CANOLA","TRITICALE",
  "GRP SOYBEANS","GRP PEANUTS","GRP CORN","GRP WHEAT","GRP GRAIN SORGHUM","GRP COTTON",
  "INCOME PROTECTION COTTON","REVENUE COVERAGE CORN","REVENUE COVERAGE SOYBEANS","INCOME PROTECTION CORN","INCOME PROTECTION WHEAT"),
  "Field Crops:Major",adm$crop_gr)
adm$crop_gr <- ifelse(grepl("COTTON",adm$crop),"Field Crops:Major",adm$crop_gr)
adm$crop_gr <- ifelse(grepl("SORGHUM",adm$crop),"Field Crops:Major",adm$crop_gr)
adm$crop_gr <- ifelse(grepl("TOBACCO",adm$crop),"Field Crops:Major",adm$crop_gr)

# Livestock/Animal Products
adm$crop_gr <- ifelse(adm$annimal %in% 1,"Livestock/Animal Products",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% c("FEEDER CATTLE", "FED CATTLE" ,"SWINE","CATTLE","CLAMS","DAIRY CATTLE","LAMB","MILK","APICULTURE",
                                      "OYSTERS","WEANED CALVES"),
                      "Livestock/Animal Products:Livestock/Animal Products",adm$crop_gr)

# Fruits and Nuts
drupe                <- c("MANGO TREES","OLIVES","CHERRIES","PLUMS","FRESH PLUM","STONEFRUIT")
grapefruit           <- c("CITRUS TREES III","CITRUS III","GRAPEFRUIT","GRAPEFRUIT TREES","ALL OTHER GRAPEFRUIT", "RUBY RED GRAPEFRUIT","ALL OTHER GRAPEFRUIT ALL OTHER GRAPEFRUIT")
lemons               <- c("LEMON","LIME TREES","LEMON TREES","LIME")
oranges              <- c("CITRUS VII", "CITRUS VII","CITRUS V","CITRUS TREES V","CITRUS VIII","CITRUS II","CITRUS TREES I","CITRUS TREES II","CITRUS I",
                          "NAVEL ORANGES","EARLY & MIDSEASON ORANGES","ORANGE TREES",
                          "EARLY & MIDSEASON ORANGES EARLY & MIDSEASON ORANGES","SWEET ORANGES","VALENCIA ORANGES","ORANGE","LATE ORANGES")
mandarins_tangerines <- c("MANDARINS","TANGERINE TREES","MANDARIN/TANGERINE","MANDARIN/TANGERINE TREES","KINNOW MANDARINS","TANGORS")
grapes               <- c("TABLE GRAPES","GRAPES","RAISINS","TABLE GRAPES","RIO RED & STAR RUBY")
berries              <- c("BANANA","BANANA TREE","AVOCADO TREES","AVOCADO","BLUEBERRIES","CRANBERRIES",
                          "STRAWBERRIES","CANEBERRIES","RASPBERRY AND BLACKBERRY","POMEGRANATES")
tangelos             <- c("CITRUS TREES IV","CITRUS IV","MINNEOLA TANGELOS","ORLANDO TANGELOS","TANGELOS","TANGELO TREES")
pome                 <- c("APPLE TREES","PEARS","APPLE")
prunus               <- c("FRESH FREESTONE PEACHES","FRESH NECTARINES","PEACHES","PROCESSING CLING PEACHES",
                          "PRUNES","CANNING PEACHES","FRESH APRICOTS","PROCESSING APRICOTS","PROCESSING FREESTONE" )
other_citrus         <- c("ALL OTHER CITRUS TREES","ALL OTHER CITRUS TREES","CITRUS VI","CITRUS","CITRUS TREES","SPECIAL CITRUS")
other_fruits         <- c("CARAMBOLA TREES","FIGS","PAPAYA","PAPAYA TREE","GRAPEVINE","KIWIFRUIT")
nuts                 <- c("ALMONDS","MACADAMIA NUTS","MACADAMIA TREES","PECAN","PISTACHIOS","PECAN TREES","WALNUTS","TUNG NUTS","COFFEE","COFFEE TREE")

adm$crop_gr <- ifelse(adm$crop %in% drupe                ,"Fruits:drupes",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% grapefruit           ,"Fruits:grapefruit",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% lemons               ,"Fruits:lemons",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% oranges              ,"Fruits:oranges",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% mandarins_tangerines ,"Fruits:mandarins/tangerines",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% grapes               ,"Fruits:grapes",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% berries              ,"Fruits:berries",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% tangelos             ,"Fruits:tangelos",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% pome                 ,"Fruits:pomes",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% prunus               ,"Fruits:prunus",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% other_citrus         ,"Fruits:other citrus",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% other_fruits         ,"Fruits:other fruits",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% nuts                 ,"Nuts:Nuts",adm$crop_gr)

# Vegetables
# leafy_green        <- c()
# edible_plant_stem  <- c()
cruciferous        <- c("CABBAGE")
marrow             <- c("PUMPKINS","CUCUMBERS")
root               <- c("SWEET POTATOES","SWEETPOTATOES")
allium             <- c("ONIONS")
beans              <- c("DRY BEANS","PROCESSING BEANS","BEANS, FRESH SNAP","FRESH MARKET BEANS")
peas_lenils        <- c("GREEN PEAS","DRY PEAS")
fruit_vegetables   <- c("TOMATOES, FRESH","TOMATOES, PROCESS","TOMATOES","CHILE PEPPERS","PEPPERS, BELL","PEPPERS","FRESH MARKET TOMATOES" )
herb               <- c("MINT","CLARY SAGE")
other_vegetables   <- c("HYBRID VEGETABLE SEED","SWEET CORN","SWEET CORN, FRESH","SWEET CORN, PROCESS","HYBRID SWEET CORN SEED","FRESH MARKET SWEET CORN",
                        "MUSTARD","WATERMELONS","WINTER SQUASH")

adm$crop_gr <- ifelse(adm$crop %in% cruciferous      ,"Vegetables:cruciferous vegetables",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% marrow           ,"Vegetables:marrow vegetables",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% root             ,"Vegetables:root vegetables",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% allium           ,"Vegetables:allium vegetables",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% beans            ,"Vegetables:beans (dry or snap)",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% peas_lenils      ,"Vegetables:peas/lenils",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% fruit_vegetables ,"Vegetables:fruit vegetables",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% herb             ,"Vegetables:herbs",adm$crop_gr)
adm$crop_gr <- ifelse(adm$crop %in% other_vegetables ,"Vegetables:other vegetables",adm$crop_gr)

unique(adm[adm$crop_gr %in% NA,c("crop_cd","crop")])

adm$crop_gr <- ifelse(adm$crop_gr %in% NA,"Other Commodity:Other Commodity",adm$crop_gr)

table(adm$crop_gr)

adm <- adm[complete.cases(adm[c("crop_yr","crop_cd")]),]

BARLEY         <- c("BARLEY")
CORN           <- c("CORN","HYBRID CORN SEED","POPCORN","HYBRID POPCORN SEED","GRP CORN","REVENUE COVERAGE CORN","INCOME PROTECTION CORN")
COTTON         <- c("COTTON","COTTON EX LONG STAPLE","GRP COTTON","INCOME PROTECTION COTTON")
SORGHUM        <- c("GRAIN SORGHUM","HYBRID SORGHUM SEED","GRP GRAIN SORGHUM","SILAGE SORGHUM" )
PEANUTS        <- c("PEANUTS","GRP PEANUTS" )
POTATOES       <- c("POTATOES")
SWEET_POTATOES <- c("SWEET POTATOES")
RICE           <- c("RICE","HYBRID SEED RICE","CULTIVATED WILD RICE")
SOYBEANS       <- c("SOYBEANS","GRP SOYBEANS","REVENUE COVERAGE SOYBEANS" )
TOBACCO        <- c("BURLEY TOBACCO","CIGAR BINDER TOBACCO","CIGAR FILLER TOBACCO","CIGAR WRAPPER TOBACCO","DARK AIR TOBACCO","FIRE CURED TOBACCO","FLUE CURED TOBACCO","MARYLAND TOBACCO","TOBACCO")
WHEAT          <- c("WHEAT","GRP WHEAT","INCOME PROTECTION WHEAT")
FLAXSEED       <- c("FLAX")
SUGARCANE      <- c("SUGARCANE")
SUGARBEET      <- c("SUGAR BEETS")
OATS           <- c("OATS")
RYE            <- c("RYE")
SUNFLOWERS     <- c("SUNFLOWERS")
#HAY           <- c("FORAGE PRODUCTION","FORAGE SEEDING","PASTURE,RANGELAND,FORAGE","ANNUAL FORAGE","GRP FORAGE PRODUCTION")

selected_crops <- c(BARLEY,CORN,COTTON,FLAXSEED,OATS,PEANUTS,POTATOES,RICE,RYE,SORGHUM,SOYBEANS,SUGARBEET,
                    SUNFLOWERS,SWEET_POTATOES,WHEAT,SUGARCANE,TOBACCO)

adm$CROP <- adm$crop
for(xx in c("MACADAMIA","PECAN","GRAPEFRUIT","APPLE","ORANGE","PAPAYA",
            "AVOCADO","COFFEE","APRICOT","NECTARINE","CARAMBOLA","PEACHES","TOMATOES","MANGO","FORAGE","SWEET CORN")){
  adm$CROP <- ifelse(grepl(xx,adm$crop),xx,adm$CROP)
}

adm$CROP <- ifelse(adm$crop %in% c("GREEN PEAS","DRY PEAS"),"DRY PEAS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("LEMONS","LEMON TREES","LIMES","LIME TREES"),"LIME/LEMON",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("FRESH PLUM","PLUMS"),"PLUM",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("GREEN PEAS","DRY PEAS"),"DRY PEAS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("TABLE GRAPES"),"GRAPES",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("CITRUS TREES IV","CITRUS IV","MINNEOLA TANGELOS","ORLANDO TANGELOS","TANGELOS","TANGELO TREES"),"TANGELO",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% c("MANDARIN/TANGERINE TREES","MANDARINS/TANGERINES","TANGERINE TREES","MANDARINS","TANGERINES"),"MANDARINS/TANGERINES",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% oranges,"ORANGE",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% grapefruit,"GRAPEFRUIT",adm$CROP)

adm$CROP <- ifelse(adm$crop %in% other_citrus,"OTHER CITRUS",adm$CROP)

adm$CROP <- ifelse(adm$crop %in% RICE,"RICE",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SOYBEANS,"SOYBEANS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% TOBACCO,"TOBACCO",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% WHEAT,"WHEAT",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% CORN,"CORN",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% COTTON,"COTTON",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SORGHUM,"SORGHUM",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% PEANUTS,"PEANUTS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% POTATOES,"POTATOES",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SWEET_POTATOES,"SWEET POTATOES",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SUGARCANE,"SUGARCANE",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SUGARBEET,"SUGAR BEETS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% OATS,"OATS",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% RYE,"RYE",adm$CROP)
adm$CROP <- ifelse(adm$crop %in% SUNFLOWERS,"SUNFLOWERS",adm$CROP)

adm$selected_crops <- as.numeric(adm$crop %in% selected_crops)

adm <- adm[c("crop_yr","crop_cd","crop","CROP","crop_gr","annimal","annual","perennial","selected_crops")]
names(adm) <- c("commodity_year","commodity_code","commodity_name","CROP","commodity_group",
                "annimal_flag","annual_flag","perennial_flag","selected_crops_flag")

adm <- as.data.table(adm[complete.cases(adm[c("commodity_year","commodity_code")]),])

adm <- adm[
  , lapply(.SD, function(x) calculate_mode(x, na.rm = TRUE)),
  by = c("commodity_year","commodity_code"), 
  .SDcols = c("CROP","commodity_name","commodity_group")][
    adm[
      , lapply(.SD, function(x) max(x, na.rm = TRUE)),
      by = c("commodity_year","commodity_code"), 
      .SDcols = c("annimal_flag","annual_flag","perennial_flag","selected_crops_flag")],
    on = c("commodity_year","commodity_code"), nomatch = 0]

