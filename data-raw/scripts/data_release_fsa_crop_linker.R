
rm(list = ls(all = TRUE));gc();library(rfcip);library(rfsa)
source("data-raw/scripts/environment_setup.R")

devtools::document()

#----------------------------------------
# RMA CROPS                           ####
rma_crops <- as.data.frame(readRDS("data-raw/release/adm/fcip_recodes_commodity_groupings.rds"))

rma_crops$crop_cd <- rma_crops$commodity_code
rma_crops$crop_yr <- rma_crops$commodity_year
rma_crops$crop <- rma_crops$commodity_name
rma_crops$crop_rma <- rma_crops$crop

rma_crops <- unique(rma_crops[c("crop_cd","crop_yr","crop","crop_rma")])
rma_crops$crop <- toupper(rma_crops$crop)
rma_crops <- rma_crops[!grepl("PREVENTED ",rma_crops$crop),]
rma_crops <- rma_crops[!grepl("ALL OTHER COMMODITIES",rma_crops$crop),]
rma_crops <- rma_crops[!rma_crops$crop %in% c("FEEDER CATTLE","FED CATTLE","SWINE","ADJ. GROSS REVENUE-LITE","ADJUSTED GROSS REVENUE","APICULTURE","CATTLE","CLAMS",
                                              "DAIRY CATTLE" ,"LAMB","OYSTERS","WHOLE FARM REVENUE PROTECTION","MILK","MICRO FARM" ,"COMBINED CROP","ALL OTHER CROPS"),]

rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("BURLEY",rma_crops$crop),"TOBACCO BURLEY",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("MARYLAND",rma_crops$crop),"TOBACCO MARYLAND",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("FLUE",rma_crops$crop) & grepl("CURED",rma_crops$crop),"TOBACCO FLUE CURED",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("FIRE",rma_crops$crop) & grepl("CURED",rma_crops$crop),"TOBACCO FIRE CURED",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("DARK",rma_crops$crop) & grepl("AIR",rma_crops$crop),"TOBACCO DARK AIR",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("CIGAR",rma_crops$crop) & grepl("BINDER",rma_crops$crop),"TOBACCO CIGAR BINDER",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("CIGAR",rma_crops$crop) & grepl("FILLER",rma_crops$crop),"TOBACCO CIGAR FILLER",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOBACCO",rma_crops$crop) & grepl("CIGAR",rma_crops$crop) & grepl("WRAPPER",rma_crops$crop),"TOBACCO CIGAR WRAPPER",rma_crops$crop)

rma_crops$crop <- ifelse(rma_crops$crop %in% c("GREEN PEAS"),"GREENS",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("CORN","HYBRID CORN SEED","GRP CORN","REVENUE COVERAGE CORN","INCOME PROTECTION CORN") ,"CORN",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("FRESH MARKET SWEET CORN","SWEET CORN","HYBRID SWEET CORN SEED") ,"SWEET CORN",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("GRAIN SORGHUM","HYBRID SORGHUM SEED","SILAGE SORGHUM","GRP GRAIN SORGHUM") ,"SORGHUM",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("WHEAT","GRP WHEAT","INCOME PROTECTION WHEAT") ,"WHEAT",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("RICE","HYBRID SEED RICE") ,"RICE",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("SOYBEANS","GRP SOYBEANS","REVENUE COVERAGE SOYBEANS") ,"SOYBEANS",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("PEANUTS","GRP PEANUTS") ,"PEANUTS",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("COTTON","COTTON EX LONG STAPLE","GRP COTTON","INCOME PROTECTION COTTON") ,"COTTON",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("POTATOES","SWEET POTATOES") ,"POTATOES",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("GRP FORAGE PRODUCTION","FORAGE PRODUCTION","ALFALFA SEED","FORAGE SEEDING",
                                               "ANNUAL FORAGE","GRASS SEED","PASTURE,RANGELAND,FORAGE","MIXED FORAGE"),"MIXED FORAGE",rma_crops$crop)

rma_crops$crop <- ifelse(grepl("BANANA",rma_crops$crop),"BANANAS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("LEMON",rma_crops$crop),"LEMONS/LIMES",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("LIME",rma_crops$crop),"LEMONS/LIMES",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TOMATOES",rma_crops$crop),"TOMATOES",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("AVOCADO",rma_crops$crop),"AVOCADOS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("PEPPERS",rma_crops$crop),"PEPPERS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("COFFEE",rma_crops$crop),"COFFEE",rma_crops$crop)

rma_crops$crop <- ifelse(grepl("PEACHES",rma_crops$crop),"PEACHES",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("APRICOTS",rma_crops$crop),"APRICOTS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("MANGO TREES",rma_crops$crop),"MANGOS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("NURSERY",rma_crops$crop),"NURSERY",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("PAPAYA",rma_crops$crop),"PAPAYA",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("PECAN",rma_crops$crop),"PECANS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("APPLE TREES",rma_crops$crop),"APPLES",rma_crops$crop)

# 211	      ALL OTHER CITRUS TREES  c("MURCOTT","TANGELO","TANGERINE)
# 248	      CITRUS IV               c("TANGERINES","TANGELOS","NAVEL ORANGES","EL ORANGES NO")
rma_crops$crop <- ifelse(grepl("TANGELO",rma_crops$crop),"TANGERINES/MANDARINS/TANGELOS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("TANGERINE",rma_crops$crop),"TANGERINES/MANDARINS/TANGELOS",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("MANDARIN",rma_crops$crop),"TANGERINES/MANDARINS/TANGELOS",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop_cd %in% c(211,248),"TANGERINES/MANDARINS/TANGELOS",rma_crops$crop)
# 245	      CITRUS I                c("EARLY ORANGES","MID-SEASON ORANGES")
# 251	      CITRUS VII              c("LATE ORANGES - FRESH" "GRAPEFRUIT - FRESH","FRESH")
# 252	      CITRUS VIII             c("NAVEL ORANGES")
# 240	      CITRUS TREES I          c("EARLY & MIDSEASON ORANGES" ,"LY & MIDSEASON ORANGES EMO")
# 241	      CITRUS TREES II         c("LATE ORANGES")
# 246	      CITRUS II               c("JUICE","LATE ORANGES - JUICE")
rma_crops$crop <- ifelse(rma_crops$crop_cd %in% c(245,251,252,240,241,246),"ORANGES",rma_crops$crop)
rma_crops$crop <- ifelse(grepl("ORANGE",rma_crops$crop),"ORANGES",rma_crops$crop)
# 250	      CITRUS VI               c("LEMONS","LIMES")
rma_crops$crop <- ifelse(rma_crops$crop_cd %in% 252,"LEMONS/LIMES",rma_crops$crop)
# 242	      CITRUS TREES III        c("ALL OTHER GRAPEFRUIT","OTHER GRAPEFRUIT AOG")
# 243	      CITRUS TREES IV         c("RIO RED & STAR RUBY GRAPEFRUIT","RED & STAR RUBY GRAPEFRUIT RRS")
# 244	      CITRUS TREES V          c("RUBY RED GRAPEFRUIT","Y RED GRAPEFRUIT RRG")
# 247	      CITRUS III              c("GRAPEFRUIT - JUICE")
rma_crops$crop <- ifelse(rma_crops$crop_cd %in% c(242,243,244,247),"GRAPEFRUIT",rma_crops$crop)
rma_crops$crop <- ifelse(rma_crops$crop %in% c("ALL OTHER GRAPEFRUIT","GRAPEFRUIT TREES","RUBY RED GRAPEFRUIT"),"GRAPEFRUIT",rma_crops$crop)

# 249	      CITRUS V                c("TEMPLES","MURCOTTS")

rma_crops$rma_id <- 1:nrow(rma_crops)

# Dry peas listings [https://www.rma.usda.gov/-/media/RMA/Policies/Dry-Pea/2022/Dry-Pea-Crop-Provisions-22-0067.ashx]
# Peas (Pisum sativum L.), 
# Austrian Peas (Pisum sativum spp arvense), 
# Fava or Faba beans (Vicia faba L.), 
# Lentils (Lens culinaris Medik.), 
# Chickpeas (Cicerarietinum L.),

#----------------------------------------
# NASS CROPS                          ####
nass_crops <- as.data.frame(readRDS("data-raw/release/nass/nass_production_data.rds"))
nass_crops$crop <- toupper(nass_crops$commodity_name)
nass_crops$crop_nass <- toupper(nass_crops$commodity_name)
nass_crops <- unique(nass_crops[c("crop_nass","crop")])
nass_crops$crop <- ifelse(nass_crops$crop %in% c("RASPBERRIES","BLACKBERRIES"), c("RASPBERRY AND BLACKBERRY"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("BEANS","LEGUMES"),c("DRY BEANS"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("CHICKPEAS","LENTILS"), c("DRY PEAS"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("HAY") ,"MIXED FORAGE",nass_crops$crop)   
nass_crops$crop <- ifelse(nass_crops$crop %in% c("SUGARBEETS"),c("SUGAR BEETS"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("FLAXSEED"), c("FLAX"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("SUNFLOWER"), c("SUNFLOWERS"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("WILD RICE"), c("CULTIVATED WILD RICE"),nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% c("NECTARINES"), "FRESH NECTARINES" ,nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% "PLUMS & PRUNES","FRESH PLUM" ,nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% "LEMONS","LEMONS/LIMES" ,nass_crops$crop)
nass_crops$crop <- ifelse(nass_crops$crop %in% "PAPAYAS","PAPAYA" ,nass_crops$crop)
nass_crops$crop <- ifelse(grepl("TANGELO",nass_crops$crop),"TANGERINES/MANDARINS/TANGELOS",nass_crops$crop)
nass_crops$crop <- ifelse(grepl("TANGERINE",nass_crops$crop),"TANGERINES/MANDARINS/TANGELOS",nass_crops$crop)
nass_crops$nass_id <- 1:nrow(nass_crops)

rma_nass <- rma_crops
rma_nass$crop <- ifelse(grepl("TOBACCO",rma_nass$crop),"TOBACCO",rma_nass$crop)
rma_nass$crop <- ifelse(rma_nass$crop %in% c("MACADAMIA NUTS","MACADAMIA TREES"), "MACADAMIAS",rma_nass$crop)
rma_nass$crop <- ifelse(rma_nass$crop %in% c( "TABLE GRAPES" ), "GRAPES",rma_nass$crop)
rma_nass$crop <- ifelse(rma_nass$crop %in% c( "HYBRID POPCORN SEED" ), "POPCORN",rma_nass$crop)
rma_nass$crop <- ifelse(rma_nass$crop %in% c("PROCESSING BEANS","FRESH MARKET BEANS"), "DRY BEANS",rma_nass$crop)

rma_nass_linker <- dplyr::inner_join(rma_nass,nass_crops,by=c("crop"))

rma_nass   <- rma_nass[!rma_nass$rma_id %in% rma_crops$rma_id,]
nass_crops <- nass_crops[!nass_crops$nass_id %in% rma_crops$nass_id,]

nasscrops <- unique(nass_crops$crop)
sobpcrops <- unique(rma_nass$crop)
done <- unique(rma_nass_linker$crop)
nasscrops <- nasscrops[order(nasscrops)]
sobpcrops <- sobpcrops[order(sobpcrops)]
nasscrops[!nasscrops %in% sobpcrops]
sobpcrops[!sobpcrops %in% nasscrops]

done[order(done)]

#saveRDS(rma_crops00,file=paste0(Dr.FCIP,"rmaInsuredShare/Output/nass_rma_crop_linker.rds"))

#----------------------------------------
# FSA CROPS                           ####
data(fsaCropAcreage)
fsa_crops <- unique(as.data.frame(fsaCropAcreage)[c("crop_cd","crop","crop_yr")]);rm(fsaCropAcreage)
names(fsa_crops) <- c("crop_cd_fsa","crop_fsa","crop_yr")
fsa_crops$crop_cd  <- fsa_crops$crop_cd_fsa
fsa_crops$fsa_id <- 1:nrow(fsa_crops)

fsa_crops$crop <- toupper(fsa_crops$crop_fsa)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("RICE, SWEET","INDUSTRIAL RICE","RICE- SWEET","RICE  SWEET"),"RICE",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("LIMES","LEMONS"),"LEMONS/LIMES",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("ALFALFA"),"ALFALFA SEED",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("CARAMBOLA (STAR FRUIT)"),"CARAMBOLA TREES",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("NECTARINES"),"FRESH NECTARINES",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("TANGERINES","TANGELOS"),"TANGERINES/MANDARINS/TANGELOS",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("SORGHUM",fsa_crops$crop) & grepl("DUAL",fsa_crops$crop),"GRAIN SORGHUM",fsa_crops$crop)
fsa_crops$crop <- ifelse(fsa_crops$crop %in% c("SORGHUM","SORGHUM DUAL PURPOSE"),"GRAIN SORGHUM",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("SORGHUM",fsa_crops$crop) & grepl("FORAGE",fsa_crops$crop),"SILAGE SORGHUM",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("RICE",fsa_crops$crop) & grepl("WILD",fsa_crops$crop),"RICE WILD",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("MARYLAND",fsa_crops$crop),"TOBACCO MARYLAND",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("BURLEY",fsa_crops$crop),"TOBACCO BURLEY",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("FLUE",fsa_crops$crop) & grepl("CURED",fsa_crops$crop),"TOBACCO FLUE CURED",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("FIRE",fsa_crops$crop) & grepl("CURED",fsa_crops$crop),"TOBACCO FIRE CURED",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("DARK",fsa_crops$crop) & grepl("AIR",fsa_crops$crop),"TOBACCO DARK AIR",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("CIGAR",fsa_crops$crop) & grepl("BINDER",fsa_crops$crop),"TOBACCO CIGAR BINDER",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("CIGAR",fsa_crops$crop) & grepl("FILLER",fsa_crops$crop),"TOBACCO CIGAR FILLER",fsa_crops$crop)
fsa_crops$crop <- ifelse(grepl("TOBACCO",fsa_crops$crop) & grepl("CIGAR",fsa_crops$crop) & grepl("WRAPPER",fsa_crops$crop),"TOBACCO CIGAR WRAPPER",fsa_crops$crop)

rma_fsa <- rma_crops

best00 <- c(11,16,28,29,31,34,37,38,39,53,54,58,60,75,78,81,83,84,87,91,94,114,132,147,154,158,396,470,501,1218,1302,6000,21,22,67,102,156)

rma_crops00 <- rma_fsa[rma_fsa$crop_cd %in% best00,]
rma_crops00 <- dplyr::inner_join(
  rma_crops00[c("crop_cd","crop_yr","crop","crop_rma","rma_id")],
  unique(fsa_crops[fsa_crops$crop_cd %in% best00,c("crop_cd","crop_fsa","crop_cd_fsa","fsa_id")]),by=c("crop_cd"))
rma_crops00 <- rma_crops00[order(rma_crops00$crop_cd),]


fsa_crops <- fsa_crops[!fsa_crops$fsa_id %in% rma_crops00$fsa_id,]
rma_fsa   <- rma_fsa[!rma_fsa$rma_id %in% rma_crops00$rma_id,]

rma_fsa$crop <- toupper(rma_fsa$crop)
rma_fsa$crop <- ifelse(rma_fsa$crop %in% "SWEET CORN","CORN",rma_fsa$crop)

rma_crops01 <- dplyr::inner_join(unique(rma_fsa[,c("crop_cd","crop_rma","crop","crop_yr","rma_id")]),
                                 unique(fsa_crops[,c("crop","crop_cd_fsa","crop_fsa","fsa_id")]),
                                 by=c("crop"))

rma_fsa_linker <- rbind(rma_crops00,rma_crops01)

fsa_crops <- fsa_crops[!fsa_crops$fsa_id %in% rma_fsa_linker$fsa_id,]
rma_fsa   <- rma_fsa[!rma_fsa$rma_id %in% rma_fsa_linker$rma_id,]

fsacrops <- unique(fsa_crops$crop)
sobpcrops <- unique(rma_fsa$crop)
done <- unique(rma_fsa_linker$crop)
fsacrops <- fsacrops[order(fsacrops)]
sobpcrops <- sobpcrops[order(sobpcrops)]
fsacrops[!nasscrops %in% sobpcrops]
sobpcrops[!sobpcrops %in% fsacrops]

done[order(done)]

#saveRDS(rma_crops00,file=paste0(Dr.FCIP,"rmaInsuredShare/Output/fsa_rma_crop_linker.rds"))

#CITRUS <- unique(rma_crops[grepl("CITRUS",rma_crops$crop),c("crop_cd","crop")])
#CITRUS[order(CITRUS$crop),]
#unique(recodes_typ[recodes_typ$crop_cd %in% 252,"typ"])
# crop_cd	  crop

#unique(rma_crops$crop)

# unique(rma_crops[grepl("TANGELO",rma_crops$crop),c("crop_cd","crop")])
# unique(fsa_crops[grepl("TANGELO",fsa_crops$crop),c("crop_cd","crop")])
# unique(rma_crops00[grepl("TANGELO",rma_crops00$crop),c("crop_cd","crop")])

#perform fuzzy matching left join
#rma_crops$rma_crops <- rma_crops$crop
# rma_crops02 <- stringdist_join(
#   unique(fsa_crops[,c("crop","crop_fsa")]), 
#   unique(rma_crops[,c("crop","rma_crops")]), 
#   by='crop', #match based on team
#   mode='left', #use right join
#   method = "jw", #use jw distance metric
#   max_dist=99, 
#   distance_col='dist') %>%
#   group_by(crop.x) %>%
#   slice_min(order_by=dist, n=1)
# rma_crops02 <- as.data.frame(rma_crops02)
# rma_crops02 <- rma_crops02[order(rma_crops02$crop.x),]
#----------------------------------------
# CROP linker                         ####

linker <- dplyr::full_join(rma_crops,unique(rma_nass_linker[c("rma_id","crop_nass")]),by="rma_id")
linker <- dplyr::full_join(linker,unique(rma_fsa_linker[c("rma_id","crop_fsa","crop_cd_fsa")]),by="rma_id")
linker <- unique(linker[c("crop_yr","crop_cd","crop_rma","crop_nass","crop_fsa","crop_cd_fsa","crop")])
#linker <- linker[complete.cases(linker),]

linker <- as.data.table(linker)


