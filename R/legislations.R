#' U.S. Agricultural Legislation Titles
#'
#' A named list of major U.S. farm bills and crop insurance acts from 1938 to 2018.
#' Each entry includes the full official title and public law citation, along with a common short name.
#'
#' @format A named list with 8 elements:
#' \describe{
#'   \item{FB1938}{Agricultural Adjustment Act of 1938 (P.L. 75-430, "1938 Act")}
#'   \item{FB1980}{Federal Crop Insurance Act of 1980 (P.L. 96-365, "1980 Act")}
#'   \item{FB1994}{Federal Crop Insurance Reform and Department of Agriculture Reorganization Act of 1994 (P.L. 103-354, "1994 Act")}
#'   \item{FB1996}{Federal Agriculture Improvement and Reform Act of 1996 (P.L. 104-127, "1996 Farm Bill")}
#'   \item{FB2000}{Agricultural Risk Protection Act of 2000 (P.L. 106-224, "ARPA")}
#'   \item{FB2008}{Food, Conservation, and Energy Act of 2008 (P.L. 110-246, "2008 Farm Bill")}
#'   \item{FB2014}{Agricultural Act of 2014 (P.L. 113–79, "2014 Farm Bill")}
#'   \item{FB2018}{Agricultural Improvement Act of 2018 (P.L. 115-334, "2018 Farm Bill")}
#' }
#'
#' @usage data(legislations)
#' @keywords internal
legislations <- list(
  # Created the first major U.S. farm subsidy and supply control system during the Great Depression.
  AAA1933 = 'Agricultural Adjustment Act of 1933 (P.L. 73-10, "1933 Act")',
  
  # Formalized permanent commodity programs, acreage allotments, and price supports.
  FB1938 = 'Agricultural Adjustment Act of 1938 (P.L. 75-430, "1938 Act")',
  
  # Expanded crop insurance coverage and established the Federal Crop Insurance Corporation (FCIC).
  FB1980 = 'Federal Crop Insurance Act of 1980 (P.L. 96-365, "1980 Act")',
  
  # Made crop insurance mandatory (briefly), then voluntary with subsidies; major USDA reorganization.
  FB1994 = 'Federal Crop Insurance Reform and Department of Agriculture Reorganization Act of 1994 (P.L. 103-354, "1994 Act")',
  
  # Introduced decoupled payments (transition payments) and removed most supply controls.
  FB1996 = 'Federal Agriculture Improvement and Reform Act of 1996 (P.L. 104-127, "1996 Farm Bill")',
  
  # Strengthened crop insurance subsidies and introduced revenue insurance options.
  FB2000 = 'Agricultural Risk Protection Act of 2000 (P.L. 106-224, "ARPA")',
  
  # Added energy and conservation titles; expanded nutrition assistance (SNAP).
  FAIR2002 = 'Farm Security and Rural Investment Act of 2002 (P.L. 107-171, "2002 Farm Bill")',
  
  # Emphasized renewable energy, conservation, and specialty crop support.
  FB2008 = 'Food, Conservation, and Energy Act of 2008 (P.L. 110-246, "2008 Farm Bill")',
  
  # Repealed direct payments; introduced ARC/PLC programs; expanded crop insurance.
  FB2014 = 'Agricultural Act of 2014 (P.L. 113–79, "2014 Farm Bill")',
  
  # Continued ARC/PLC with tweaks; added hemp legalization and minor conservation reform.
  FB2018 = 'Agricultural Improvement Act of 2018 (P.L. 115-334, "2018 Farm Bill")',
  
  # Overhauled food safety regulation to emphasize prevention and modernize inspections.
  FSMA2011 = 'Food Safety Modernization Act of 2011 (P.L. 111-353)',
  
  # Provided $20+ billion for USDA conservation and climate-smart ag programs via reconciliation.
  FB2023 = 'Inflation Reduction Act of 2022 (P.L. 117-169, "Climate & Conservation Funding")',
  
  # Created the National School Lunch Program to address child nutrition and surplus food.
  SC1946 = 'National School Lunch Act of 1946 (P.L. 79-396)',
  
  # Established U.S. food aid program ("Food for Peace"); tied food surplus to foreign assistance.
  FPA1954 = 'Agricultural Trade Development and Assistance Act of 1954 (P.L. 83-480, "Food for Peace")',
  
  # Introduced Conservation Reserve Program (CRP), Sodbuster and Swampbuster provisions.
  FPA1985 = 'Food Security Act of 1985 (P.L. 99-198, "1985 Farm Bill")',
  
  # Expanded conservation programs and addressed sustainability, food safety, and rural development.
  FTAA1990 = 'Food, Agriculture, Conservation, and Trade Act of 1990 (P.L. 101-624, "1990 Farm Bill")'
)
