# Insurance pool identifier fields
FCIP_INSURANCE_POOL <- c("state_code","county_code","commodity_code","type_code","practice_code")

# Insurance election identifier fields
FCIP_INSURANCE_ELECTION <- c("unit_structure_code","insurance_plan_code","coverage_type_code","coverage_level_percent")

# Insurance election identifier fields (recoded)
FCIP_INSURANCE_ELECTION_RCODED <- c("unit_structure_recode","insurance_plan_recode","coverage_type_code","coverage_level_percent")

# Column names to coerce to numeric
FCIP_FORCE_NUMERIC_KEYS <- c("commodity_year",FCIP_INSURANCE_POOL,"record_category_code","insurance_plan_code","coverage_level_percent")


