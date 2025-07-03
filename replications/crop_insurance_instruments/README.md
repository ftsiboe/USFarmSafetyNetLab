Crop-Insurance-Instruments
================

-   <a href="#crop-insurance-instruments"
    id="toc-crop-insurance-instruments">Crop-Insurance-Instruments</a>
    -   <a href="#files-in-this-directory"
        id="toc-files-in-this-directory">Files in this directory</a>
    -   <a
        href="#definitions-for-each-data-column-in-the-final_fcip_instruments-are-included-below"
        id="toc-definitions-for-each-data-column-in-the-final_fcip_instruments-are-included-below">Definitions
        for each data column in the final_fcip_instruments are included
        below</a>
    -   <a href="#-citation" id="toc--citation">📚 Citation</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Crop-Insurance-Instruments

The folder provides data and replication codes for the United States
Crop Insurance Program as described in

-   [Econometric identification of crop insurance
    participation](https://doi.org/10.1017/age.2023.13)

-   [The crop insurance demand response to premium subsidies: Evidence
    from US Agriculture](https://doi.org/10.1016/j.foodpol.2023.102505)

-   [Effects of crop insurance premium subsidies on crop
    acreage](https://doi.org/10.1093/ajae/aax058)

Database was last updated on 2025-06-11

## Files in this directory

| File                                                                               | Description                                                                                                                        |
|:-----------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------|
| `2023 TsiboeTurner Econometric-identification-of-crop-insurance-participation.pdf` | Published article for the proposed instrument ([doi:10.1017/age.2023.13](https://doi.org/10.1017/age.2023.13))                     |
| `exogeneity_justification.docx`                                                    | Exogeneity argument for the proposed instruments                                                                                   |
| `2009 FCIC Rate Methodology Handbook APH.pdf`                                      | 2009 FCIC Rate Methodology Handbook APH published by RMA ([legacy PDF](https://legacy.rma.usda.gov/pubs/2008/ratemethodology.pdf)) |
| `fcip_instruments_formulation.R`                                                   | Script to formulate the instrument at the county–crop level on a historical basis                                                  |

## Definitions for each data column in the final_fcip_instruments are included below

| Column               | Definition                                                                                                                                                                                                                                                 |
|:---------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `commodity_year`     | The identifier that represents the year in which the crop/commodity is normally harvested and indicates the policy year for which coverage was provided.                                                                                                   |
| `state_abbreviation` | USPS state abbreviation.                                                                                                                                                                                                                                   |
| `state_code`         | The FIPS code that denotes the State in which the insured farm is located.                                                                                                                                                                                 |
| `county_name`        | Name of the county.                                                                                                                                                                                                                                        |
| `county_code`        | The FIPS code that denotes the County in which the insured farm is located.                                                                                                                                                                                |
| `commodity_name`     | Name of the crop/commodity.                                                                                                                                                                                                                                |
| `commodity_code`     | The Risk Management Agency (RMA) code that denotes the crop/commodity for which the policy is issued.                                                                                                                                                      |
| `tau_sob`            | An approximation of the “target rate” as described by Tsiboe & Turner (2023). Tracks changes in the cost of crop insurance that are plausibly exogenous to any one producer; estimated from historic SOB data.                                             |
| `tau_sob`            | An approximation of the “target rate” as described by Tsiboe & Turner (2023). Tracks changes in the cost of crop insurance that are plausibly exogenous to any one producer; estimated by aggregating directly from RMA’s Actuarial Data Master.           |
| `tau_final`          | Same as `tau_adm`, with any missing values filled in using `tau_sob`.                                                                                                                                                                                      |
| `subsidy_rate_65`    | National subsidy rate for 65% coverage‐level individual crop insurance policies. Constructed as described by Yu et al. (2018) (see footnote on Figure 1). Can be used as an instrument on its own or interacted with `tau` for greater temporal variation. |
| `subsidy_rate_75`    | National subsidy rate for 75% coverage‐level individual crop insurance policies. Constructed as described by Yu et al. (2018) (see footnote on Figure 1). Can be used as an instrument on its own or interacted with `tau` for greater temporal variation. |

## 📚 Citation

If you find this repository useful, please star this project and cite
our papers listed above.

See the [LICENSE](../LICENSE) file in the repository’s root for details.

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
