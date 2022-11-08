# chinook-bycatch-public
Final R code for publication on Chinook bycatch in the hake fishery
** The raw data files for this analysis are confidential and cannot be shared under the Magnuson-Stevens Act at section 402(b), 16 U.S.C. 1881a(b). Therefore, the analyses from this project are not reproducible; however, we are sharing the code to show our work as much as possible.

Files include:

1. "1_tidying_observer_data.R"
- This file cleans the observer dataset including organizing and checking for and dealing with any errors.
- Output is an updated file for the observer data (unique row id: haul_join): data_by_haul_v1.R

2. "2_tidying_gsi_data.R"
- This file cleans the genetic dataset including organizing and checking for and dealing with any errors. It also combines the genetic data with the observer data from the prior script.
- Output is an updated file for gsi data (unique row id: samp_no): data_by_fish_v1.R
- Output is an updated file for summarised gsi data by haul (unique row id: haul_join + esu): data_by_haul_and_esu_v1.R

3. "3_gathering_covariates.R"
- This file gathers the covariates by haul: sea surface temperature (SST), day vs. night (defined between local sunrise and sunset), and distances from shore and the 200-m isobath.
- Output is an updated file for data by haul and esu, which includes covariates and has dropped rows with relevant NAs: data_by_haul_and_esu_v2.R

4. "4_run_models.R"
- This file runs Generalized Additive Models to test hypotheses and looks at model diagnostics.

5. "5_make_figures.R"
- This file needs to have models and files loaded from script (4_run_models.R) first. Then this file builds various figures for the manuscript and Supporting Information.
