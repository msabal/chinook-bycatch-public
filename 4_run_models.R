# Building Statistical Models

# Outline of script ----
# 1 - Set settings
# 2 - Import data



# 1 - Set settings ----

# Load libraries
library(tidyverse); library(lubridate)
library(modelr); library(car)
library(mgcv); library(sf)
library(boot); library(mgcViz)
library(viridis); library(gridExtra); library(ggpubr)
library(ape); library(ragg)

# set settings for print (to be like head()) 
#options(tibble.print_max = 5)
options(tibble.width = Inf)

# what to do with NAs
options(na.action = na.fail)

options(scipen = 9999)


# 2 - Import data ----

# Location where my files are stored
in_drive <- "C:/Users/sabalm/Documents/Chinook-Hake-Bycatch-Data/Data/"

# Load full dataset by haul + ESU including covariates after cleaning.
full_dat <- readRDS(str_c(in_drive, "Saved Files/data_by_haul_and_esu_v2.rds")) # N = 54509


# 3 - Build Model Data sets

# Make data sets with only covariates needed for model with no NAs

# Get data set with only haul data (drop rows with ESU data)
haul_dat <- full_dat %>% 
  ungroup() %>% 
  dplyr::select(-esu, -pa_esu, -catch_esu, -catch_esu_cp, -catch_esu_ia_8) %>%
  dplyr::select(haul_join, chinook_count, lon, lat, year, doy, time_num, fishing_m, bottom_m, duration, sst_mean) %>% 
  mutate(pa = ifelse(chinook_count == 0, 0, 1),
         bpue = chinook_count / duration * 60) %>%  # Chinook bycatch-per-unit-effort (BPUE) is number of Chinook caught per hour.
  distinct() # N = 54509

# Dataset for positive Chinook catch only model
haul_dat_p <- haul_dat %>% filter(chinook_count > 0) # N = 8534

# Make nested tibble by focal stock
esu_dat <- full_dat %>% 
  filter(esu %in% c("klam_trinity", "sor_nca", "or_coast", "pug", "so_bc") & !is.na(catch_esu_ia_8) & year %in% c(2008:2015)) %>%
  dplyr::select(esu, catch_esu_ia_8, pa_esu, catch_esu, lon, lat, year, doy, time_num, fishing_m, bottom_m, duration, sst_mean) %>% 
  drop_na() %>%  # N=46,313 (39,002 hauls with zero catch)
  group_by(esu) %>% 
  nest()
esu_dat # N = 19,491 per esu


# 4 - Build Model Functions ----

# Make function to run the full model (not ESU-specific): haul-level chinook catch
gam_all <- function(df){
  bam(chinook_count ~ te(lon, lat, bs='tp') +  as.factor(year) + s(doy, k=10) + s(bottom_m) +       # space-time covariates
        te(time_num, fishing_m, bs = c("cc","tp")) +   te(sst_mean, fishing_m) +                     # mechanism covariates
        offset(log(duration)), family=quasipoisson(link="log"), data = df, method = "REML") 
} # end of function.


# Make function to run the full model (not ESU-specific): probability of occurrence (SI)
gam_all_pa <- function(df){
  bam(pa ~ te(lon, lat, bs='tp') +  as.factor(year) + s(doy, k=10) + s(bottom_m) +       # space-time covariates
        te(time_num, fishing_m, bs = c("cc","tp")) +   te(sst_mean, fishing_m) +                     # mechanism covariates
        offset(log(duration)), family=binomial(link="logit"), data = df, method = "REML") 
} # end of function.


# Make function for stock-specific model: stock-specific haul-level catch
gam_all_esu_c8 <- function(df){
  gam(catch_esu_ia_8 ~ te(lon, lat, bs='tp') + s(bottom_m) + as.factor(year) + s(doy, k=10) + 
        te(sst_mean, fishing_m) + te(time_num, fishing_m, bs = c("cc","tp")) +
        offset(log(duration)), family=quasipoisson(link="log"), data = df, method = "REML")
} # end of function.


# Make function for stock-specific model: stock-specific haul-level probability of occurrence (SI)
gam_all_esu_c8_pa <- function(df){
  gam(pa_esu ~ te(lon, lat, bs='tp') + s(bottom_m) + as.factor(year) + s(doy, k=10) + 
        te(sst_mean, fishing_m) + te(time_num, fishing_m, bs = c("cc","tp")) +
        offset(log(duration)), family=binomial(link="logit"), data = df, method = "REML")
} # end of function.


# Make function for stock-specific model: stock-specific haul-level catch
gam_all_esu_c0 <- function(df){
  gam(catch_esu ~ te(lon, lat, bs='tp') + s(bottom_m) + as.factor(year) + s(doy, k=10) + 
        te(sst_mean, fishing_m) + te(time_num, fishing_m, bs = c("cc","tp")) +
        offset(log(duration)), family=quasipoisson(link="log"), data = df, method = "REML")
} # end of function.



# Make function to run the full model (not ESU-specific): haul-level chinook catch
gam_all_nb <- function(df){
  bam(chinook_count ~ te(lon, lat, bs='tp') +  as.factor(year) + s(doy, k=10) + s(bottom_m) +       # space-time covariates
        te(time_num, fishing_m, bs = c("cc","tp")) +   te(sst_mean, fishing_m) +                     # mechanism covariates
        offset(log(duration)), family=nb(link="log"), data = df, method = "REML") 
} # end of function.

# 5 - Run models & check diagnostics ----

## 5.1 - All Chinook ----
full_mod <- gam_all(haul_dat) # All Chinook

# Summary & diagnostics (Table S1)
summary(full_mod) # significance, test statistics, deviance explained, adj-R squared.
gam.check(full_mod, k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(full_mod, full=F)), digits=3) # check if covariates have concurvity: 0.850 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(full_mod, scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

plot(exp(predict(full_mod)) ~ (haul_dat$chinook_count))
hist(residuals(full_mod, type="deviance"), breaks=50)


# 5.2 - By ESU (stock-specific) ----
esu_dat <- esu_dat %>% 
  mutate(gam_esu_c8 = map(data, gam_all_esu_c8)) # data is the column name in the nested tibble.
esu_dat

# Summary & diagnostics
# Klamath - Trinity
summary(esu_dat$gam_esu_c8[[1]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8[[1]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8[[1]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8[[1]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. OR - N. CA
summary(esu_dat$gam_esu_c8[[5]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8[[5]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8[[5]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8[[5]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# OR Coast
summary(esu_dat$gam_esu_c8[[2]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8[[2]], k.rep=1000) # convergence, k > edf, no patterns in residuals from p-value.
round(as.data.frame(concurvity(esu_dat$gam_esu_c8[[2]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8[[2]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# Puget Sound
summary(esu_dat$gam_esu_c8[[3]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8[[3]], k.rep=1000) # convergence, k > edf, no patterns in residuals from p-value.
round(as.data.frame(concurvity(esu_dat$gam_esu_c8[[3]], full=F)), digits=3) # check if covariates have concurvity: 0.849 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8[[3]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. BC
summary(esu_dat$gam_esu_c8[[4]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8[[4]], k.rep=1000) # convergence, k > edf, no patterns in residuals from p-value.
round(as.data.frame(concurvity(esu_dat$gam_esu_c8[[4]], full=F)), digits=3) # check if covariates have concurvity: 0.849 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8[[4]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.


## 5.3 - All Chinook: (SI) ----
full_mod_p <- gam_all(haul_dat_p) # All Chinook

# Summary & diagnostics
summary(full_mod_p) # significance, test statistics, deviance explained, adj-R squared.
gam.check(full_mod_p, k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(full_mod_p, full=F)), digits=3) # none > 0.7
plot(full_mod_p, scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

## 5.4 - All Chinook: Probability of occurrence (SI) ----
full_mod_pa <- gam_all_pa(haul_dat) # All Chinook

# Summary & diagnostics
summary(full_mod_pa) # significance, test statistics, deviance explained, adj-R squared.
gam.check(full_mod_pa, k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(full_mod_pa, full=F)), digits=3) # check if covariates have concurvity: 0.850 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(full_mod_pa, scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

## 5.5 - All Chinook: All catch negative binomial ----
full_mod_nb <- gam_all_nb(haul_dat) # All Chinook

# Summary & diagnostics
summary(full_mod_nb) # significance, test statistics, deviance explained, adj-R squared.
gam.check(full_mod_nb, k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(full_mod_nb, full=F)), digits=3) # check if covariates have concurvity: 0.850 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(full_mod_nb, scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.




# 5.6 - By ESU (stock-specific): Probability of occurrence (SI) ----
esu_dat <- esu_dat %>% 
  mutate(gam_esu_c8_pa = map(data, gam_all_esu_c8_pa)) # data is the column name in the nested tibble.
esu_dat

# Summary & diagnostics
# Klamath - Trinity
summary(esu_dat$gam_esu_c8_pa[[1]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8_pa[[1]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8_pa[[1]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8_pa[[1]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. OR - N. CA
summary(esu_dat$gam_esu_c8_pa[[5]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8_pa[[5]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8_pa[[5]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8_pa[[5]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# OR Coast
summary(esu_dat$gam_esu_c8_pa[[2]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8_pa[[2]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8_pa[[2]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_esu_c8_pa[[2]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# Puget Sound
summary(esu_dat$gam_esu_c8_pa[[3]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8_pa[[3]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8_pa[[3]], full=F)), digits=3) # none > 0.7
plot(esu_dat$gam_esu_c8_pa[[3]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. BC
summary(esu_dat$gam_esu_c8_pa[[4]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_esu_c8_pa[[4]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_esu_c8_pa[[4]], full=F)), digits=3) # none > 0.7
plot(esu_dat$gam_esu_c8_pa[[4]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.


# 5.7 - By ESU (stock-specific): All catch: No threshold ----
esu_dat <- esu_dat %>% 
  mutate(gam_all_esu_c0 = map(data, gam_all_esu_c0)) # data is the column name in the nested tibble.
esu_dat

# Summary & diagnostics
# Klamath - Trinity
summary(esu_dat$gam_all_esu_c0[[1]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_all_esu_c0[[1]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_all_esu_c0[[1]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_all_esu_c0[[1]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. OR - N. CA
summary(esu_dat$gam_all_esu_c0[[5]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_all_esu_c0[[5]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_all_esu_c0[[5]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_all_esu_c0[[5]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# OR Coast
summary(esu_dat$gam_all_esu_c0[[2]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_all_esu_c0[[2]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_all_esu_c0[[2]], full=F)), digits=3) # check if covariates have concurvity: 0.859 between bottom_m and te(lon, lat), makes sense, GAMs can handle with large datasets.
plot(esu_dat$gam_all_esu_c0[[2]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# Puget Sound
summary(esu_dat$gam_all_esu_c0[[3]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_all_esu_c0[[3]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_all_esu_c0[[3]], full=F)), digits=3) # none > 0.7
plot(esu_dat$gam_all_esu_c0[[3]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.

# S. BC
summary(esu_dat$gam_all_esu_c0[[4]]) # significance, test statistics, deviance explained, adj-R squared.
gam.check(esu_dat$gam_all_esu_c0[[4]], k.rep=1000) # convergence, k > edf
round(as.data.frame(concurvity(esu_dat$gam_all_esu_c0[[4]], full=F)), digits=3) # none > 0.7
plot(esu_dat$gam_all_esu_c0[[4]], scheme=2, pages=1, shade=T) # quick view of covariate marginal effects.


