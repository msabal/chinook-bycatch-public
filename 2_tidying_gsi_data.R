
# Data Tidying: Connecting GSI with Observer datasets.

# Outline of script ----
# 1 - Set settings
# 2 - Import 3 files (haul_dat, fish_dat, join_dat) and re-organize files respectively
# 3 - Merge files and look for & fix data issues
# 4 - Save file: merged data by individual fish 
# 5 - Summarize gsi data by haul and add all hauls to data set
# 6 - Final cleaning & Save file: merged data by haul


# 1 - Set settings ----

# Load libraries
library(tidyverse); library(lubridate)

# Set settings for print (to be like head())
#options(tibble.print_max = 5)
options(tibble.width = Inf)


# 2 - Import 3 files (haul_dat, fish_dat, join_dat) and re-organize files respectively ----

## 2.1 - Import data files ----

# Location where my files are stored
in_drive <- "C:/Users/sabalm/Documents/Chinook-Hake-Bycatch-Data/Data/"

haul_dat <-readRDS(str_c(in_drive, "Saved Files/data_by_haul_v1.rds")) # read in pre-cleaned haul data.
fish_dat <- read_csv(str_c(in_drive, "Raw Files/A-SHOP_2008-2015_2021_03_18.csv"))
join_dat <- read_csv(str_c(in_drive, "Raw Files/ASHOP-SampnoHauljoin2008-18.csv"))

# Make tibbles
haul_dat <- as_tibble(haul_dat)
fish_dat <- as_tibble(fish_dat)
join_dat <- as_tibble(join_dat)

# Look at columns and classes
colnames(haul_dat)
colnames(fish_dat)
colnames(join_dat)

str(haul_dat)
str(fish_dat)
str(join_dat)


## 2.2- Clean observer dataset ----

# This dataset was already cleaned in the R script (1_tidying_observer_data.R), so nothing to do here.



## 2.3 - Clean fish_dat ----

# Make date into date format
fish_dat <- mutate(fish_dat, Date = mdy(Date))

# Clean up fish_dat: drop unnecessary column and rename others.
fish_dat$`0` <- NULL
fish_dat <- rename(fish_dat, c(samp_no = Sampno,
                             date = Date,
                             year = YEAR,
                             day = Day,
                             season = Period,
                             lat_gsi = Lat,
                             lon_gsi = Long,
                             area = Area,
                             fishing_depth_m = 'Fishing depth (m)',
                             bottom_depth_m = 'Bottom depth (m)',
                             sex = SEX,
                             length = LENGTH,
                             weight = WEIGHT,
                             snout_barcode = SALMON_SNOUT_BARCODE,
                             adipose = ADIPOSE_PRESENT,
                             cwt_fish_match = 'cwt-gsi_match',
                             esu = ESU,
                             prob_esu = 'P-ESU',
                             pop = Pop,
                             prob_pop = 'P-Pop',
                             loci_typed = 'Loci typed',
                             sac_wi = 'Sacramento Wi',
                             cv_sp = 'Central Valley Sp',
                             cv_fa = 'Central Valley Fa',
                             ca_coast = 'California Coast',
                             klam_trinity = 'Klamath/Trinity',
                             sor_nca = 'S Oregon/N California',
                             or_coast = 'Oregon Coast',
                             wa_coast = 'Washington Coast',
                             l_col = 'L Columbia R',
                             u_will = 'U Willamette R',
                             m_col_sp = 'Mid-Columbia R Sp',
                             u_col_sp = 'U Columbia R Sp',
                             des_sufa = 'Deschutes R Su/Fa',
                             u_col_sufa = 'U Columbia R Su/Fa',
                             sn_spsu = 'Snake R Sp/Su',
                             sn_fa = 'Snake R Fa',
                             pug = 'Puget Sound',
                             so_bc = 'Southern BC',
                             c_bcak = 'Central BC-AK'))

# Also update fish_dat$esu character/factor levels same as above.
fish_dat$esu <- as.factor(fish_dat$esu)
levels(fish_dat$esu)

fish_dat <- fish_dat %>% 
  mutate(esu = fct_recode(esu,
                          "cv_sp" = 'Central Valley Sp',
                          "cv_fa" = 'Central Valley Fa',
                          "ca_coast" = 'California Coast',
                          "klam_trinity" = 'Klamath/Trinity',
                          "sor_nca" = 'S Oregon/N California',
                          "or_coast" = 'Oregon Coast',
                          "wa_coast" = 'Washington Coast',
                          "l_col" = 'L Columbia R',
                          "u_will" = 'U Willamette R',
                          "m_col_sp" = 'Mid-Columbia R Sp',
                          "u_col_sp" = 'U Columbia R Sp',
                          "des_sufa" = 'Deschutes R Su/Fa',
                          "u_col_sufa" = 'U Columbia R Su/Fa',
                          "sn_spsu" = 'Snake R Sp/Su',
                          "sn_fa" = 'Snake R Fa',
                          "pug" = 'Puget Sound',
                          "so_bc" = 'Southern BC',
                          "c_bcak" = 'Central BC-AK'))

# reorder factor levels from south to north.
fish_dat$esu <- factor(fish_dat$esu, levels = c("cv_sp" ,
                                              "cv_fa",
                                              "ca_coast",
                                              "klam_trinity",
                                              "sor_nca",
                                              "or_coast",
                                              "wa_coast",
                                              "l_col",
                                              "u_will",
                                              "m_col_sp",
                                              "u_col_sp",
                                              "des_sufa",
                                              "u_col_sufa",
                                              "sn_spsu",
                                              "sn_fa",
                                              "pug",
                                              "so_bc",
                                              "c_bcak"))


count(fish_dat, esu) # sample sizes of genotyped fish for each esu


## 2.4 - Clean join_dat ----

# To link the haul_join fields in the haul_dat with the fish_dat we will first 
# need to link samp_no in fish_dat with haul_join in join_dat, and then link
# fish_dat (with haul_join) with haul_dat. However, the haul_join fields don't
# look quite the same.

# Check to see if a HAUL_JOIN in the join_dat shows up in either haul_join column
# in haul_dat. See how the formatting differs.
haul_dat %>%
  filter(grepl("20151003681000001814", haul_join))
# Returns one haul! It more closely matches "haul_join" over "unique_haul_id" in
# the haul_dat. But has a HJ in front.

# Try another one
haul_dat %>%
  filter(grepl("11940003245000000966", haul_join))
# Cool, the same pattern. Next, I need to change the formatting for join_dat by
# (1) adding an HJ in the front and (2) dropping the extra numeric values after the -.


# Clean up join_dat: (1) change Samp_no to samp_no to match with cleaned up fish_dat,
# (2) make HAUL_JOIN to haul_join to match with haul_dat.

join_dat <- join_dat %>% 
  rename(c(samp_no = Samp_no,
           haul_join = HAUL_JOIN)) %>%                 # change column names to match haul_dat
  mutate(haul_join = str_c("HJ", haul_join)) %>%       # take chr-values in haul_join and add an "HJ" to the front so matches format in haul_dat
  mutate(haul_join = str_remove(haul_join, "\\-.*"))   # take chr-values in haul_join and remove all values after the first "-". \\ (indicates first occurrence), - (symbol), . (all remaining values), * (any amount of times)


# 3 - Merge files and look for data issues ----

## 3.1 - Merge and check shared variables match  ----

# Join datasets together! First, join fish_dat with join_dat to get the column
# haul_join in fish_dat. Then join fish_dat with haul_dat to get all other variables.

fish_dat2 <- fish_dat %>% 
  left_join(join_dat, by = "samp_no") %>% 
  left_join(haul_dat, by = "haul_join")


# Check if the duplicate columns originally from haul_dat and fish_dat are the same (e.g., bottom_depths, etc.)

# Check that date of the haul from the fish_dat (date) is the same as the date from the haul_dat (date_obs) 
fish_dat2 %>% 
  filter(date != date_haul_dep) %>% 
  dplyr::select(-11:-42) # Drop fish-specific columns to focus more easily on the haul data.
# When date_obs is deployment date, 308 fish have date from gsi that matches from retrieval date.

fish_dat2 %>% 
  filter(date != date_haul_ret) %>% 
  dplyr::select(-11:-42)
# When date_obs is retrieval date 218 fish have date from gsi that matches from deployment date.


# Make a new fish_date column where I change the midnight haul dates to match retrieval 
# date. Use this new column when I want to connect two datasets by date.
fish_dat2 <- fish_dat2 %>% 
  mutate(date_gsi = as_date(ifelse(date == date_haul_ret, date, date_haul_ret)))

# Check again
fish_dat2 %>% 
  filter(date_gsi != date_haul_ret) %>% 
  dplyr::select(-11:-42) # drop fish-specific columns to focus more easily on the haul data.
# Good, now 0 rows are returned where dates do not match between gsi and obs datasets.


# Are there any NAs in date_gsi?
filter(fish_dat2, is.na(date_gsi)) # Yes the ones where there was an issue with joining hy haul_join. Will fix this later.


# View lat then lon columns to see if they appear to match - they do.
dplyr::select(fish_dat2, lat_gsi, avg_lat, latdd_start, latdd_end)
dplyr::select(fish_dat2, lon_gsi, avg_long, londd_start, londd_end)

# Are there any instances where lat_gsi (from fish_dat) is more than 0.1 degree 
# different than avg_lat (from haul_dat)?
fish_dat2 %>% 
  filter(!abs(lat_gsi - avg_lat) < 0.1)
# 0 rows where  lat_gsi and avg_lat vary by more than 0.1 degree

# Are there any instances where lon_gsi (from fish_dat) is more than 0.1 degree 
# different than avg_long (from haul_dat)?
fish_dat2 %>% 
  filter(!abs(lon_gsi - avg_long) < 0.1)
# 0 rows where lon_gsi and avg_long vary by more than 0.1 degree

# Check if fishing and bottom depth columns seem similar between gsi and obs dats.
check_depth <- fish_dat2 %>% 
  dplyr::select(fishing_depth_m, fishing_fa_to_m, fishing_depth_fathoms, bottom_depth_m, bottom_fa_to_m, bottom_depth_fathoms)
check_depth

# Are there any instances where fishing_depth_m (from fish_dat) is more than 1 m
# different than fishing_fa_to_m (converted from haul_dat column fishing_depth_fathoms)?
check_depth %>% 
  filter(!abs(fishing_depth_m - fishing_fa_to_m) < 1)
# 0 rows where fishing depths vary by more than 1 meter.

# Are there any instances where bottom_depth_m (from fish_dat) is more than 1 m
# different than bottom_fa_to_m (converted from haul_dat column bottom_depth_fathoms)?
check_depth %>% 
  filter(!abs(bottom_depth_m - bottom_fa_to_m) < 1)
# 0 rows where bottom depths vary by more than 1 meter.

# Summary: GSI haul attributes match well with observer data attributes with the
# exception of some dates where the haul occurs over midnight.


## 3.2 - Fix wrong haul_join values ----

# Check for hauls where  the gsi data and observer data didn't match up to fish_dat2(returned NAs) - found 19 hauls

missing_haul_dat <- fish_dat2 %>% 
  filter(is.na(chinook_count)) %>% 
  group_by(haul_join) %>% 
  summarise(N_samp_no = n_distinct(samp_no))

# I manually/visually was able to take the single fish (samp_no = 90601-833717) from
# haul (haul_join = HJ17349001607000000000) and by matching near-enough lat_gsi
# with latdd_end, lon_gsi with londd_end, fishing_depth_fathoms with fishing_depth_m,
# and bottom_depth_fathoms with bottom_depth_m...realized the matching observer
# data haul_join should actually be HJ17349001607000004932! Now will do this legit
# in R for all wrong fish/haul_joins.

# How many fish need updated haul_join values?
sum(missing_haul_dat$N_samp_no) #287 fish that need a new haul number.

# What fish samp_no need updated haul_join values?
fish_need_haul <- fish_dat2 %>% 
  filter(is.na(chinook_count)) %>% 
  dplyr::select(samp_no) %>% 
  distinct()
# 287 fish that need a new haul from haul_dat because the one from join_dat/fish_dat2is wrong.


# Get subset of fish_dat2columns originally from gsi dataset but only the rows where they will need to be fixed/updated.
fix_gsi <- fish_dat2 %>% 
  dplyr::select(samp_no, date, lat_gsi, lon_gsi, fishing_depth_m, bottom_depth_m, chinook_count) %>% 
  filter(is.na(chinook_count))

# Get subset of columns from haul_dat that will be used to match to haul related cols in the original gsi dataset.
fix_obs <- haul_dat %>% 
  dplyr::select(haul_join, date_haul_ret, date_haul_dep, avg_lat, avg_long, latdd_end, latdd_start, londd_end, londd_start, bottom_fa_to_m, fishing_fa_to_m)

# Join fix_gsi and fix_obs by date first and then 
fix_dat_dep <- left_join(fix_gsi, fix_obs, by = c("date" = "date_haul_dep"))

# Build a function that will take the fish (samp_no) with columns from the gsi data,
# then subsequently filter with increasingly narrower requirements. After each filter, save
# the number

FIX_HAUL_JOIN <- function(samp_no, lat_gsi, lon_gsi, avg_lat, avg_long, fishing_depth_m, fishing_fa_to_m, bottom_depth_m, bottom_fa_to_m, haul_join){
  
  df<- data.frame(samp_no, lat_gsi, lon_gsi, avg_lat, avg_long, fishing_depth_m, fishing_fa_to_m, bottom_depth_m, bottom_fa_to_m, haul_join)
  
  OUT <-data.frame(samp_n = NA, haul_join = NA) # place to save the number of unique hauls per filter and IF only one, that haul_join value.
  OUT[1,1] <- length(df$samp_no)
  
  df1 <- df %>% 
    filter(abs(lat_gsi - avg_lat) < 1) %>% 
    filter(abs(lon_gsi - avg_long) < 1) %>% 
    filter(abs(fishing_depth_m - fishing_fa_to_m) < 1) %>% 
    filter(abs(bottom_depth_m - bottom_fa_to_m) < 1)
  
  OUT[2,1] <- length(df1$samp_no) # how many unique haul_join values are returned? store in OUT.
  OUT[2,2] <- ifelse(OUT[2,1] == 1, df1$haul_join, NA) # if there is only one unique haul_join, then return that value in OUT.
  
  df2 <- df1 %>% 
    filter(abs(lat_gsi - avg_lat) < 0.1) # narrower filter.
  
  OUT[3,1] <- length(df2$samp_no)
  OUT[3,2] <- ifelse(OUT[3,1] == 1, df2$haul_join, NA)
  
  df3 <- df2 %>% 
    filter(abs(lat_gsi - avg_lat) < 0.01) # narrower filter.
  
  OUT[4,1] <- length(df3$samp_no)
  OUT[4,2] <- ifelse(OUT[4,1] == 1, df3$haul_join, NA)
  
  df4 <- df3 %>% 
    filter(abs(lat_gsi - avg_lat) < 0.001) # narrower filter.
  
  OUT[5,1] <- length(df4$samp_no)
  OUT[5,2] <- ifelse(OUT[5,1] == 1, df4$haul_join, NA)
  
  df5 <- df4 %>% 
    filter(abs(lat_gsi - avg_lat) < 0.0001) # narrower filter.
  
  OUT[6,1] <- length(df5$samp_no)
  OUT[6,2] <- ifelse(OUT[7,1] == 1, df5$haul_join, NA)
  
  new_haul <- filter(OUT, !is.na(haul_join)) # filter OUT and drop any NAs (when either there are >1 unique haul_joins or 0 haul_joins)
  new_haul <- new_haul[1,2] # take the first instance of a unique haul_join and return that as the new_haul for that  fish.
  return(new_haul)
  
} # end function

# Apply the FIX_HAUL_JOIN function to the subset of fish_dat2(fix_dat) that isn't 
# matching to haul_joins in the haul_dat. Group fix_dat by samp_no, then return 
# with a new column that is the output from the FIX_HAUL_JOIN function (the new_haul value!)

fish_new_hauls <- fix_dat_dep %>%
  group_by(samp_no) %>%  
  summarise(new_haul = FIX_HAUL_JOIN(samp_no, lat_gsi, lon_gsi, avg_lat, avg_long, fishing_depth_m, fishing_fa_to_m, bottom_depth_m, bottom_fa_to_m, haul_join))
# Great! Now I have a tibble with the samp_no and the new_haul as found by my function.

count(fish_new_hauls, is.na(new_haul))
# But there are still 16 fish that return NAs for haul_join - this means they
# could never return a single unique haul through the various filters.

# Now repeat and but join by date and date_haul_ret to see if any of the missing 16 match using the different date.
fix_dat_ret <- left_join(fix_gsi, fix_obs, by = c("date" = "date_haul_ret"))

fish_new_hauls2 <- fix_dat_ret %>%
  group_by(samp_no) %>%  
  summarise(new_haul2 = FIX_HAUL_JOIN(samp_no, lat_gsi, lon_gsi, avg_lat, avg_long, fishing_depth_m, fishing_fa_to_m, bottom_depth_m, bottom_fa_to_m, haul_join)) %>% 
  left_join(fish_new_hauls)
# Great! Now I have a tibble with the samp_no and the new_haul (found via date_haul_dep)
# and newhaul2 (found via date_haul_ret) to compare.

# Look at all fish that need new hauls and see if we successfully found them in our two iterations above.
View(fish_new_hauls2 %>% 
       filter(is.na(new_haul) | is.na(new_haul2)))
# Awesome! We can match every fish with a new haul except three fish.

# Manually look at the remaining three fish where we couldn't match to a unique haul. Will have to drop these.
filter(fix_dat_dep, samp_no == "90601-506709") # fish could be either from haul_join HJ18683002943000000046 or HJ18683002943000000052: they have the same date, fishing depth, ocean depth, and lat/long!! But different deployment times and chinook_count.
filter(fix_dat_dep, samp_no == "90601-506710") # fish could be either from haul_join HJ18683002943000000046 or HJ18683002943000000052: they have the same date, fishing depth, ocean depth, and lat/long!! But different deployment times and chinook_count.
filter(fix_dat_dep, samp_no == "90517-508960") # no haul matches quite right.

# Make new column with all the new_haul final values.
fish_new_hauls2 <- fish_new_hauls2 %>% 
  mutate(new_haul_f = ifelse(is.na(new_haul), new_haul2, new_haul))

# Replace in join_dat the correct haul_join numbers for samp_no
sub_fish_dat2<- fish_dat2 %>% 
  filter(samp_no %in% fish_new_hauls2$samp_no) %>% 
  mutate(haul_join =  fish_new_hauls2$new_haul_f) %>% 
  dplyr::select(1:44) %>% 
  left_join(haul_dat, by = "haul_join")

sub_fish_dat2$date_gsi <- rep(NA, length(sub_fish_dat2$samp_no)) # add a column for date_gsi so that it will have the same columns as fish_dat2to rbind in the next step.

# Put the updated joined rows in sub_fish_dat2back with fish_dat2in place of the old samp_no values.
fish_dat3 <- rbind(sub_fish_dat2, filter(fish_dat2, !samp_no %in% fish_new_hauls2$samp_no))

sum(is.na(fish_dat3$haul_join)) # check to make sure there are still 3 NAs. Good.

# Drop 3 fish we can't accurately match to a haul in the observer dataset.
fish_dat3 <- fish_dat3 %>% 
  filter(!is.na(haul_join))


## 3.3 - Fix when number of gsi fish > chinook_count ----

# Now check to make sure we never have more gsi chinook in a haul than total chinook_count.

n_haul <- fish_dat3 %>% 
  group_by(haul_join) %>% 
  summarise(count=n()) %>% 
  rename(fish_count = count)

n_haul <- left_join(n_haul, dplyr::select(fish_dat3, haul_join, chinook_count))

n_haul <- n_haul %>% 
  mutate(fish_per_haul = fish_count / chinook_count)

fish_per_haul_errors <- filter(n_haul, fish_per_haul > 1) # one haul where it indicates that
fish_per_haul_errors  
# 2 salmon were genotyped but only 1 was caught in the haul from the observer data.

fish_dat3 %>% 
  filter(haul_join == "HJ14672003261000001072") %>% 
  dplyr::select(samp_no)
# The 2 salmon dropped are samp_no: 90517-507922 and 90517-507923.

# Drop 1 haul (HJ14672003261000001072) where this doesn't make sense.
fish_dat3 <- filter(fish_dat3, !haul_join %in% fish_per_haul_errors$haul_join)


# 4 - Save file: merged data by individual fish ----

# Save this full GSI dataset to fish_dat_all, then I can drop columns I'm not interested in for now.
saveRDS(fish_dat3, str_c(in_drive, "Saved Files/data_by_fish_v1.rds"))


# 5 - Summarize gsi data by haul and add all hauls to data set ----


#### Make second data set with combined gsi and obs data, but BY HAUL.
## 1. Calculate approximate estimates for esu (1) presence/absence, (2) proportion of haul, and (3) estimated catch BY HAUL.
## 2. Include all hauls where chinook catch is 0.


## 5.1 - Calculate tot_esu_n, pa_esu, p_esu, and catch_esu ----

# 5 - Summarize gsi data by haul and add all hauls to data set ----

haul_dat # Has all haul information including hauls with 0 chinook
fish_dat3 # Has gsi individuals with extra information from haul_dat and the original gsi data


# Calculate values I need by haul.

n_dat <- fish_dat3 %>% 
  group_by(haul_join) %>% 
  count() %>% 
  rename(tot_esu_n = n) # get total number of genotyped salmon per haul.

# Expand full dataset by haul_join and esu. Only for hauls where chinook were genotyped.
full_esu_dat <- fish_dat3 %>% dplyr::select(haul_join, esu) %>% ungroup() %>%
  mutate(esu = fct_expand(esu, "sac_wi")) %>% complete(haul_join, esu) %>%    # need to ungroup the df first for complete() to work! Otherwise, getting error: Error in `dplyr::summarise()`:! Problem while computing `..1 = complete(data = dplyr::cur_data(), ..., fill = fill, explicit = explicit)`.i The error occurred in group 1: haul_join = "HJ11889003703000000762", esu = so_bc. Caused by error ! object 'haul_join' not found
  left_join(n_dat)

# Calculate esu catch by haul using individual assignment: no threshold.
ia_esu_dat <- fish_dat3 %>% 
  group_by(haul_join, esu) %>% 
  count() %>%                      # get the number of salmon of diff ESUs per haul.    
  left_join(n_dat) %>% 
  mutate(p_esu = n / tot_esu_n) %>% ungroup() %>% # calculate proportion of diff ESUs per haul.
  left_join(dplyr::select(haul_dat, haul_join, chinook_count)) %>%
  mutate(catch_esu = p_esu * chinook_count,
         pa_esu = ifelse(p_esu > 0, 1, 0)) %>%  # extrapolate the proportion of ESUs to total ESU catch.
  dplyr::select(haul_join, esu, tot_esu_n, chinook_count, pa_esu, p_esu, catch_esu) %>% 
  ungroup() 

# Calculate esu catch by haul using individual assignment: using 0.8 threshold. IGNORE genotyped fish < 0.8 at the beginning!
n_dat_8 <- fish_dat3 %>% 
  filter(!prob_esu < 0.8) %>% 
  group_by(haul_join) %>% 
  count() %>% 
  rename(tot_esu_n_8 = n) 

ia_esu_dat_8 <- fish_dat3 %>%
  filter(!prob_esu < 0.8) %>% # ignore 943 genotyped fish with final esu assignment probabilities less than 0.8
  group_by(haul_join, esu) %>% 
  count() %>%                      # get the number of salmon of diff ESUs per haul.    
  left_join(n_dat_8) %>% 
  mutate(p_esu = n / tot_esu_n_8) %>% ungroup() %>% # calculate proportion of diff ESUs per haul.
  left_join(dplyr::select(haul_dat, haul_join, chinook_count)) %>%
  mutate(catch_esu_ia_8 = p_esu * chinook_count,
         pa_esu = ifelse(p_esu > 0, 1, 0)) %>%  # extrapolate the proportion of ESUs to total ESU catch.
  dplyr::select(haul_join, esu, tot_esu_n_8, pa_esu, catch_esu_ia_8) %>% 
  ungroup() 


# Calculate catch by esu not by individual assignment, but by summing composite proportions.
comp_prop_dat <- fish_dat3 %>% dplyr::select(samp_no, date, year, haul_join, sac_wi:c_bcak) %>% 
  group_by(haul_join) %>% 
  summarise_at(.vars=c("sac_wi", "cv_sp", "cv_fa", "ca_coast", "klam_trinity", "sor_nca", "or_coast", "wa_coast",
                       "l_col", "u_will", "m_col_sp", "u_col_sp", "des_sufa", "u_col_sufa", "sn_spsu", "sn_fa", "pug", "so_bc", "c_bcak"), sum) %>% 
  left_join(n_dat) %>% 
  mutate(across(c(2:20), .fns= ~./tot_esu_n)) %>%  # cool, divide multiple columns by the same column.
  left_join(haul_dat) %>% 
  pivot_longer(cols=c("sac_wi", "cv_sp", "cv_fa", "ca_coast", "klam_trinity", "sor_nca", "or_coast", "wa_coast",
                      "l_col", "u_will", "m_col_sp", "u_col_sp", "des_sufa", "u_col_sufa", "sn_spsu", "sn_fa", "pug", "so_bc", "c_bcak"),
               names_to = "esu", values_to = "comp_prop") %>% 
  mutate(catch_esu_cp = comp_prop * chinook_count) %>% 
  dplyr::select(haul_join, esu, comp_prop, catch_esu_cp) %>% ungroup()


# Add to complete dataset: this is complete with all the hauls where at least one fish was genotyped.
full_esu_dat <- full_esu_dat %>% left_join(ia_esu_dat) %>% left_join(comp_prop_dat) %>% left_join(ia_esu_dat_8) %>% 
  dplyr::select(-chinook_count) %>% left_join(dplyr::select(haul_dat, haul_join, chinook_count))
full_esu_dat

# Are there any hauls where catch_esu_cp is 0 but the catch_esu is a number? Should be NONE. Good.
filter(full_esu_dat, catch_esu_cp == 0 & catch_esu > 0)


# Now I need to combine with all the hauls in haul_dat including those with 0 chinook and those with chinook catch but none genotyped.

full_dat <- haul_dat %>% full_join(full_esu_dat) %>% complete(haul_join, esu)
# This works and expands the rows, but it enters NAs for all other columns in the new dataset.
# Plus, for hauls with no gsi fish, it makes an esu level "NA" and returns the haul data there.

# To replace the NAs with the right haul data, we select only the haul_join and esu columns,
# then rejoin the haul_dat columns, then rejoin the haul_chinook_dat columns,
# then ignore the rows where the esu level is NA (because now we have that haul data in the appro esu level columns).

full_dat <- full_dat %>% 
  dplyr::select(haul_join, esu) %>% 
  left_join(haul_dat) %>% 
  left_join(full_esu_dat) %>% 
  filter(!is.na(esu)) %>% 
  dplyr::select(-tot_esu_n_8) %>% # drop then re-join to get tot_esu_n_8 for all rows (removes many NAs).
  left_join(n_dat_8) %>% 
  mutate(pa_esu = ifelse(tot_esu_n == 0 & chinook_count > 0, NA, replace_na(pa_esu, 0)),
         p_esu = ifelse(tot_esu_n == 0 & chinook_count > 0, NA, replace_na(p_esu, 0)),
         catch_esu = ifelse(tot_esu_n == 0 & chinook_count > 0, NA, replace_na(catch_esu, 0)),
         catch_esu_ia_8 = ifelse(tot_esu_n_8 == 0 & chinook_count > 0, NA, replace_na(catch_esu_ia_8, 0)),
         comp_prop = ifelse(tot_esu_n == 0 & chinook_count > 0, NA, replace_na(comp_prop, 0)),
         catch_esu_cp = ifelse(tot_esu_n == 0 & chinook_count > 0, NA, replace_na(catch_esu_cp, 0))) %>% 
  mutate(tot_esu_n = replace_na(tot_esu_n, 0), tot_esu_n_8 = replace_na(tot_esu_n_8, 0)) %>% 
  distinct()

# Checks:
filter(full_dat, haul_join == "HJ17315003794000004200") # look at a haul where they caught 2 Chinook, genotyped 1 and was mostly so_bc...
filter(ia_esu_dat, haul_join == "HJ17315003794000004200")
filter(full_esu_dat, haul_join == "HJ17315003794000004200")

filter(full_dat, is.na(catch_esu)) # are there hauls where catch_esu (and other similar vars) is na, but there are still zeros for tot_esu_n vars?
filter(full_dat, is.na(catch_esu_ia_8) & !is.na(catch_esu)) # are there hauls where catch_esu_ia_8 is na but NOT catch_esu? Yes, good. These are hauls where none are genotyped based on the THRESHOLD.

filter(full_dat, haul_join == "HJ11889003703000000702") # haul where no chinook were caught.

# Check: Are there any hauls where chinook were caught but none were genotyped??
full_dat %>% 
  filter(chinook_count > 0 & tot_esu_n < 1) %>% 
  dplyr::select(haul_join) %>% distinct() %>% count()
# 6689 hauls where caught chinook but didn't genotype salmon.

View(filter(full_dat, haul_join == "HJ11889003703000001230")) # look one haul where chinook
# were caught but none genotyped and see if my function worked correctly and it makes sense.
# Perfect! My function worked correctly. tot_esu_n is 0 but pa_esu, p_esu, catch_esu are all NAs!!!


# Are there any hauls that don't have 19 rows (one for each esu)?
full_dat %>% 
  group_by(haul_join) %>% 
  count() %>% 
  filter(n != 19)
# No, all have 19 rows (good.)

# Are there any duplicate hauls?
full_dat %>% 
  group_by(haul_join) %>% 
  summarise(n_hauls = n_distinct(haul_join)) %>% 
  filter(n_hauls != 1)
# No, all have 1 (good.)

# full_dat should have the number of rows in haul_dat * 19 ESUs: 54510 * 19 = 1035690 (good!)

# 6 - Final cleaning & Save file: merged data by haul ----

# Save the full dataset with selected columns.

full_dat <- full_dat %>% 
  dplyr::select(haul_join, esu, chinook_count, tot_esu_n, tot_esu_n_8, pa_esu, catch_esu, catch_esu_cp, catch_esu_ia_8, retained_hake_mt,
                duration_in_min, latdd_start, londd_end, deployment_date, date_haul_dep, year_obs, month, doy, time_num, time_hms,
                bottom_fa_to_m, fishing_fa_to_m, drvid) %>% 
  rename(lat = latdd_start, lon = londd_end, datetime = deployment_date, date = date_haul_dep, year = year_obs,
         bottom_m = bottom_fa_to_m, fishing_m = fishing_fa_to_m, duration = duration_in_min,
         hake_mt = retained_hake_mt)
full_dat

saveRDS(full_dat, str_c(in_drive, "Saved Files/data_by_haul_and_esu_v1.rds"))


