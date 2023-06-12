
# Scenario Analysis

# Outline of script ----
# 1 - Set settings
# 2 - Import & clean data
# 3 - Scenarios


# 1 - Set settings ----

# Load libraries
library(tidyverse); library(ggpubr)

# set settings for print (to be like head()) 
#options(tibble.print_max = 5)
options(tibble.width = Inf)

# what to do with NAs
options(na.action = na.fail)

options(scipen = 9999)


# 2 - Import & Clean data ----

# Location where my files are stored
in_drive <- "C:/Users/sabalm/Documents/Chinook-Hake-Bycatch-Data/Data/"

# Load full dataset by haul + ESU including covariates after cleaning.
full_dat <- readRDS(str_c(in_drive, "Saved Files/data_by_haul_and_esu_v2.rds")) # N = 54509

# Get data set with only haul data (drop rows with ESU data)
haul_dat <- full_dat %>% 
  ungroup() %>% 
  dplyr::select(-esu, -pa_esu, -catch_esu, -catch_esu_cp, -catch_esu_ia_8) %>%
  dplyr::select(haul_join, chinook_count, lon, lat, year, doy, time_num, fishing_m, bottom_m, duration, sst_mean) %>% 
  mutate(pa = ifelse(chinook_count == 0, 0, 1),
         bpue = chinook_count / duration * 60) %>%  # Chinook bycatch-per-unit-effort (BPUE) is number of Chinook caught per hour.
  distinct() # N = 54509

# Get the day-night column from full_dat
day_night_dat <- full_dat %>% dplyr::select(haul_join, day_night) %>% distinct()



# 3 - Scenarios ----

# Set thresholds for warm and cool SSTs (chose 14 because first temperature in southern stock thermal refugia models
  # where we started to see a signal of that behavoir and also this keeps a reasonable sample size in each bin)
cool_sst <- 14
warm_sst <- 14

# Make dataset for scenarios: observed Chinook BPUE by depth, sst bin, and lat bin.
ref_dvm_dat <- haul_dat %>%
  left_join(day_night_dat) %>%
  filter(fishing_m < 600) %>%
  mutate(depth_bin = cut(fishing_m, breaks=seq(0,600,by=100)),
         lat_bin = ifelse(lat > 45.77, ">45 lat", "<45 lat"),
         bpue = chinook_count / duration *60,
         sst_bin = ifelse(sst_mean > warm_sst, "warm",               
                          ifelse(sst_mean < cool_sst, "cool", "other"))) %>%
  filter(sst_bin != "other") %>% 
  group_by(depth_bin, day_night, sst_bin, lat_bin) %>%
  summarise(mean_chinook = mean(chinook_count),  
            mean_bpue = mean(bpue),             
            sd_chinook = sd(chinook_count),
            sd_bpue = sd(bpue),
            count = length(chinook_count)) %>% #sum_hrs = sum(duration)
  mutate(se = sd_chinook / sqrt(count),
         se_bpue = sd_bpue / sqrt(count),
         day_night = ifelse(day_night == "day", "Day", "Night"))
ref_dvm_dat

####

# Make another dataset with the different effort categories by scenarios (even-restrictions by shift vs. reduce)

# Observed depth-distribution of fishing effort by SST bin
eff_dat <- haul_dat %>% filter(fishing_m < 600) %>%
  mutate(depth_bin = cut(fishing_m, breaks=seq(0,600,by=100)),
         lat_bin = ifelse(lat > 45.77, ">45 lat", "<45 lat"),
         bpue = chinook_count / duration *60,
         sst_bin = ifelse(sst_mean > warm_sst, "warm",
                          ifelse(sst_mean < cool_sst, "cool", "other"))) %>%
  filter(sst_bin != "other") %>% 
  group_by(depth_bin, sst_bin, lat_bin) %>%
  summarise(count_hauls = length(bpue)) # get observed duration (effort)

eff_dat <-  haul_dat %>% filter(fishing_m < 600) %>%
  mutate(depth_bin = cut(fishing_m, breaks=seq(0,600,by=100)),
         lat_bin = ifelse(lat > 45.77, ">45 lat", "<45 lat"),
         bpue = chinook_count / duration *60,
         sst_bin = ifelse(sst_mean > warm_sst, "warm",
                          ifelse(sst_mean < cool_sst, "cool", "other"))) %>%
  filter(sst_bin != "other") %>% 
  group_by(sst_bin, lat_bin) %>%
  summarise(tot_hauls = length(duration)) %>% # get total number of hauls by sst and lat bins
  right_join(eff_dat) %>% 
  mutate(per_effort = count_hauls / tot_hauls * 100) %>% # calculate the percent of observed hauls by cat.
  select(sst_bin, lat_bin, depth_bin, per_effort) %>% 
  mutate(depth_dist_cat = "sst")

#####

# Effort by # of hauls (same as Fig 1d)

# Fig 1d: Fishing effort by fishing depth
depth_dat <- haul_dat %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100))) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue), 
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100))
depth_dat

# make into percent distribution
eff_dat2 <- depth_dat %>% 
  mutate(per_effort = (count_hauls / 54509) * 100) %>% 
  dplyr::select(fishing_bins, per_effort) %>% 
  rename(depth_bin = fishing_bins) %>%  # new_name = old_name
  mutate(depth_dist_cat = "average")


eff_dat2a <- eff_dat2 %>% mutate(lat_bin = "<45 lat", sst_bin = "cool")
eff_dat2b <- eff_dat2 %>% mutate(lat_bin = "<45 lat", sst_bin = "warm")
eff_dat2c <- eff_dat2 %>% mutate(lat_bin = ">45 lat", sst_bin = "cool")
eff_dat2d <- eff_dat2 %>% mutate(lat_bin = ">45 lat", sst_bin = "warm")

eff_dat_tot <- eff_dat2a %>% bind_rows(eff_dat2b) %>% bind_rows(eff_dat2c) %>% bind_rows(eff_dat2d) %>% bind_rows(eff_dat)


###

# Combine effort and observed DVM by refugia datasets
ref_dvm_dat <- ref_dvm_dat %>% left_join(dplyr::select(eff_dat_tot, sst_bin, lat_bin,  depth_dist_cat, depth_bin, per_effort))

ref_dvm_dat2 <- ref_dvm_dat %>% mutate(reduce_shift_cat = "reduce")

ref_dvm_dat <- ref_dvm_dat %>% mutate(reduce_shift_cat = "shift") %>% bind_rows(ref_dvm_dat2)



# Define variables for number of hours to extrapolate over.
tot_hrs <- 1000


scenario_dat_even <- ref_dvm_dat %>% mutate(dn_hrs = tot_hrs*0.5) %>% 
  mutate(scenario = "Even")

scenario_dat_rest <- ref_dvm_dat %>% mutate(dn_hrs = ifelse(reduce_shift_cat == "shift" & day_night == "Night", tot_hrs*0.25,
                                                            ifelse(reduce_shift_cat == "shift" & day_night == "Day", tot_hrs*0.75,
                                                                   ifelse(reduce_shift_cat == "reduce" & day_night == "Night", tot_hrs*0.125, tot_hrs*0.5 )))) %>% 
  mutate(scenario = "Restrictions")

scenario_dat <- scenario_dat_even %>% bind_rows(scenario_dat_rest) %>%  # Combine two scenarios
  mutate(est_hrs = dn_hrs * (per_effort/100)) %>%  # get estimated hours of towing per depth bin.
  mutate(est_bycatch = mean_bpue * est_hrs) # get estimated bycatch per depth bin.
  
# scenario_dat_wide <- scenario_dat %>% dplyr::select(depth_bin, day_night, sst_bin, lat_bin, reduce_shift_cat, depth_dist_cat, scenario, est_bycatch) %>% 
#   pivot_wider(names_from = scenario, values_from=est_bycatch)


# Summary of net reduction in bycatch by using night fishing restrictions
scenario_dat %>% group_by(sst_bin, lat_bin, reduce_shift_cat, depth_dist_cat, scenario) %>% summarise(est_bycatch = sum(est_bycatch)) %>%  # This is amazing!!!
  pivot_wider(names_from = scenario, values_from = est_bycatch) %>% 
  mutate(p_night_effectivness = (Restrictions - Even)/Even*100) %>% 
  filter(lat_bin == "<45 lat")
# In south...
  # Cool: 19.8% hypothetical reduction in bycatch by using night fishing restrictions.
  # Warm: 1.2% hypothetical increase in bycatch by using night fishing restrictions!



# Figures

# Make function for observed DVM plots with SE bars.
obs_dvm_plot_fun <- function(data){
  ggplot(data=data, aes(x=as.factor(depth_bin), y=mean_bpue, fill=day_night)) +
    geom_bar(stat="identity", color="black", alpha=0.7, position=position_dodge()) +
    ylab("Observed Chinook salmon\nbycatch per hour") + theme_classic() +
    ylim(c(0,4)) +
    geom_errorbar(aes(ymin=mean_bpue-se_bpue, ymax=mean_bpue+se_bpue), color="black", width=0, position=position_dodge(.9)) +
    scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
    scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
    xlab("Fishing depth bin (m)") +
    theme(strip.text = element_text(face = "bold", size=14)) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "top", legend.title = element_blank()) +
    scale_x_discrete(limits = c("(500,600]",
                                "(400,500]", "(300,400]", "(200,300]",
                                "(100,200]", "(0,100]")) +
    coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
} # end function

fig_4_bpue_cool <- obs_dvm_plot_fun(data=filter(ref_dvm_dat, lat_bin == "<45 lat" & sst_bin == "cool")); fig_4_bpue_cool
fig_4_bpue_warm <- obs_dvm_plot_fun(data=filter(ref_dvm_dat, lat_bin == "<45 lat" & sst_bin == "warm")); fig_4_bpue_warm


data <- filter(ref_dvm_dat, lat_bin == "<45 lat" & sst_bin == "cool")

### Average fishing effort by depth (%)
fig_4_eff <- ggplot(data=eff_dat2, # filter(eff_dat, sst_bin == "cool" & lat_bin == "<45 lat")
                         aes(x=depth_bin, y=per_effort)) + 
  geom_bar(stat="identity", color="black", alpha=0.7, position=position_dodge(0.9, preserve='single')) +
  ylab("Observed % of\nhauls") + theme_classic() +
  ylim(c(0,50)) +
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
  xlab("Fishing depth bin (m)") +
  theme(strip.text = element_text(face = "bold", size=14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()) +
  scale_x_discrete(limits = c("(500,600]",
                              "(400,500]", "(300,400]", "(200,300]",
                              "(100,200]", "(0,100]")) +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"); fig_4_eff



### Total catch by depth

scenario_dat2 <- scenario_dat %>% pivot_wider(id_cols = c(depth_bin, day_night, sst_bin, lat_bin, reduce_shift_cat, depth_dist_cat),
                                              values_from = est_bycatch, names_from = scenario)


dat_cool <- filter(scenario_dat2, lat_bin == "<45 lat" & sst_bin == "cool" & reduce_shift_cat == "shift" & depth_dist_cat == "average")

fig_4b_cool <- ggplot() +
  geom_segment(data=subset(dat_cool, day_night == "Night"),
               aes(x = depth_bin, y = Even, xend = depth_bin, yend = Restrictions),
               position=position_nudge(0.2),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), linewidth = 1, color="black") +
  
  geom_segment(data=subset(dat_cool, day_night == "Day"),
               aes(x = as.factor(depth_bin), y = Even, xend = as.factor(depth_bin), yend = Restrictions),
               position=position_nudge(-0.2),
               arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed"), linewidth = 1, color="gold") +
  geom_bar(data=dat_cool, 
           aes(x=as.factor(depth_bin), y=Even, fill=day_night),
           stat="identity", color="black", alpha=0.7, position=position_dodge(0.9, preserve='single')) +
  ylab("Estimated Chinook salmon\nbycatch over 1000 hours" ) + theme_classic() +
  ylim(c(0,600)) +
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
  xlab("Fishing depth bin (m)") +
  theme(strip.text = element_text(face = "bold", size=14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()) +
  scale_x_discrete(limits = c("(500,600]",
                              "(400,500]", "(300,400]", "(200,300]",
                              "(100,200]", "(0,100]")) +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"); fig_4b_cool

###
dat_warm <- filter(scenario_dat2, lat_bin == "<45 lat" & sst_bin == "warm" & reduce_shift_cat == "shift" & depth_dist_cat == "average")

fig_4b_warm <- ggplot() +
  geom_segment(data=subset(dat_warm, day_night == "Night"),
               aes(x = depth_bin, y = Even, xend = depth_bin, yend = Restrictions),
               position=position_nudge(0.2),
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), linewidth = 1, color="black") +
  
  geom_segment(data=subset(dat_warm, day_night == "Day"),
               aes(x = as.factor(depth_bin), y = Even, xend = as.factor(depth_bin), yend = Restrictions),
               position=position_nudge(-0.2),
               arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed"), linewidth = 1, color="gold") +
  geom_bar(data=dat_warm, 
           aes(x=as.factor(depth_bin), y=Even, fill=day_night),
           stat="identity", color="black", alpha=0.7, position=position_dodge(0.9, preserve='single')) +
  ylab("Estimated Chinook salmon\nbycatch over 1000 hours" ) + theme_classic() +
  ylim(c(0,600)) +
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
  xlab("Fishing depth bin (m)") +
  theme(strip.text = element_text(face = "bold", size=14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()) +
  scale_x_discrete(limits = c("(500,600]",
                              "(400,500]", "(300,400]", "(200,300]",
                              "(100,200]", "(0,100]")) +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"); fig_4b_warm


### Final Plots
setwd("C:/Users/sabalm/Desktop/")
pdf("fig_4_v2.pdf", width=8, height=8, onefile=FALSE)

ggarrange(fig_4_bpue_cool, fig_4b_cool, fig_4_bpue_warm, fig_4b_warm, ncol=2, nrow=2, common.legend = TRUE, legend = "top")

dev.off()


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_4_mini.pdf", width=4, height=4, onefile=FALSE)
fig_4_eff_cool 
dev.off()