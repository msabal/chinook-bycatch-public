# Making Figures for PNAS

library(scales); library(raster); library(ggrepel); library(ggridges); library(ggpubr); library(sf)

# Run script 4 (4_run_models.R) FIRST.

#Check these are loaded
full_dat
haul_dat

full_mod
esu_dat

# Load natural earth file
states2 <- readRDS(str_c(in_drive, "Saved GIS Files/naturalearth_states.rds"))

# Lon x Lat
gb_r <- raster(x = str_c(in_drive, "Raw GIS Files/gebco_2021_n50.1_s39.9_w-128.1_e-122.9.tif"))
gb_cont_200 <- rasterToContour(gb_r, levels = -200) # Get contour lines
gb_cont_200 <- st_as_sf(gb_cont_200) # Covert to sf objects


# Summary stats in manuscript ----

# Total Chinook caught 2002 - 2021
sum(haul_dat$chinook_count) # 67165

# With average per year
haul_dat %>% group_by(year) %>% summarise(chinook_yr = sum(chinook_count)) %>% ungroup() %>% summarise(mean(chinook_yr),
                                                                                                       sd(chinook_yr))
# Bycatch occurred in N = 8534 hauls out of N  = 54509 total hauls
haul_dat %>% filter(chinook_count > 0)

# 86% of hauls caught fewer than 10 chinook per hour
haul_dat %>% filter(bpue > 0 & bpue < 10) %>% count() # 7372 / 8534 * 100 = 86%

# % of hauls caught over 200 chinook per hour
haul_dat %>% filter(bpue > 200) %>% count() # 6 hauls

# 75% hauls occurred at night, 25% during day
full_dat %>% dplyr::select(haul_join, day_night) %>%  distinct() %>% group_by(day_night) %>% count()

# SST: 
haul_dat %>% summarise(range(sst_mean), mean(sst_mean), sd(sst_mean))

# ESU sample sizes of positive catches: unique hauls
full_dat %>% filter(esu == "klam_trinity" & catch_esu_ia_8 > 0) %>% count() #564
full_dat %>% filter(esu == "sor_nca" & catch_esu_ia_8 > 0) %>% count() #488
full_dat %>% filter(esu == "or_coast" & catch_esu_ia_8 > 0) %>% count() #206
full_dat %>% filter(esu == "pug" & catch_esu_ia_8 > 0) %>% count() #184
full_dat %>% filter(esu == "so_bc" & catch_esu_ia_8 > 0) %>% count() #334

# ESU sample sizes of individual fish genotyped
fish_dat <- readRDS(str_c(in_drive, "Saved Files/data_by_fish_v1.rds"))

fish_dat %>% filter(esu %in% c("klam_trinity", "sor_nca", "or_coast", "pug", "so_bc") & prob_esu > 0.8) %>% 
  group_by(esu) %>% count() # kt: 1052, sor_nca: 722, or_coast: 223, pug: 273, so_bc: 459


# Figure 1: Summary of bycatch and fishing effort ----
## This is a multi-panel Figure, which I assemble in Inkscape. Individual plots made below:

# First calculate observed Chinook bycatch per vessel per hour.
haul_dat <-  haul_dat %>% mutate(bpue = (chinook_count / duration) *60)


# Make dataset summarizing data by lat/lon grid cells.

haul_dat_more_covars <- readRDS(str_c(in_drive, "Saved Files/data_by_haul_v1.rds"))

# Make a function to cut a numeric varible to intervals, but make the return category value be the MIDPOINT!!! (https://stackoverflow.com/questions/5915916/divide-a-range-of-values-in-bins-of-equal-length-cut-vs-cut2)
cut2 <- function(x, breaks) {
  r <- range(x)
  b <- seq(r[1], r[2], length=2*breaks+1)
  brk <- b[0:breaks*2+1]
  mid <- b[1:breaks*2]
  brk[1] <- brk[1]-0.01
  k <- cut(x, breaks=brk, labels=FALSE)
  mid[k]
}

# Summarize fishing effort and chinook catch (BPUE) by lat and lon grid cells.
map_dat <- haul_dat %>% left_join(dplyr::select(haul_dat_more_covars, haul_join, drvid)) %>% 
  mutate(lat = cut2(lat, breaks=40), lon = cut2(lon, breaks=10)) %>% 
  group_by(lat, lon) %>% summarise(n_hauls = n(),
                                   bpue = mean(bpue),
                                   n_vessels = n_distinct(drvid)) %>% 
  filter(n_vessels > 3) # drop cells where less than 3 vessels fished (to keep fishing location data v. anonymous)


# Figures 1a, 1b, 1c: Map of Fishing effort, observed Chinook bycatch per hour, Focal ESU latitude distributions.

# Fishing effort (number of hauls): map
fig_1a <- ggplot() + theme_classic() +
  geom_tile(data=map_dat, aes(lon, lat, fill = n_hauls), stat = "identity", alpha=0.5) +
  geom_sf(data=states2, fill = "gray95") +
  geom_sf(data=gb_cont_200, size=0.8) +
  #scale_fill_gradient(low="gray80", high="firebrick3", name="Number\nof hauls") +
  scale_fill_viridis(name = "Number\nof hauls") +
  theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = c(0.2,0.5), legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5)),
        legend.title = element_text(size=8)) +
  coord_sf(xlim = c(-127,-123.5),
           ylim = c(41.4,49)); fig_1a


# Observed Chinook bycatch per vessel per hour: map
fig_1b <- ggplot() + theme_classic() +
  geom_tile(data=map_dat, aes(lon, lat, fill = log(bpue)), stat = "identity", alpha=0.5) +
  geom_sf(data=states2, fill = "gray95") +
  geom_sf(data=gb_cont_200, size=0.8) +
  scale_fill_viridis(name="log(bycatch\nrate)") +
  #scale_fill_gradient(low="royalblue", high="firebrick3", name="log(BPUE)") +
  theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = c(0.25,0.5), legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5)),
        legend.title = element_text(size=8)) +
  coord_sf(xlim = c(-127,-123.5),
           ylim = c(41.4,49)); fig_1b



# Focal ESU distributions: violin
esu_lat <- full_dat %>% dplyr::select(haul_join, esu, catch_esu_ia_8, lat, lon) %>%   # Using 0.8 threshold right now! But maybe change later to composite proportion if figure out the SE.
  filter(catch_esu_ia_8 > 0 & 
           esu %in% c("klam_trinity", "sor_nca", "or_coast", "pug", "so_bc"))

# Change ESU level names and order
esu_lat$esu <- as.factor(esu_lat$esu)
levels(esu_lat$esu) <- c("Klamath - Trinity", "OR Coast", "Puget Sound", "S. BC", "S. OR - N. CA")
esu_lat <- esu_lat %>% mutate(esu = relevel(esu, "Klamath - Trinity", "S. OR - N. CA", "OR Coast", "Puget Sound", "S. BC"))
esu_lat <- esu_lat %>% mutate(esu = fct_relevel(esu, "Klamath - Trinity", "S. OR - N. CA", "OR Coast", "Puget Sound", "S. BC"))
levels(esu_lat$esu)

# Violin
fig_1c <- ggplot(data=esu_lat, aes(x=esu, y=lat, fill=esu)) + geom_violin(alpha=0.5) + stat_summary(fun=mean, geom="point", shape=23, size=4) +
  theme_classic() + ylab("Latitude") + theme(legend.title = element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x = element_blank(), legend.position = "bottom") + 
  scale_y_continuous(limits = c(41.4, 49), breaks = c(42,43,44,45,46,47,48)) +
  theme(plot.title = element_text(size = 16, face = "bold")); fig_1c


# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_1_abc.pdf", width=16, height=8, onefile=FALSE)

ggarrange(fig_1a, fig_1b, fig_1c, ncol=3)

dev.off()


# Figures 1d, 1e, 1f: Chinook bycatch histogram, Chinook bycatch by fishing depths, Fishing effort by time of day.

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


fig_1d <- ggplot(data=depth_dat, aes(x=fishing_m, y=count_hauls)) + 
  geom_bar(stat="identity", color="black", fill="gray30") + theme_classic() +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 21000), expand = c(0,0)) +
  ylab("Number of hauls\n ") + xlab("Fishing depth (m)"); fig_1d

# Fig 1e: Chinook bycatch by fishing depth

fig_1e <- ggplot(data=depth_dat, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="gray88") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 3.5), expand = c(0,0)) +
  ylab("Observed Chinook salmon\nbycatch per hour") + xlab("Fishing depth (m)");  fig_1e


# Fig 1f: Circular histogram: time of day
fig_1f <- ggplot(data=haul_dat, aes(x=time_num/3600)) + geom_histogram(color="black", fill="lightblue", alpha=0.5) +
  coord_polar("x") + theme_bw() + scale_x_continuous(breaks=seq(0,24, by=1), name ="Time of Day") + 
  ylab("Number of hauls") + theme(panel.grid.major=element_line(color="gray50")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()); fig_1f


# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_1_def.pdf", width=8, height=4, onefile=FALSE)

ggarrange(fig_1d, fig_1e, fig_1f, ncol=3)

dev.off()


# Figure 2: Thermal refugia ----

ref_plot_fun <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16, 18),
                                duration = 60,
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit - se), ymax=exp(fit + se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    scale_x_continuous(breaks=c(seq(0,400,by=50))) +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook salmon\nbycatch per hour") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C)))
} # end function.

ref_plot_fun_16 <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16),
                                duration = 60,
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit-se), ymax=exp(fit+se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    scale_x_continuous(breaks=c(seq(0,400,by=50))) +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook salmon\nbycatch per hour") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C)))
} # end function.


fig_2a <- ref_plot_fun(data=haul_dat, response="chinook_count", model=full_mod, title="(a) All Chinook salmon"); fig_2a

# By ESU: south to north
fig_2b <- ref_plot_fun(data=esu_dat$data[[1]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[1]], title="(b) Klamath - Trinity") + coord_cartesian(ylim=c(0,0.02)); fig_2b
fig_2c <- ref_plot_fun(data=esu_dat$data[[5]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[5]], title="(c) S. OR - N. CA") + coord_cartesian(ylim=c(0,0.07)); fig_2c
fig_2d <- ref_plot_fun(data=esu_dat$data[[2]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[2]], title="(d) OR Coast") + coord_cartesian(ylim=c(0,0.17)); fig_2d
fig_2e <- ref_plot_fun_16(data=esu_dat$data[[3]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[3]], title="(e) Puget Sound") + coord_cartesian(ylim=c(0,0.12)); fig_2e
fig_2f <- ref_plot_fun_16(data=esu_dat$data[[4]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[4]], title="(f) S. BC") + coord_cartesian(ylim=c(0,0.008)); fig_2f


# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_2.pdf", width=10, height=6, onefile=FALSE)

ggarrange(fig_2a, fig_2b, fig_2c, fig_2d, fig_2e, fig_2f, ncol=3, nrow=2, common.legend = TRUE, legend="top")

dev.off()




# Figure 3: DVM ----

# Make function for dvm plots with SE intervals.
dvm_plot_fun <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(time_num = seq_range(time_num, 50, pretty=T),
                                fishing_m = c(50, 100, 200, 300, 400),
                                duration = 60,
                                lat = av_lat,
                                .model = model)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(time_num/3600), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit-se), ymax=exp(fit+se), fill=as.factor(fishing_m)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(fishing_m))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook salmon\nbycatch per hour") + xlab(label="Time (hours since midnight)") +
    scale_fill_viridis(discrete = TRUE, direction=-1, name="Fishing depth (m)") + scale_color_viridis(discrete=TRUE, direction=-1, name="Fishing depth (m)")
} # end function


fig_3a <- dvm_plot_fun(data=haul_dat, response="chinook_count", model=full_mod, title="(a) All Chinook salmon"); fig_3a

# By ESU: south to north
fig_3b <- dvm_plot_fun(data=esu_dat$data[[1]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[1]], title="(b) Klamath - Trinity"); fig_3b
fig_3c <- dvm_plot_fun(data=esu_dat$data[[5]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[5]], title="(c) S. OR - N. CA"); fig_3c
fig_3d <- dvm_plot_fun(data=esu_dat$data[[2]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[2]], title="(d) OR Coast"); fig_3d
fig_3e <- dvm_plot_fun(data=esu_dat$data[[3]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[3]], title="(e) Puget Sound"); fig_3e
fig_3f <- dvm_plot_fun(data=esu_dat$data[[4]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[4]], title="(f) S. BC"); fig_3f

# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("fig_3.pdf", width=10, height=6, onefile=FALSE) # for ncol=4, width=12, height=6

ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, fig_3f, ncol=3, nrow=2, common.legend = TRUE, legend="top")

dev.off()


# Day: 1.3 Chinook per hour is highest BPUE in Fig 2a: 50 fishing depths at 12:30.
newdata_dvm1 <- haul_dat %>% data_grid(time_num = 12.5*60*60,
                                       fishing_m = 50,
                                       duration = rep(60, length.out=20),
                                       .model = full_mod)
exp(predict(full_mod, newdata_dvm1)) #1.3 bpue

# Day: concurrent at 200 m depth: 
newdata_dvm2 <- haul_dat %>% data_grid(time_num = 12.5*60*60,
                                       fishing_m = 200,
                                       duration = rep(60, length.out=20),
                                       .model = full_mod)
exp(predict(full_mod, newdata_dvm2)) # 0.136 bpue; 1.3 / 0.136 = 9.6 times lower than concurrent bpue at 50m.

# Night: concurrent at 200 m depth: 
newdata_dvm3 <- haul_dat %>% data_grid(time_num = 0,
                                       fishing_m = 200,
                                       duration = rep(60, length.out=20),
                                       .model = full_mod)
exp(predict(full_mod, newdata_dvm3)) # 0.45 bpue; 0.45 / 0.136 = 3.3 times higher than same depth (200 m) between day and night.





# When SST near 18 degrees, almost 0 bpue in surface waters (50 m)
newdata_ref1 <- haul_dat %>% data_grid(sst_mean = 18,
                                  fishing_m = 50,
                                  duration = rep(60, length.out=20),
                                  .model = full_mod)
exp(predict(full_mod, newdata_ref1)) #0.05 bpue

# When SST near 18 degrees, 0.63 bpue at 250 m
newdata_ref2 <- haul_dat %>% data_grid(sst_mean = 18,
                                       fishing_m = 250,
                                       duration = rep(60, length.out=20),
                                       .model = full_mod)
exp(predict(full_mod, newdata_ref2)) # 0.63 bpue; 1.3 / 0.136 = 9.6 times lower than concurrent bpue at 50m.



# Figure 5: Annual bycatch by annual SST ----

# Get annual SST values from haul locations.
annual_dat <- haul_dat %>% group_by(year) %>% 
  summarise(mean_sst = mean(sst_mean),
            count_sst = length(sst_mean),
            sd_sst = sd(sst_mean)) %>% 
  mutate(se_sst = sd_sst / sqrt(count_sst))

# Get raw total bycatch #s
annual_dat <- full_dat %>% dplyr::select(haul_join, chinook_count, year, duration) %>%  distinct() %>% 
  group_by(year) %>% summarize(total_bycatch = sum(chinook_count),
                               total_duration = sum(duration)) %>% 
  left_join(annual_dat)


# Get model predictions of bycatch per effort for year term in the model.
newdata <- haul_dat %>% data_grid(year = seq(2002,2021, by=1),
                                 duration = rep(60, length.out=20),
                                 .model = full_mod)
pred_out <- predict(full_mod, newdata, se.fit=T)
pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
annual_dat <- left_join(pred_t, annual_dat) # Join predicted annual bycatch estimates with mean SST values.


# Figure 4
fig_5 <- ggplot(data=annual_dat, aes(x=mean_sst, y=exp(fit), fill=mean_sst)) + 
  stat_smooth(method = "lm", fill= "gray88", color = "gray75", alpha=0) +
  geom_point(shape=21, size=3) +
  theme_classic() + theme(legend.position = c(0.3,0.8), legend.direction="horizontal", 
                          legend.background=element_rect(fill=alpha("white", 0)),
                          legend.key=element_rect(fill=alpha("white", 0.5))) +
  scale_fill_gradient(low="royalblue", high="red", name=expression("SST " ( degree*C))) +
  ylab("Predicted Chinook salmon\nbycatch per hour") + xlab("Mean annual SST") +
  ylim(c(0,0.2)) +
  geom_label_repel(aes(label = year), size=3, box.padding = 0.2, point.padding = 0.1, 
                   segment.color = 'grey50', fill="transparent", label.size = NA); fig_5

summary(lm(exp(fit) ~ mean_sst, data=annual_dat)) # summary of linear relationship in plot.

# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_5.pdf", width=5, height=4, onefile=FALSE)

fig_5

dev.off()

# Text summary:
# 2008 bycatch per hour value
v08 <- annual_dat %>% filter(year == 2008) %>% dplyr::select(fit) %>% pull()
exp(v08) * 7170  # bycatch per hour * annual average hours of towing = 51 salmon

# 2014 bycatch per hour value
v14 <- annual_dat %>% filter(year == 2014) %>% dplyr::select(fit) %>% pull()
exp(v14) * 7170 # bycatch per hour * annual average hours of towing = 1352 salmon









# Figure S1: ESU: bpue by fishing depth ----

# Fig S1a: Klamath - Trinity
depth_kt <- esu_dat$data[[1]] %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = catch_esu_ia_8 / duration * 60) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100)) %>% 
  drop_na()
depth_kt

levels(depth_kt$fishing_bins) <- c("50", "150", "250", "350", "450", "550")

fig_s1a <- ggplot(data=depth_kt, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="#FBB9B5") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 0.65), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)") +
  ggtitle(label = "(a) Klamath - Trinity") + theme(plot.title = element_text(hjust = 0.5)); fig_s1a

# Fig S1b: S. OR - N. CA
depth_sor <- esu_dat$data[[5]] %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = catch_esu_ia_8 / duration * 60) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100)) %>% 
  drop_na()
depth_sor

levels(depth_sor$fishing_bins) <- c("50", "150", "250", "350", "450", "550")

fig_s1b <- ggplot(data=depth_sor, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="#CDCE7C") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 0.65), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)") +
  ggtitle(label = "(b) S. OR - N. CA") + theme(plot.title = element_text(hjust = 0.5)); fig_s1b

# Fig S1c: OR Coast
depth_orc <- esu_dat$data[[2]] %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = catch_esu_ia_8 / duration * 60) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100)) %>% 
  drop_na()
depth_orc

levels(depth_orc$fishing_bins) <- c("50", "150", "250", "350", "450", "550")

fig_s1c <- ggplot(data=depth_orc, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="#7EDFBD") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 0.4), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)") +
  ggtitle(label = "(c) OR Coast") + theme(plot.title = element_text(hjust = 0.5)); fig_s1c


# Fig S1d: Puget Sound
depth_pug <- esu_dat$data[[3]] %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = catch_esu_ia_8 / duration * 60) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100)) %>% 
  drop_na()
depth_pug

levels(depth_pug$fishing_bins) <- c("50", "150", "250", "350", "450", "550")

fig_s1d <- ggplot(data=depth_pug, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="#7ED7F9") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 0.22), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)") +
  ggtitle(label = "(d) Puget Sound") + theme(plot.title = element_text(hjust = 0.5)); fig_s1d


# Fig S1e: S. BC
depth_sbc <- esu_dat$data[[4]] %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = catch_esu_ia_8 / duration * 60) %>% 
  group_by(fishing_bins) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(fishing_m = seq(50,650,by=100)) %>% 
  drop_na()
depth_sbc

levels(depth_sbc$fishing_bins) <- c("50", "150", "250", "350", "450", "550")

fig_s1e <- ggplot(data=depth_sbc, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="#F1B3F7") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 0.6), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)") +
  ggtitle(label = "(e) S. BC") + theme(plot.title = element_text(hjust = 0.5)); fig_s1e


setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_S1.pdf", width=10, height=4, onefile=FALSE)

ggarrange(fig_s1a, fig_s1b, fig_s1c, fig_s1d, fig_s1e, ncol=5, nrow=1)

dev.off()



# Figure S2: SST density distributions by lat ----

ridge_dat <- haul_dat %>% mutate(lat_bin = cut(lat, seq(41,50, by=1)))


fig_s2 <- ggplot(data=ridge_dat, aes(x=sst_mean, y=lat_bin, fill=stat(x))) +
  geom_density_ridges_gradient(scale=1.8) + theme_classic() +
  scale_fill_gradient(high="red", low="royalblue", name=expression("SST " ( degree*C))) +
  geom_vline(xintercept=c(10,12,14,16,18), color="gray", size=0.7, linetype="dashed") +
  ylab("Latitude bin") + scale_x_continuous(breaks=seq(4,20,by=1), name=expression("SST " ( degree*C))) +
  theme(legend.position = "right"); fig_s2


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s2.pdf", width=5, height=5, onefile=FALSE)

fig_s2 

dev.off()



# Figure S3: Fishing depth by day vs. night and warm vs. cool SSTs ----

# Fishing depth by day vs. night

# Add columns to haul_dat with day-night and cool-warm categories
haul_dat <- haul_dat %>% 
  left_join(dplyr::select(full_dat, haul_join, day_night) %>% distinct()) %>% 
  mutate(sst_cat = ifelse(sst_mean > 14, "warm", "cool"))

# day-night data
depth_dat_daynight <- haul_dat %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100))) %>% 
  group_by(fishing_bins, day_night) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  drop_na() %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(day_night2 = ifelse(day_night == "day", "(a) Day", "(b) Night"))
depth_dat_daynight

depth_dat_daynight$fishing_m <- rep(seq(50, 550, by=100), each=2)


# warm-cool data
depth_dat_sst <- haul_dat %>% 
  mutate(fishing_bins = cut(fishing_m, breaks=seq(0,600,by=100))) %>% 
  group_by(fishing_bins, sst_cat) %>% 
  summarise(mean_bpue = mean(bpue),
            count_hauls = length(bpue),
            bpue_sd = sd(bpue)) %>% 
  drop_na() %>% 
  mutate(se_bpue = bpue_sd / sqrt(count_hauls)) %>% 
  mutate(sst_cat2 = ifelse(sst_cat == "cool", "(c) Cool", "(d) Warm"))
depth_dat_sst

depth_dat_sst$fishing_m <- rep(seq(50, 550, by=100), each=2)

# day-night fig
fig_s3ab <- ggplot(data=depth_dat_daynight, aes(x=fishing_m, y=count_hauls)) + 
  geom_bar(stat="identity", color="black", aes(fill=day_night2)) + theme_bw() +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  ylab("Number of hauls") + xlab("Fishing depth (m)") +
  scale_fill_manual(values=c("gold", "gray22")) +
  facet_wrap(~day_night2, ncol=2, scales = "free") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position = "none") ; fig_s3ab


# warm-cool fig
fig_s3cd <- ggplot(data=depth_dat_sst, aes(x=fishing_m, y=count_hauls)) + 
  geom_bar(stat="identity", color="black", aes(fill=sst_cat2)) + theme_bw() +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  ylab("Number of hauls") + xlab("Fishing depth (m)") +
  scale_fill_manual(values=c("royalblue", "firebrick2")) +
  facet_wrap(~sst_cat2, ncol=2, scales = "free") +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.position = "none"); fig_s3cd


# All four plots of fishing depth by (a) day, (b) night, (c) cool, and (d) warm.
fig_s3_abcd <- ggarrange(fig_s3ab, fig_s3cd, ncol=1, nrow=2); fig_s3_abcd

setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s3.pdf", width=8, height=8, onefile=FALSE)

fig_s3_abcd

dev.off()



# Figure S3: Hurdle model plots ----

# DVM function for presence-absence
dvm_plot_fun_pa <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(time_num = seq_range(time_num, 50, pretty=T),
                                fishing_m = c(50, 100, 200, 300, 400),
                                lat = av_lat,
                                .model = model)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(time_num/3600), y=plogis(fit))) + 
    geom_ribbon(aes(ymin=plogis(fit-se), ymax=plogis(fit+se), fill=as.factor(fishing_m)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(fishing_m))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Probability of bycatch") + xlab(label="Time (hours since midnight)") +
    scale_fill_viridis(discrete = TRUE, direction=-1, name="Fishing depth (m)") + scale_color_viridis(discrete=TRUE, direction=-1, name="Fishing depth (m)")
} # end function

# Thermal Refugia: presence/absence
ref_plot_fun_pa <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16, 18),
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=plogis(fit))) + 
    geom_ribbon(aes(ymin=plogis(fit - se), ymax=plogis(fit + se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Probability of bycatch") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C)))
} # end function.

ref_plot_fun_16_pa <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16),
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=plogis(fit))) + 
    geom_ribbon(aes(ymin=plogis(fit-se), ymax=plogis(fit+se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Probabilty of bycatch") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C)))
} # end function.



fig_s3a <- dvm_plot_fun_pa(data=haul_dat, response="pa", model=full_mod_pa, title="(a) Probability of occurence"); fig_s3a
fig_s3b <- dvm_plot_fun(data=haul_dat_p, response="chinook_count", model=full_mod_p, title="(b) Positive Chinook bycatch"); fig_s3b
fig_s3c <- ref_plot_fun_pa(data=haul_dat, response="pa", model=full_mod_pa, title="(c) Probability of occurence"); fig_s3c
fig_s3d <- ref_plot_fun(data=haul_dat_p, response="chinook_count", model=full_mod_p, title="(d) Positive Chinook bycatch"); fig_s3d


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s3.pdf", width=8, height=8, onefile=FALSE)

ggarrange(fig_s3a, fig_s3b, fig_s3c, fig_s3d, ncol=2, nrow=2)

dev.off()




# Figure S4: ESU probability of occurrence DVM ----

fig_s4a <- dvm_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s4a
fig_s4b <- dvm_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s4b
fig_s4c <- dvm_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s4c
fig_s4d <- dvm_plot_fun_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s4d
fig_s4e <- dvm_plot_fun_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s4e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s4.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s4a, fig_s4b, fig_s4c, fig_s4d, fig_s4e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()



# Figure S5: ESU probability of occurrence Refugia ----

fig_s5a <- ref_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s5a
fig_s5b <- ref_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s5b
fig_s5c <- ref_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s5c
fig_s5d <- ref_plot_fun_16_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s5d
fig_s5e <- ref_plot_fun_16_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s5e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s5.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s5a, fig_s5b, fig_s5c, fig_s5d, fig_s5e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()



# Figure S4: ESU no threshold DVM ----

fig_s4a <- dvm_plot_fun(data=esu_dat$data[[1]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[1]], title="(a) Klamath - Trinity"); fig_s4a
fig_s4b <- dvm_plot_fun(data=esu_dat$data[[5]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[5]], title="(b) S. OR - N. CA"); fig_s4b
fig_s4c <- dvm_plot_fun(data=esu_dat$data[[2]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[2]], title="(c) OR Coast"); fig_s4c
fig_s4d <- dvm_plot_fun(data=esu_dat$data[[3]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[3]], title="(d) Puget Sound"); fig_s4d
fig_s4e <- dvm_plot_fun(data=esu_dat$data[[4]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[4]], title="(e) S. BC"); fig_s4e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s4.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s4a, fig_s4b, fig_s4c, fig_s4d, fig_s4e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()



# Figure S5: ESU no threshold Refugia ----

fig_s5a <- ref_plot_fun(data=esu_dat$data[[1]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[1]], title="(a) Klamath - Trinity") + coord_cartesian(ylim=c(0,0.2)); fig_s5a
fig_s5b <- ref_plot_fun(data=esu_dat$data[[5]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[5]], title="(b) S. OR - N. CA")  + coord_cartesian(ylim=c(0,3)); fig_s5b
fig_s5c <- ref_plot_fun(data=esu_dat$data[[2]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[2]], title="(c) OR Coast") + coord_cartesian(ylim=c(0,0.3)); fig_s5c
fig_s5d <- ref_plot_fun_16(data=esu_dat$data[[3]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[3]], title="(d) Puget Sound") + coord_cartesian(ylim=c(0,0.3)); fig_s5d
fig_s5e <- ref_plot_fun_16(data=esu_dat$data[[4]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[4]], title="(e) S. BC") + coord_cartesian(ylim=c(0,0.4)); fig_s5e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s5.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s5a, fig_s5b, fig_s5c, fig_s5d, fig_s5e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()


# Figure S4: ESU probability of occurrence DVM ----

fig_s4a <- dvm_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s4a
fig_s4b <- dvm_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s4b
fig_s4c <- dvm_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s4c
fig_s4d <- dvm_plot_fun_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s4d
fig_s4e <- dvm_plot_fun_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s4e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s4.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s4a, fig_s4b, fig_s4c, fig_s4d, fig_s4e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()



# Figure S5: ESU probability of occurrence Refugia ----

fig_s5a <- ref_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s5a
fig_s5b <- ref_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s5b
fig_s5c <- ref_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s5c
fig_s5d <- ref_plot_fun_16_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s5d
fig_s5e <- ref_plot_fun_16_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s5e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s5.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s5a, fig_s5b, fig_s5c, fig_s5d, fig_s5e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()






# Figure S6: Observed annual BPUE by SST ----

# Get annual SST values from haul locations.
sst_dat <- haul_dat %>% group_by(year) %>% 
  summarise(mean_sst = mean(sst_mean),
            count_sst = length(sst_mean),
            sd_sst = sd(sst_mean)) %>% 
  mutate(se_sst = sd_sst / sqrt(count_sst))


# Calculate observed BPUE by year
yr_sst_data <- haul_dat %>% group_by(year) %>% summarise(tot_bycatch = sum(chinook_count)) %>% 
  left_join(sst_dat)

# Figure 4
fig_s6 <- ggplot(data=yr_sst_data, aes(x=mean_sst, y=tot_bycatch, fill=mean_sst)) + 
  stat_smooth(method = "lm", fill= "gray88", color = "gray75", alpha=0) +
  geom_point(shape=21, size=3) +
  theme_classic() + theme(legend.position = c(0.7,0.1), legend.direction="horizontal", 
                          legend.background=element_rect(fill=alpha("white", 0)),
                          legend.key=element_rect(fill=alpha("white", 0.5))) +
  scale_fill_gradient(low="royalblue", high="red", name=expression("SST " ( degree*C))) +
  ylab("Observed total Chinook bycatch") + xlab("Mean annual SST") +
  geom_label_repel(aes(label = year), size=3, box.padding = 0.2, point.padding = 0.1, 
                   segment.color = 'grey50', fill="transparent", label.size = NA); fig_s6

summary(lm(tot_bycatch ~ mean_sst, data=yr_sst_data)) # summary of linear relationship in plot.

# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_s6.pdf", width=5, height=4, onefile=FALSE)

fig_s6

dev.off()







# Figure SX: ESU: annual bycatch by SST ----

# for Focal ESUs: year
year_grid_fun <- function(data, model){
  lat_pred <- data %>% filter(catch_esu_ia_8 > 0) %>% # generate unique lat to predict over - median positive catch for that ESU.
    summarise(lat_pred=median(lat)) %>% pull()
  
  newdata <- data %>% data_grid(year = seq(2008, 2015),
                                duration = rep(130, length.out = 50),
                                lat = lat_pred,
                                .model = model)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  pred_t$upr <- pred_t$fit + (1.96 * pred_t$se) # change se to 95% confidence intervals. 95% CI = fit + 1.96 * sd/sqrt(n) (but sd/sqrt(n) is se!)
  pred_t$lwr <- pred_t$fit - (1.96 * pred_t$se)
  return(pred_t)
}

# Generate data_grids and predictions with se for all model-data pairs!
foc_dat <- esu_dat %>%
  mutate(preds = map2(data, gam_esu_c8, year_grid_fun))
foc_dat

# Unnest so we can plot.
preds_esu_dat <- unnest(foc_dat, preds) %>% dplyr::select(esu, fit, se, year) %>% 
  left_join(annual_dat %>% dplyr::select(year, mean_sst))

# Plots

# Change ESU level names and order
levels(as.factor(preds_esu_dat$esu))

preds_esu_dat$esu <- as.factor(preds_esu_dat$esu)
levels(preds_esu_dat$esu) <- c("Klamath - Trinity", "OR Coast", "Puget Sound", "S. BC", "S. OR - N. CA")

preds_esu_dat <- preds_esu_dat %>% mutate(esu = fct_relevel(esu, "Klamath - Trinity", "S. OR - N. CA", "OR Coast", "Puget Sound", "S. BC"))
levels(preds_esu_dat$esu)



# Figure #
fig_sx <- ggplot(data=preds_esu_dat, aes(x=mean_sst, y=exp(fit), fill=esu)) + 
  stat_smooth(method = "lm", fill= "gray88", color = "gray75", alpha=0) +
  geom_point(shape=21, size=3) +
  theme_classic() +
  ylab("Predicted Chinook salmon\nbycatch per hour") + xlab("Mean annual SST") +
  geom_label_repel(aes(label = year), size=3, box.padding = 0.2, point.padding = 0.1, 
                   segment.color = 'grey50', fill="transparent", label.size = NA) +
  facet_wrap(~esu, scales="free") +
  theme(legend.position = "none"); fig_sx

# summary(lm(exp(fit) ~ mean_sst, data=(preds_esu_dat %>% filter(esu == "Klamath - Trinity")))) # 
# summary(lm(exp(fit) ~ mean_sst, data=(preds_esu_dat %>% filter(esu == "S. OR - N. CA")))) # 
# summary(lm(exp(fit) ~ mean_sst, data=(preds_esu_dat %>% filter(esu == "OR Coast")))) # 
# summary(lm(exp(fit) ~ mean_sst, data=(preds_esu_dat %>% filter(esu == "Puget Sound")))) # 
# summary(lm(exp(fit) ~ mean_sst, data=(preds_esu_dat %>% filter(esu == "S. BC")))) # 

# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_sx.pdf", width=8, height=5, onefile=FALSE)

fig_sx

dev.off()



#### REGUIA with negative binomial

fig_ref_nb <- ref_plot_fun(data=haul_dat, response="chinook_count", model=full_mod_nb, title="(a) All Chinook salmon"); fig_ref_nb 

# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_ref_nb.pdf", width=5, height=4, onefile=FALSE)

fig_ref_nb

dev.off()

### DVM with negative binomial

fig_dvm_nb <- dvm_plot_fun(data=haul_dat, response="chinook_count", model=full_mod_nb, title="(a) All Chinook salmon"); fig_dvm_nb

# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_dvm_nb.pdf", width=5, height=4, onefile=FALSE)

fig_dvm_nb

dev.off()














### REFUGIA with no filter

# By ESU: south to north
fig_2b <- ref_plot_fun(data=esu_dat$data[[1]], response="catch_esu", model=esu_dat$gam_all_esu_c0[[1]], title="(b) Klamath - Trinity") + coord_cartesian(ylim=c(0,0.02)); fig_2b
fig_2c <- ref_plot_fun(data=esu_dat$data[[5]], response="catch_esu", model=esu_dat$gam_esu_c0[[5]], title="(c) S. OR - N. CA") + coord_cartesian(ylim=c(0,0.07)); fig_2c
fig_2d <- ref_plot_fun(data=esu_dat$data[[2]], response="catch_esu", model=esu_dat$gam_esu_c0[[2]], title="(d) OR Coast") + coord_cartesian(ylim=c(0,0.17)); fig_2d
fig_2e <- ref_plot_fun_16(data=esu_dat$data[[3]], response="catch_esu", model=esu_dat$gam_esu_c0[[3]], title="(e) Puget Sound") + coord_cartesian(ylim=c(0,0.12)); fig_2e
fig_2f <- ref_plot_fun_16(data=esu_dat$data[[4]], response="catch_esu", model=esu_dat$gam_esu_c0[[4]], title="(f) S. BC") + coord_cartesian(ylim=c(0,0.008)); fig_2f


# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_2.pdf", width=10, height=6, onefile=FALSE)

ggarrange(fig_2a, fig_2b, fig_2c, fig_2d, fig_2e, fig_2f, ncol=3, nrow=2, common.legend = TRUE, legend="top")

dev.off()
