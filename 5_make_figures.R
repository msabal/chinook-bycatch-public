# Making Figures for PNAS

library(scales); library(raster); library(ggrepel)

# Run script 4 (4_run_models.R) FIRST.

#Check these are loaded
full_dat
haul_dat

full_mod
esu_dat

states2

# Lon x Lat
gb_r <- raster(x = str_c(in_drive, "Raw GIS Files/gebco_2021_n50.1_s39.9_w-128.1_e-122.9.tif"))
gb_cont_200 <- rasterToContour(gb_r, levels = -200) # Get contour lines
gb_cont_200 <- st_as_sf(gb_cont_200) # Covert to sf objects


# Figure 1: Summary of bycatch and fishing effort ----
## This is a multi-panel Figure, which I assemble in Inkscape. Individual plots made below:


# First calculate observed Chinook bycatch per vessel per hour.
h_chinook <-  h_chinook %>% mutate(bpue = (chinook_count / duration) *60)


# Make dataset summarizing data by lat/lon grid cells.

obs_final2 <- readRDS(str_c(in_drive, "Saved Files/MS_cleaned_ashop_all.rds"))

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



map_dat <- h_chinook %>% left_join(dplyr::select(obs_final2, haul_join, drvid)) %>% 
  mutate(lat = cut2(lat, breaks=40), lon = cut2(lon, breaks=10)) %>% 
  group_by(lat, lon) %>% summarise(n_hauls = n(),
                                   bpue = mean(bpue),
                                   n_vessels = n_distinct(drvid)) %>% 
  filter(n_vessels > 3)


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
  scale_fill_viridis(name="log(BPUE)") +
  #scale_fill_gradient(low="royalblue", high="firebrick3", name="log(BPUE)") +
  theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = c(0.2,0.5), legend.background=element_rect(fill = alpha("white", 0)),
        legend.key=element_rect(fill = alpha("white", .5)),
        legend.title = element_text(size=8)) +
  coord_sf(xlim = c(-127,-123.5),
           ylim = c(41.4,49)); fig_1b



# # Fishing effort (number of hauls): map
# fig_1a <- ggplot() + theme_classic() +
#   geom_tile(data=all_dat, aes(lon, lat, z = chinook_count), binwidth = 0.2, stat = "summary_2d", fun = length, alpha=0.5) +
#   geom_sf(data=states2, fill = "gray95") +
#   geom_sf(data=gb_cont_200, size=0.8) +
#   scale_fill_gradient2(low="gray80", high="royalblue", name="Total hauls", breaks=seq(0,2000, by=500)) +
#   theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   theme(legend.position = c(0.2,0.5), legend.background=element_rect(fill = alpha("white", 0)),
#         legend.key=element_rect(fill = alpha("white", .5)),
#         legend.title = element_text(size=8)) +
#   coord_sf(xlim = c(-127,-123.5),
#            ylim = c(41.4,49)); fig_1a

# # Observed Chinook bycatch per vessel per hour: map
# fig_1b <- ggplot() + theme_classic() +
#   geom_tile(data=filter(all_dat, bpue > 0), aes(lon, lat, z = bpue), binwidth = 0.2, stat = "summary_2d", fun = mean, alpha=0.5) +
#   geom_sf(data=states2, fill = "gray95") +
#   geom_sf(data=gb_cont_200, size=0.8) +
#   scale_fill_gradient(low="gray80", high="firebrick3", name="Observed Chinook\nbycatch per hour", breaks=seq(0,50, by=5)) +
#   theme(axis.title.x = element_blank(), axis.title.y=element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   theme(legend.position = c(0.3,0.5), legend.background=element_rect(fill = alpha("white", 0)),
#                                             legend.key=element_rect(fill = alpha("white", .5)),
#         legend.title = element_text(size=8)) +
#   coord_sf(xlim = c(-127,-123.5),
#            ylim = c(41.4,49)); fig_1b

# Focal ESU distributions: violin
esu_lat <- hdat %>% dplyr::select(haul_join, esu, catch_esu_ia_8, lat, lon) %>%   # Using 0.8 threshold right now! But maybe change later to composite proportion if figure out the SE.
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

# # Fig 1d: Chinook catch histogram
# catch_dat <- h_chinook %>%  mutate(count_bins = cut(chinook_count, breaks=c(-1,0.5,10,20,30,40,50,100,200,300)  ))  %>% 
#   group_by(count_bins) %>% count()
# 
# levels(catch_dat$count_bins) <- c("0", "10", "20", "30", "40", "50", "100", "200", "300")
# 
# fig_1d <- ggplot(data=catch_dat, aes(x=count_bins, y=n)) + 
#   geom_bar(stat="identity", color="black", fill="gray88") + theme_classic() +
#   scale_y_continuous(expand=c(0,0), limits=c(0,11000), name="Number of hauls", oob=squish) + 
#   xlab("Chinook caught per haul") +
#   geom_text(aes(label=n, y = n+600), size=4, angle=45) +
#   theme(strip.background = element_blank(), strip.text.x = element_blank()) +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         #panel.border = element_blank(),
#         panel.background = element_blank()); fig_1d


# Fig 1d: Fishing effort by fishing depth
depth_dat <- all_dat %>% 
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
  ylab("Number of hauls") + xlab("Fishing depth (m)"); fig_1d

# Fig 1e: Chinook bycatch by fishing depth

fig_1e <- ggplot(data=depth_dat, aes(x=fishing_m, y=mean_bpue)) + 
  geom_bar(stat="identity", color="black", fill="gray88") + theme_classic() +
  geom_errorbar(aes(ymin=mean_bpue+se_bpue, ymax=mean_bpue-se_bpue), width=0) +
  coord_flip() + scale_x_reverse(breaks=seq(0,600,by=50)) +
  scale_y_continuous(limits=c(0, 3.5), expand = c(0,0)) +
  ylab("Observed Chinook\nbycatch per hour") + xlab("Fishing depth (m)");  fig_1e
#geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.2), size=4, angle=45); fig_1e


# Fig 1f: Circular histogram: time of day
fig_1f <- ggplot(data=all_dat, aes(x=time_num/3600)) + geom_histogram(color="black", fill="lightblue", alpha=0.5) +
  coord_polar("x") + theme_bw() + scale_x_continuous(breaks=seq(0,24, by=1), name ="Time of Day") + 
  ylab("Number of hauls") + theme(panel.grid.major=element_line(color="gray50")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()); fig_1f


# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_1_def.pdf", width=8, height=4, onefile=FALSE)

ggarrange(fig_1d, fig_1e, fig_1f, ncol=3)

dev.off()



# Figure 2: DVM ----

# Make function for dvm plots with SE intervals.
dvm_plot_fun <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(time_num = seq_range(time_num, 50, pretty=T),
                                fishing_m = c(50, 100, 200, 300, 400),
                                duration = 120,
                                lat = av_lat,
                                .model = model)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(time_num/3600), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit-se), ymax=exp(fit+se), fill=as.factor(fishing_m)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(fishing_m))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook bycatch per hour") + xlab(label="Time (hours since midnight)") +
    scale_fill_viridis(discrete = TRUE, direction=-1, name="Fishing depth (m)") + scale_color_viridis(discrete=TRUE, direction=-1, name="Fishing depth (m)")
} # end function


fig_2a <- dvm_plot_fun(data=all_dat, response="chinook_count", model=full_mod, title="(a) All Chinook catch"); fig_2a

# By ESU: south to north
fig_2b <- dvm_plot_fun(data=esu_dat$data[[1]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[1]], title="(b) Klamath - Trinity"); fig_2b
fig_2c <- dvm_plot_fun(data=esu_dat$data[[5]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[5]], title="(c) S. OR - N. CA"); fig_2c
fig_2d <- dvm_plot_fun(data=esu_dat$data[[2]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[2]], title="(d) OR Coast"); fig_2d
fig_2e <- dvm_plot_fun(data=esu_dat$data[[3]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[3]], title="(e) Puget Sound"); fig_2e
fig_2f <- dvm_plot_fun(data=esu_dat$data[[4]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[4]], title="(f) S. BC"); fig_2f

# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_2.pdf", width=10, height=6, onefile=FALSE) # for ncol=4, width=12, height=6

ggarrange(fig_2a, fig_2b, fig_2c, fig_2d, fig_2e, fig_2f, ncol=3, nrow=2, common.legend = TRUE, legend="top")

dev.off()


# Figure 3: Thermal refugia ----

ref_plot_fun <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16, 18),
                                duration = 120,
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit - se), ymax=exp(fit + se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook bycatch per hour") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62", "#b2182b"), name=expression("SST " ( degree*C)))
} # end function.

ref_plot_fun_16 <- function(data, model, title, response){
  av_lat <- data %>% filter(response > 0) %>% summarise(av_lat = median(lat)) %>% pull()
  newdata <- data %>% data_grid(fishing_m = seq_range(fishing_m, 50, pretty=T),
                                sst_mean = c(10, 12, 14, 16),
                                duration = 120,
                                lat = av_lat,
                                .model = model) %>% 
    filter(fishing_m < 401 & fishing_m > 30)
  pred_out <- predict(model, newdata, se.fit=T)
  pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
  
  ggplot(data=pred_t, aes(x=(fishing_m), y=exp(fit))) + 
    geom_ribbon(aes(ymin=exp(fit-se), ymax=exp(fit+se), fill=as.factor(sst_mean)), alpha=0.2) + geom_line(size=1, aes(color=as.factor(sst_mean))) + theme_classic() +
    ggtitle(label = title) + theme(plot.title = element_text(hjust = 0.5)) + 
    theme(legend.position = "bottom") + ylab(label="Predicted Chinook bycatch per hour") + xlab(label="Fishing depth (m)") +
    scale_fill_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C))) +
    scale_color_manual(values=c("#2166ac", "#67a9cf", "#FDDBC7", "#ef8a62"), name=expression("SST " ( degree*C)))
} # end function.


fig_3a <- ref_plot_fun(data=all_dat, response="chinook_count", model=full_mod, title="(a) All Chinook catch"); fig_3a

# By ESU: south to north
fig_3b <- ref_plot_fun(data=esu_dat$data[[1]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[1]], title="(b) Klamath - Trinity") + coord_cartesian(ylim=c(0,0.03)); fig_3b
fig_3c <- ref_plot_fun(data=esu_dat$data[[5]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[5]], title="(c) S. OR - N. CA") + coord_cartesian(ylim=c(0,0.13)); fig_3c
fig_3d <- ref_plot_fun(data=esu_dat$data[[2]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[2]], title="(d) OR Coast") + coord_cartesian(ylim=c(0,0.3)); fig_3d
fig_3e <- ref_plot_fun_16(data=esu_dat$data[[3]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[3]], title="(e) Puget Sound") + coord_cartesian(ylim=c(0,0.15)); fig_3e
fig_3f <- ref_plot_fun_16(data=esu_dat$data[[4]], response="catch_esu_ia_8", model=esu_dat$gam_esu_c8[[4]], title="(f) S. BC") + coord_cartesian(ylim=c(0,0.017)); fig_3f


# Save plot
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_3.pdf", width=10, height=6, onefile=FALSE)

ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, fig_3f, ncol=3, nrow=2, common.legend = TRUE, legend="top")

dev.off()


# Figure 4: Annual bycatch by annual SST ----

# Get annual SST values from haul locations.
sst_dat <- all_dat %>% group_by(year) %>% 
  summarise(mean_sst = mean(sst_mean),
            count_sst = length(sst_mean),
            sd_sst = sd(sst_mean)) %>% 
  mutate(se_sst = sd_sst / sqrt(count_sst))


# Get model predictions of bycatch per effort for year term in the model.
newdata <- all_dat %>% data_grid(year = seq(2002,2021, by=1),
                                 duration = rep(120, length.out=20),
                                 .model = full_mod)
pred_out <- predict(full_mod, newdata, se.fit=T)
pred_t <- as_tibble(data.frame(fit = pred_out$fit, se = pred_out$se.fit)) %>% bind_cols(newdata)
pred_t <- left_join(pred_t, sst_dat) # Join predicted annual bycatch estimates with mean SST values.

# Figure 3
fig_4 <- ggplot(data=pred_t, aes(x=mean_sst, y=exp(fit), fill=mean_sst)) + 
  stat_smooth(method = "lm", fill= "gray88", color = "gray75", alpha=0) +
  geom_point(shape=21, size=3) +
  theme_classic() + theme(legend.position = c(0.7,0.1), legend.direction="horizontal", 
                          legend.background=element_rect(fill=alpha("white", 0)),
                          legend.key=element_rect(fill=alpha("white", 0.5))) +
  scale_fill_gradient(low="royalblue", high="red", name=expression("SST " ( degree*C))) +
  ylab("Predicted Chinook bycatch per hour") + xlab("Mean annual SST") +
  geom_label_repel(aes(label = year), size=3, box.padding = 0.2, point.padding = 0.1, 
                   segment.color = 'grey50', fill="transparent", label.size = NA); fig_4

summary(lm(exp(fit) ~ mean_sst, data=pred_t)) # summary of linear relationship in plot.

# Plots to Save
setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_4.pdf", width=5, height=4, onefile=FALSE)

fig_4

dev.off()

# Figure 5: DVM & Thermal refugia ----

ref_dvm_dat <- h_chinook %>% filter(fishing_m < 600) %>%
  drop_na() %>% 
  mutate(depth_bin = cut(fishing_m, breaks=seq(0,600,by=100)),
         bpue = chinook_count / duration *60) %>% 
  mutate(sst_bin = ifelse(sst_mean > 14, "(b) SST > 14 degrees C", 
                          ifelse(sst_mean < 14, "(a) SST < 14 degrees C", "middle"))) %>% 
  group_by(depth_bin, day_night, sst_bin) %>%
  summarise(mean_chinook = mean(chinook_count),
            mean_bpue = mean(bpue),
            sd_chinook = sd(chinook_count),
            sd_bpue = sd(bpue),
            count = length(chinook_count)) %>% 
  mutate(se = sd_chinook / sqrt(count),
         se_bpue = sd_bpue / sqrt(count),
         day_night = ifelse(day_night == "day", "Day", "Night"))
ref_dvm_dat

ref_dvm_dat$fishing_m <- rep(seq(50,550,by=100), each=4)

# Expand data for scenarios
depth_dat <- depth_dat %>% mutate(p_hauls = count_hauls / 54518)

scenario_dat1 <- ref_dvm_dat %>% rename("fishing_bins" = "depth_bin") %>% 
  left_join(dplyr::select(depth_dat, fishing_bins, p_hauls)) %>% 
  mutate(night_fishing_bin = "Even")

scenario_dat <- scenario_dat1 %>% 
  mutate(night_fishing_bin = "Restrictions") %>% 
  rbind(scenario_dat1) %>% 
  mutate(num_tows = ifelse(night_fishing_bin == "Restrictions" & day_night == "Night", 250,
                           ifelse(night_fishing_bin == "Restrictions" & day_night == "Day", 750, 500))) %>%
  dplyr::select(1:3,5,11:14) %>% 
  mutate(tot_bycatch = num_tows * p_hauls * mean_bpue)

scenario_dat %>% group_by(sst_bin, night_fishing_bin) %>% summarise(tot_bycatch = sum(tot_bycatch)) # This is amazing!!!
sum_dat <- scenario_dat %>% group_by(sst_bin, night_fishing_bin, day_night) %>% summarise(tot_bycatch = sum(tot_bycatch)) %>% 
  mutate(scenario_cat = ifelse(sst_bin == "(a) SST < 14 degrees C" & night_fishing_bin == "Even", "<14 &\nNo restrictions", 
                               ifelse(sst_bin == "(a) SST < 14 degrees C" & night_fishing_bin == "Restrictions", "<14 &\nRestrictions",
                                      ifelse(sst_bin == "(b) SST > 14 degrees C" & night_fishing_bin == "Even", ">14 &\nNo restrictions", ">14 &\nRestrictions"))))
sum_dat

# Make multiple figures
# Split facets manually because will make arranging multiple figures easier.
bpue <- ref_dvm_dat %>% filter(sst_bin == "(b) SST > 14 degrees C")
cool_dat <-ref_dvm_dat %>% filter(sst_bin == "(a) SST < 14 degrees C")

warm_dat_sc <- scenario_dat %>% filter()


# Flipped facets

fig_5a <- ggplot(data=ref_dvm_dat, aes(x=as.factor(depth_bin), y=mean_bpue, fill=day_night)) +
  geom_bar(stat="identity", color="black", alpha=0.7, position=position_dodge(0.9, preserve='single')) +
  facet_wrap(~sst_bin, ncol=1) +
  scale_y_continuous(limits=c(0,3.6), expand=c(0,0), name = "Observed Chinook bycatch per hour") + theme_bw() +
  geom_errorbar(aes(ymin=mean_bpue-se_bpue, ymax=mean_bpue+se_bpue), color="black", width=0, position=position_dodge(0.9, preserve='single')) +
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
  xlab("Fishing depth bin (m)") +
  theme(strip.text = element_text(face = "bold", size=14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()) +
  geom_text(aes(label=count, y = mean_bpue + se_bpue + 0.3), size=3, position=position_dodge(0.9)); fig_5a


setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_5.pdf", width=6, height=6, onefile=FALSE)

fig_5a

dev.off()


# Total abundance
fig_5b <- ggplot(data=scenario_dat, aes(x=as.factor(fishing_bins), y=tot_bycatch, fill=day_night)) +
  geom_bar(stat="identity", color="black", alpha=0.7, position=position_dodge(0.9, preserve='single')) +
  facet_wrap(~sst_bin + night_fishing_bin, ncol=2) +
  scale_y_continuous(limits=c(0,500), expand=c(0,0), name = "Estimated Chinook bycatch per 1000 hours") + theme_bw() +
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_color_manual(values=c("gold", "gray22"), name="Time bin") +
  xlab("Fishing depth bin (m)") +
  theme(strip.text = element_text(face = "bold", size=14)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()); fig_5b


fig_5c <- ggplot(data=sum_dat, aes(x=scenario_cat, y=tot_bycatch, fill=day_night)) +
  geom_bar(stat="identity", color="black") + theme_bw() + 
  scale_fill_manual(values=c("gold", "gray22"), name="Time bin") +
  scale_y_continuous(limits=c(0,1000), expand=c(0,0), name = "Estimated Chinook bycatch per 1000 hours") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "top", legend.title = element_blank()); fig_5c



ggarrange(fig_5a, fig_5b, ncol=2, common.legend = TRUE)


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
  #geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.05), size=4, angle=45) +
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
  #geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.05), size=4, angle=45) +
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
  #geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.03), size=4, angle=45) +
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
  #geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.02), size=4, angle=45) +
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
  #geom_text(aes(label=count_hauls, y=mean_bpue + se_bpue+ 0.03), size=4, angle=0) +
  ggtitle(label = "(e) S. BC") + theme(plot.title = element_text(hjust = 0.5)); fig_s1e


setwd("C:/Users/sabalm/Desktop/")
pdf("Fig_S1.pdf", width=10, height=4, onefile=FALSE)

ggarrange(fig_s1a, fig_s1b, fig_s1c, fig_s1d, fig_s1e, ncol=5, nrow=1)

dev.off()




# Figure S2: Hurdle model plots ----

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



fig_s2a <- dvm_plot_fun_pa(data=all_dat, response="pa", model=full_mod_pa, title="(a) Probability of occurence"); fig_s2a
fig_s2b <- dvm_plot_fun(data=all_dat_p, response="chinook_count", model=full_mod_p, title="(b) Positive Chinook bycatch"); fig_s2b
fig_s2c <- ref_plot_fun_pa(data=all_dat, response="pa", model=full_mod_pa, title="(c) Probability of occurence"); fig_s2c
fig_s2d <- ref_plot_fun(data=all_dat_p, response="chinook_count", model=full_mod_p, title="(d) Positive Chinook bycatch"); fig_s2d


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s2.pdf", width=8, height=8, onefile=FALSE)

ggarrange(fig_s2a, fig_s2b, fig_s2c, fig_s2d, ncol=2, nrow=2)

dev.off()


# Figure S3: ESU probability of occurrence DVM ----

fig_s3a <- dvm_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s3a
fig_s3b <- dvm_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s3b
fig_s3c <- dvm_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s3c
fig_s3d <- dvm_plot_fun_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s3d
fig_s3e <- dvm_plot_fun_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s3e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s3.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s3a, fig_s3b, fig_s3c, fig_s3d, fig_s3e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()



# Figure S4: ESU probability of occurrence Refugia ----

fig_s4a <- ref_plot_fun_pa(data=esu_dat$data[[1]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[1]], title="(a) Klamath - Trinity"); fig_s4a
fig_s4b <- ref_plot_fun_pa(data=esu_dat$data[[5]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[5]], title="(b) S. OR - N. CA"); fig_s4b
fig_s4c <- ref_plot_fun_pa(data=esu_dat$data[[2]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[2]], title="(c) OR Coast"); fig_s4c
fig_s4d <- ref_plot_fun_16_pa(data=esu_dat$data[[3]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[3]], title="(d) Puget Sound"); fig_s4d
fig_s4e <- ref_plot_fun_16_pa(data=esu_dat$data[[4]], response="esu_pa", model=esu_dat$gam_esu_c8_pa[[4]], title="(e) S. BC"); fig_s4e


setwd("C:/Users/sabalm/Desktop/")
pdf("fig_s4.pdf", width=9, height=6, onefile=FALSE)

ggarrange(fig_s4a, fig_s4b, fig_s4c, fig_s4d, fig_s4e, ncol=3, nrow=2, common.legend = TRUE)

dev.off()


# Figure S4: ----
