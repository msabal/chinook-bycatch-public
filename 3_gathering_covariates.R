
# Gathering and Adding Covariates

# Outline of script ----
# 1 - Set settings
# 2 - Import data
# 3 - SST
# 4 - Day vs. night (local)
# 5 - Distances from shore & 200m isobath
# 6 - Combine datasets

# 1 - Set settings ----

# Load libraries
library(ncdf4); library(raster); library(sf)
library(pbapply)
library(tidyverse); library(lubridate)
library(suncalc); library(hms)

# Set settings for print (to be like head())
options(tibble.print_max = 5)
options(tibble.width = Inf)

# Location where my files are stored
in_drive <- "C:/Users/sabalm/Documents/Chinook-Hake-Bycatch-Data/Data/"



# 2 - Import data ----

# load the final cleaned dataset by haul with only selected columns.
full_dat <- readRDS(str_c(in_drive, "Saved Files/data_by_haul_and_esu_v1.rds"))

# Make data frame haul_dat with each unique haul from 2002 - 2021 (drop duplicate rows by all 19 ESUs)
haul_dat <- full_dat %>% 
  dplyr::select(-esu, -pa_esu, -catch_esu, -catch_esu_cp, -catch_esu_ia_8) %>% 
  distinct() %>% 
  arrange(datetime)

haul_dat %>% 
  group_by(haul_join) %>% 
  count() %>% 
  filter(n > 1)
# no duplicate rows, good. N = 54510


# Load natural earth file
states2 <- readRDS(str_c(in_drive, "Saved GIS Files/naturalearth_states.rds"))


# 3 - SST ----

# Description: NOAA High-resolution Blended Analysis of Daily SST and Ice (# SST info available: https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html)
# Spatial coverage: 0.25 degree latitude x 0.25 degree longitude global grid (1440x720).

# Cite:
# Huang, B., C. Liu, V. Banzon, E. Freeman, G. Graham, B. Hankins, T. Smith, and H.-M. Zhang, 2021: Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST) Version 2.1. J. Climate, 34, 2923-2939, DOI 10.1175/JCLI-D-20-0166.1 (V2.1).
# Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496 https://doi.org/10.1175/2007JCLI1824.1.


# Download all relevant sst files

#What years do we need data for?
years <- c(2002:2021) # If ever re-run for the entire dataset.

#SST file names to download
files <- str_c("sst.day.mean.",years,".nc")

#Download SST data: mean for selected years. This will take a while! 
lapply(files, function(filenames){
  download.file(str_c("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/",filenames), destfile =  str_c(in_drive, "Raw GIS Files/SST/", filenames), mode = "wb")
})


# My updates to Philip's function to get SST values by haul.

get_SST <- function(dataset, 
                    lag_days = 0,
                    sst_type = 'mean', # "mean" or "anom"
                    sst_folder = paste0(in_drive, 'Raw GIS files/SST'), # my folder of netCDF files from NOAA
                    show.progress = FALSE){
  # sst_type can be 'mean' or 'anom'. This is based on the netcdf file names from NOAA.
  # this function assumes that "dataset" has column "datetime"
  
  # original data order
  dataset$ID <- 1:nrow(dataset)
  
  # lag date
  dataset$datetime_lag <- dataset$datetime - (lag_days * 3600*24) # default datetime units are seconds, so they need to be converted to days
  dataset$year_lag <- as.numeric(format(dataset$datetime_lag, format = '%Y'))
  dataset$doy_lag  <- as.numeric(format(dataset$datetime_lag, format = '%j'))
  # nc SST longitude values are from 0 to 360
  dataset <- dataset %>%  
    dplyr::mutate(x = (lon + 360)%%360, # mod isn't actually necessary since our study area is only negative longitudes
                  y = lat) %>%
    # I change the longitude manually instead of using sf::st_shift_longitude(), 
    # because sf::st_shift_longitude() is slow and b/c "dataset" loses the "sf" 
    # class when assigning values the way that I do, which prevents the sf:: 
    # functions from actually working
    dplyr::as_tibble() # make sure it's not an sf object
  
  # arrange data by year
  dataset <- dataset %>% dplyr::arrange(datetime)
  
  # Column for SSTs
  colindex <- ifelse(test = sst_type %in% names(dataset),
                     yes = match(x = sst_type, table = names(dataset)), 
                     no = ncol(dataset) + 1)
  dataset[, colindex] <- NA
  column_names <- names(dataset)
  column_names[colindex] <- sst_type #this name is just temporary. 
  
  # create progress bar
  if(show.progress){
    counter <- 1
    pb <- pbapply::startpb(0, nrow(dataset))
    on.exit(pbapply::closepb(pb))
  }
  
  # loop over years 
  #    this is to speed things up by decreasing the number of times that 
  #    each SST dataset is read into R
  for(this.yr in unique(dataset$year_lag)){
    # print(this.yr)
    
    # subset the data
    daty <- dataset %>% 
      dplyr::filter(year_lag == this.yr)
    
    # get the netcdf files with mean or anomaly
    # as a raster brick
    nc <- raster::brick(x = paste0(#getwd(),
      #"/",
      sst_folder,
      "/sst.day.", 
      sst_type, 
      ".", 
      this.yr, 
      ".nc"))
    
    # loop over unique Day of Year
    for(this.doy in unique(daty$doy_lag)){ # for each row i (i.e. each haul)
      # print(this.doy)
      
      # subset the data
      datd <- daty %>% 
        dplyr::filter(doy_lag == this.doy)
      
      # interpolate the SST values for all hauls on a particular day
      datd$temp <- raster::extract(x = nc[[this.doy]],     # subset the relevant doy
                                   y = datd[,c('x', 'y')], # coordinates of hauls
                                   method = 'bilinear')
      
      # add the sst back into the dataset
      dataset[match(x     = datd$ID, 
                    table = dataset$ID), 
              sst_type ] <- datd$temp
      
      # update the pbapply timer
      if(show.progress){
        pbapply::setpb(pb, counter)
        counter <- counter + nrow(datd)
      }
    } # end for loop over day of year
  } # end of loop over years
  
  rdata <- dataset %>% dplyr::arrange(ID)
  return(rdata %>% dplyr::pull(!!as.symbol(sst_type)))
} # end function get_SST

# Try it out on one year
 # sub_dat <- haul_dat %>% filter(year == 2015)
 # 
 # sub_dat$sst_mean <- get_SST(dataset = haul_dat, lag_days = 0,   sst_type = 'mean', show.progress = T)
 # sub_dat

# Extract SST mean for all years
haul_dat$sst_mean <- get_SST(dataset = haul_dat, lag_days = 0, sst_type = "mean", show.progress = T)


# 4 - Local day vs. night ----

# Calculate day vs. night based on latitude and day of the year

# Get local sunrise & sunset times
suncalc_dat <- haul_dat %>% 
  getSunlightTimes(data= ., keep = c("dawn", "dusk"), tz = "America/Los_Angeles") %>% 
  as_tibble()
suncalc_dat # 54510 rows the same as haul_dat

# Combine dawn and dusk times with haul_dat
suncalc_dat <- suncalc_dat %>%
  dplyr::select(dawn, dusk) %>% 
  bind_cols(haul_dat) %>%
  dplyr::select(haul_join, dawn, dusk, time_hms) %>% 
  mutate(dusk_time = parse_hms(format(suncalc_dat$dusk, format = "%H:%M:%S")),
         dawn_time = parse_hms(format(suncalc_dat$dawn, format = "%H:%M:%S"))) %>% 
  mutate(day_night = ifelse(time_hms > dawn_time & time_hms < dusk_time, "day", "night")) %>% 
  dplyr::select(haul_join, dusk_time, dawn_time, day_night)
suncalc_dat

haul_dat <- haul_dat %>% left_join(suncalc_dat)


# 5 - Distances from shore & 200m isobath ----

# Get bathymetry (depth) from from GECBO (https://www.gebco.net/).
# Click on Gridded Bathymetry Data - Read More, then Download data for user-defined regions.
# Then, enter boundaries for our dataset 50.1N, 39.9S, -128.1W, -122.9E.
# Select format: GeoTIFF
# Then download your data!

# Read in the file.
gb_r <- raster(x = str_c(in_drive, "Raw GIS Files/gebco_2021_n50.1_s39.9_w-128.1_e-122.9.tif"))
plot(gb_r)

# Get contour lines
gb_cont_200 <- rasterToContour(gb_r, levels = -200)
gb_cont_0 <- rasterToContour(gb_r, levels = 0)

plot(rasterToContour(gb_r, levels = c(0, -200))) # look at both together (shoreline and 200m isobath)

# Covert to sf objects
gb_cont_200 <- st_as_sf(gb_cont_200)
gb_cont_0 <- st_as_sf(gb_cont_0)

haul_dat_sf <- st_as_sf(haul_dat,
                    coords = c("lon", "lat"), # choose retrieval long and lat values.
                    crs = 4326, # this is the standard CRS for GPS data
                    remove = F) # this prevents the removal of the "lon" and "lat" columns


# Calculate distance from spatial haul points (in haul_dat_sf) to the contour lines
dist_200 <- st_distance(haul_dat_sf, gb_cont_200) # points, area
dist_0 <- st_distance(haul_dat_sf, gb_cont_0) # points, area
head(dist_200); head(dist_0) # says the units are in m!

# Add these distances to main dataset, store as numeric, and convert to km.
haul_dat$dist_200 <- as.numeric(dist_200)/1000
haul_dat$dist_0 <- as.numeric(dist_0)/1000


# How to discern between dist_200 values inshore vs. offshore of the 200m isobath????

# Get polygon between 0 and -200 m depth (aka continental shelf)
gb_poly <- rasterToPolygons(gb_r, fun = function(x){x > -200 & x < 0})
# plot(gb_poly) # this works, but takes a long time to run.

# Convert to sf object
gb_shelf <- st_as_sf(gb_poly)

# See if haul points are contained within the shelf polygon:
# join points file with gb_shelf, returns haul_dat_sf with a new column called
# "gebco_2021_n50.1_s39.9_w.128.1_e.122.9" which has an NA if the haul was NOT
# on the continental shelf polygon and returns a value if it was found on the shelf polygon.
shelf_hauls <- st_join(haul_dat_sf, gb_shelf, join = st_within)

# Reorganize and make new column of on-shelf/off-shelf
shelf_hauls2 <- shelf_hauls %>% 
  mutate(shelf_cat = as.factor(ifelse(!is.na(gebco_2021_n50.1_s39.9_w.128.1_e.122.9), 
                                      "on-shelf", "off-shelf")),
         dist_200 = ifelse(shelf_cat == "off-shelf", dist_200/1000, (-1*dist_200)/1000)) %>% 
  dplyr::select(haul_join, shelf_cat) %>% 
  st_drop_geometry() %>% 
  distinct()

# Join new on-shelf/off-shelf category into haul_dat.
haul_dat <- haul_dat %>% 
  left_join(shelf_hauls2) %>% 
  mutate(dist_200 = ifelse(shelf_cat == "off-shelf", dist_200, (-1*dist_200)))

haul_dat_sf <- st_as_sf(haul_dat, coords = c("lon", "lat"), crs = 4326, remove = F) 

# Look at hauls on-shelf (green) and off-shelf (blue)
ggplot() + geom_sf(data=states2, fill = "gray95") +
  geom_sf(data=gb_cont_200) +
  geom_point(data= filter(haul_dat_sf, shelf_cat == "on-shelf"), aes(lon, lat), shape = 21, size = 0.5, color = "green") +
  geom_point(data=filter(haul_dat_sf, shelf_cat == "off-shelf"), aes(lon, lat), shape = 21, size = 0.5, color = "blue") +
  theme_bw() +
  coord_sf(xlim = range(haul_dat_sf$lon),
           ylim = range(haul_dat_sf$lat)) 

# Look at random hauls on a map with their calculated distance to see if things make sense.
# get a random number of points from haul_dat_sf
haul_dat_sub <- haul_dat[sample(nrow(haul_dat), 10), ]

# look at a subset of points and label by their calculated distance (km) to the 200m isobath.
# do the numbers seem to make sense?
ggplot() + geom_sf(data=states2, fill = "gray95") +
  geom_sf(data=gb_cont_200) +
  geom_point(data=haul_dat_sub, aes(lon, lat), shape = 21, fill = "slateblue1") +
  theme_bw() +
  geom_text(data=haul_dat_sub, aes(x=lon-0.3, y=lat, label=trunc(dist_200)), size=3.5, fontface="bold") +
  coord_sf(xlim = range(haul_dat_sf$lon),
           ylim = range(haul_dat_sf$lat)) # awesome! these look great!
# Everything looks good! on-shelf points are negative values and fall inside the 200 m isobath!
# 


# 6 - Check for NAs and Export Final Dataset ----

# Check for NAs

haul_dat %>%
  ungroup() %>% 
  summarise_all(funs(sum(is.na(.))))
# one NA for sst_mean

# Join haul covariates back with the full dataset by ESU catches.
haul_covs_only <- haul_dat %>% dplyr::select(haul_join, sst_mean:shelf_cat)

full_dat <- full_dat %>% left_join(haul_covs_only) %>%  # N = 1035690 = 54510 * 19
  filter(!is.na(sst_mean)) # drop the haul with NA values for SST: N = 1035690 - 19 = 1035671 (good!)

# Export
saveRDS(full_dat, str_c(in_drive, "Saved Files/data_by_haul_and_esu_v2.rds")) # N = 1035690


