
# Initial Data Tidying: Cleaning observer haul dataset building on Philip's cleaning code.

# Outline of script ----
# 1 - Set settings
# 2 - Import data file (observer data) and re-organize
# 3 - Check: location (with mapping)
# 4 - Check: trawl depth
# 5 - Check: haul duration
# 6 - Check: bottom depth
# 7 - Final data updates
# 8 - Save final cleaned file


# 1 - Set settings ----

# Load libraries
library(tidyverse); library(lubridate); library(hms); library(janitor); library(DT)
library(sf); library(rnaturalearth); library(rgdal)
library(ncdf4); library(raster)

# Set settings for print (to be like head())
options(tibble.print_max = 5)
options(tibble.width = Inf)


# 2 - Import datafile (observer data) and re-organize files ----

# Import data files
# Location where my files are stored
in_drive <- "C:/Users/sabalm/Documents/Chinook-Hake-Bycatch-Data/Data/"

obs_dat <- readRDS(str_c(in_drive, "Raw Files/ashop_haul_info_20220211.rds"))
obs_dat_2021 <- readRDS(str_c(in_drive, "Raw Files/ashop_2021_haul_info_20220511.rds"))

# look at structure
str(obs_dat)
str(obs_dat_2021)

# join 2021 data with the rest of the data
obs_dat <- bind_rows(obs_dat, obs_dat_2021)

# Check for errors

# Make sure hauls are unique (no duplicates) - yes, good.
n_distinct(obs_dat$unique_haul_id) == nrow(obs_dat)

# Check the earliest deployment date (should be the opening of the fishing season): 5/15/2002 at 7:55 am.
min(obs_dat$deployment_date)

# Check that if we set the timezone it still matches the original datetime: yes, it does.
obs_dat %>% 
  mutate(deployment_date_tz = with_tz(deployment_date, tzone = "America/Los_Angeles")) %>% 
  dplyr::select(deployment_date, deployment_date_tz)


# Clean dataset: make a few new columns.
obs_dat <- obs_dat %>% 
  mutate(date_obs_dep = as_date(deployment_date),
         date_obs_ret = as_date(retrieval_date),
         year = year(deployment_date),
         month = month(deployment_date),
         doy = yday(deployment_date),
         time_num = as.numeric(parse_hms(paste(hour(obs_dat$deployment_date), minute(obs_dat$deployment_date), "00", sep = ":"))),
         time_hms = parse_hms(paste(hour(obs_dat$deployment_date), minute(obs_dat$deployment_date), "00", sep = ":")),
         fishing_fa_to_m = fishing_depth_fathoms * 1.8288,
         bottom_fa_to_m = bottom_depth_fathoms * 1.8288) %>% 
  rename(year_obs = year)


# 3 - Check: location (with mapping) ----

# Make a "simple feature" dataset for mapping.
# Good resources here about library(sf) and spatial data in R (https://www.jessesadler.com/post/gis-with-r-intro/)

obs_sf <- st_as_sf(obs_dat,
                   coords = c("londd_end", "latdd_end"), # choose retrieval long and lat values.
                   crs = 4326, # this is the standard CRS for GPS data
                   remove = F) # this prevents the removal of the "lon" and "lat" columns


# Check accuracy of lat/long locations:
# Are any locations over land?

# Download data on coastline from natural earth
# Get the states of North America.

# states  <- ne_download(scale = 'large',
#                        type = 'states',
#                        category = 'cultural',
#                        returnclass = 'sf') %>%
#   subset(., admin %in% c('United States of America', 'Canada', 'Mexico'))
# 
# states2 <- st_transform(x = states, crs = 4326) # transform states to projection crs = 4326 (projection: EPSG 4326)
# 
# # Save natural earth states dataset.
# saveRDS(object = states2, file = str_c(in_drive, "Saved GIS Files/naturalearth_states.rds"))

# Load natural earth states dataset.
states2 <- readRDS(str_c(in_drive, "Saved GIS Files/naturalearth_states.rds"))

# Check to see which hauls are over land:  make a new column called over_land that defines ocean is there is no postal location and if there is, then it must be overland.
obs_sf <- st_join(obs_sf, states2) %>% # join the states dataframe with spatial dataset of observer dataset.
  mutate(over_land = ifelse(is.na(postal), "ocean", "land"))     

# Summarize overland-ness
obs_sf %>% 
  filter(over_land == "land") %>% 
  count()
# One point is overland 

# Look at the point that is overland
obs_sf %>% 
  filter(over_land == "land") %>% 
  dplyr::select(haul_join, unique_haul_id, bottom_fa_to_m, fishing_fa_to_m, year)
overland_haul_id <- "98126338352005"
# The issue unique_haul_id is 98126338352005, bottom depth is supposedly 256 meters

# Plot of those over land
ggplot() + 
  theme_classic() + 
  geom_sf(data = states2, fill="gray") + 
  geom_sf(data = obs_sf, shape = 21) +
  geom_sf(data = obs_sf %>% filter(over_land != 'ocean'), 
          size = 2, 
          color = 'red') + facet_wrap(~year, ncol = 10) +
  coord_sf(xlim = range(obs_sf$londd_end), # note the coords_sf need to be at the end otherwise the last geom_sf overwrites the coordinate limits.
           ylim = range(obs_sf$latdd_end))


# plot of those over land - zoomed in
ggplot() + 
  theme_classic() + 
  geom_sf(data = states2, fill="gray") + 
  geom_sf(data = obs_sf, shape = 21) +
  geom_sf(data = obs_sf %>% filter(over_land != 'ocean'), 
          size = 2, 
          color = 'red') +
  coord_sf(xlim = c(-126, -124), # note the coords_sf need to be at the end otherwise the last geom_sf overwrites the coordinate limits.
           ylim = c(48, 49))

#### DROP one haul ("98126338352005") in Canada, but do this at the end of this script with any other adjustments.


# 4 - Check: trawl depth ----

# Check: are there hauls where the fishing depth is deeper than the bottom depth?
obs_sf %>% 
  filter(fishing_fa_to_m > bottom_fa_to_m)
# None! good.

# Check: are there hauls where the fishing depth is 0?
obs_sf %>% 
  filter(fishing_fa_to_m == 0)
# N = 5 hauls where fishing depths are 0. Discuss with observer program lead.


# Check for missing data in all columns
obs_dat %>%
  summarise_all(funs(sum(is.na(.))))
# 31 NAs in duration_in_min
# 75 NAs in bottom_depth_fathoms


# 5 - Check: haul duration ----

# Look at hauls with NA duration or very short (less than 10 minutes)
obs_dat %>% 
  filter(duration_in_min < 10) %>% 
  dplyr::select(unique_haul_id, duration_in_min, chinook_count, deployment_date, retrieval_date)
# Short hauls still often catch fish. 8 hauls have duration of 0 mins. Discuss with observer program lead.


# 6 - Check: bottom depth ----

# Bottom Depth NAs (75 hauls)

# First, look at them.
obs_dat %>% 
  filter(is.na(bottom_fa_to_m)) %>% 
  dplyr::select(unique_haul_id, duration_in_min, chinook_count, deployment_date, retrieval_date, fishing_fa_to_m, bottom_fa_to_m, latdd_end, londd_end)
# All hauls caught 0 chinook.


# Get raster spatial datasets with bottom depths and use this to estimate these bottom depths from the lat/lon locations.

# Get bottom depths from # NOAA Coastal Relief Model (https://www.ngdc.noaa.gov/mgg/coastal/crm.html)
# # file name of the NOAA Coastal Relief Model bathymetry data for Vol 7 (Central Pacific)

crm7_filename <- str_c(in_drive, "Raw GIS Files/noaa_crm_bathymetry_vol7.nc")

# download.file(url = 'https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol7.nc', 
#               destfile = "noaa_crm_bathymetry_vol7.nc", 
#               mode = 'wb') # need to make sure to write the file in binary (wb), or it won't open

crm8_filename <- str_c(in_drive, "Raw GIS Files/noaa_crm_bathymetry_vol8.nc")

# download.file(url = 'https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol8.nc', 
#               destfile = "noaa_crm_bathymetry_vol8.nc", 
#               mode = 'wb') # need to make sure to write the file in binary (wb), or it won't open


# Read in the bathymetry data as raster files with raster
bath7.r <- raster(x = crm7_filename)
bath7.r
plot(bath7.r) #cool.

bath8.r <- raster(x = crm8_filename)
bath8.r
plot(bath8.r)

# Do the rasters intersect? 
intersect(extent(bath7.r), extent(bath8.r)) # similar longitudes, but different latitude areas.
# They overlap between lats 44.0042 and 43.99958 (so very very small overlap!)

# Merge the two raster datasets
bath0 <- merge(bath7.r, bath8.r)
plot(bath0)

# Set coordinate reference system (crs) aka projection for the raster.
# https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:348/html#
# gives the CRS as: "urn:ogc:def:crs:EPSG::4269urn:ogc:def:crs:EPSG::5715"
# https://spatialreference.org/ref/epsg/4269/
# 4269 = horizontal CRS; proj4 = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
# https://spatialreference.org/ref/epsg/5715/
# 5715 = vertical CRS; proj4 = ""
crs(bath0) <- CRS('+init=EPSG:4269')

# save this raster file.
# writeRaster(x = bath0, 
#                     filename = 'G:/My Drive/Professional/OSU/Chinook-Hake Bycatch/Big Files for Git/From R/crm_bathymetry_orig_extent.tif', 
#                     overwrite=T)

# Make new column in obs_dat with extracted bottom depth values for each haul.
obs_dat$bottom_depth_m_CRM <- -1* raster::extract(x = bath0,  # multiply by 1 to get depth as positive values.
                                                  y = obs_sf, # has to be a spatial vector object with geometry! obs_dat returns an error.
                                                  method = 'bilinear')

obs_sf$bottom_depth_m_CRM <- -1* raster::extract(x = bath0,  # multiply by 1 to get depth as positive values.
                                                 y = obs_sf, # has to be a spatial vector object with geometry! obs_dat returns an error.
                                                 method = 'bilinear')


# 7 - Final data updates ----

#### Edits to the dataset to address issues

obs_final <- obs_dat

# LOCATION
# N=1 haul caught overland in BC Canada -> DROP IT (same as PS)
obs_final <- filter(obs_final, unique_haul_id != "98126338352005")

# TOW DURATION
# N=31 hauls with NAs for tow duration -> FIX IT by calculating from deployment and retrieval dates and dividing by half.

# First, filter obs_final to get those rows with NAs in duration, then replace the duration column with the new values we calculated above, then add back into the obs_final (after filtering those same NA values)
new_dur <- obs_final %>%
  filter(is.na(duration_in_min)) %>%
  dplyr::select(unique_haul_id, duration_in_min, chinook_count, deployment_date, retrieval_date) %>%
  mutate(duration2 = retrieval_date - deployment_date) %>% # this returns a difftime class <drtn>
  mutate(duration2 = as.character(duration2)) %>%          # need to then convert to character to drop the "mins" attached
  mutate(duration2 = as.numeric(duration2))                # then can convert to numeric to get minutes as a <dbl> class which will match the duration_in_min column.
new_dur
# After discussing with observer program lead, these new duration values aren't accurate because these nets were not in correct fishing configuration.
# The proper fishing duration must be less than the estimated total duration. We estimate duration of proper fishing configuration is 1/2 total deployment time.

obs_final <- obs_final %>%
  filter(is.na(duration_in_min)) %>%
  mutate(duration_in_min = new_dur$duration2/2) %>% # Give NA durations a new value: 1/2 of total deployment time.
  rbind(filter(obs_final, !is.na(duration_in_min)))

# N=8 hauls with tow duration = 0 -> Observer program lead says these are fouled nets, so drop them.
obs_final <- obs_final %>% 
  filter(duration_in_min != 0)

# Make very short tow durations (< 10 minutes) all 10 minutes long. Observer program lead says tows shorter than 10 mins are probably not realistic and due to human error in recording times.
obs_final <- obs_final %>% 
  filter(duration_in_min < 10) %>%  # N = 153 hauls with durations < 10 minutes.
  mutate(duration_in_min = 10) %>%
  rbind(filter(obs_final, !duration_in_min < 10))


# FISHING DEPTHS
# N=0 hauls with NAs
# N=5 hauls with 0 m depths -> Observer program lead says these are fouled nets, so drop them.

obs_final <- obs_final %>% 
  filter(fishing_fa_to_m != 0)

# BOTTOM DEPTHS
# N=82 hauls with NAs for bottom depths -> FIX IT by using bottom depth value from CRM raster dataset.
obs_final <- obs_final %>% 
  filter(is.na(bottom_depth_fathoms)) %>% 
  mutate(bottom_fa_to_m = bottom_depth_m_CRM) %>% 
  rbind(filter(obs_final, !is.na(bottom_depth_fathoms)))


# Check for missing data in all columns
obs_final %>%
  summarise_all(funs(sum(is.na(.))))
# Good! there are 11 NAs in bottom depth_m_CRM (which doesn't matter to us) and the same 82 in bottom_depth_fathoms because I replaced values in meters in the bottom_fa_to_m column.

# 8 - Save final cleaned file ----

#### SAVE final obs_dat data.

saveRDS(obs_final, str_c(in_drive, "Saved Files/data_by_haul_v1.rds"))


