# This script groups raw data records from the Water Quality Portal by HUC 4
# polygons for mapping counts. It's run outside of aquasat_version_report.Rmd
# as a means of running the large spatial computations separately

library(tidyverse)
library(sf)
library(tigris)
library(tictoc)
library(feather)

# Read in raw WQP data from the original 2019 AquaSat
raw_aq <- map_df(.x = list("data/wqp_raw/all_raw_chlorophyll.csv",
                           "data/wqp_raw/all_raw_doc.csv",
                           "data/wqp_raw/all_raw_secchi.csv",
                           "data/wqp_raw/all_raw_tss.csv"),
                 .f = read_csv) %>%
  # Keep only certain years
  filter(year(ActivityStartDate) %in% 1984:2019)

# Raw data from AquaSat/Match v2 project
raw_aq2 <- read_feather(path = "2_download/out/p2_wqp_data_aoi_20230525.feather") %>%
  filter(year(ActivityStartDate) %in% 1984:2019)

# Get metadata for WQP locations
wqp_meta <- read_csv("data/us_wqp_station.csv")

# Identify chars present in both version of AquaSat
aq_chars <- unique(raw_aq$CharacteristicName)
aq2_chars <- unique(raw_aq2$CharacteristicName)

# Which CharacteristicNames do they share?
shared_chars <- dplyr::intersect(x = aq_chars, y = aq2_chars)

# Keep only shared CharacteristicNames:
raw_aq <- raw_aq %>%
  filter(CharacteristicName %in% shared_chars)

raw_aq2 <- raw_aq2 %>%
  filter(CharacteristicName %in% shared_chars)

# Confirm CharacteristicNames match
all( sort(unique(raw_aq$CharacteristicName)) == sort(unique(raw_aq2$CharacteristicName)) )

# If the start time doesn't include a timestamp, make it 12:00:00
raw_aq <- raw_aq %>%
  mutate(ActivityStartDateTime = as.character(ActivityStartDateTime),
         ActivityStartDateTime = if_else(
           # 10 characters = date only (e.g., 1988-08-14)
           condition = nchar(ActivityStartDateTime) == 10,
           # If date only, add time = noon
           true = paste0(ActivityStartDateTime, " 12:00:00"),
           false = ActivityStartDateTime
         ),
         ActivityStartDateTime = ymd_hms(ActivityStartDateTime))

raw_aq2 <- raw_aq2 %>%
  mutate(ActivityStartDateTime = as.character(ActivityStartDateTime),
         ActivityStartDateTime = if_else(
           # 10 characters = date only (e.g., 1988-08-14)
           condition = nchar(ActivityStartDateTime) == 10,
           # If date only, add time = noon
           true = paste0(ActivityStartDateTime, " 12:00:00"),
           false = ActivityStartDateTime
         ),
         ActivityStartDateTime = ymd_hms(ActivityStartDateTime))

# Get site locations but only use ones that aren't NA for location data and don't
# have difficult CRS
aq_site_locs <- wqp_meta %>%
  filter(MonitoringLocationIdentifier %in% unique(raw_aq$MonitoringLocationIdentifier)) %>%
  filter(!is.na(LatitudeMeasure) &
           !is.na(LongitudeMeasure) &
           !is.na(HorizontalCoordinateReferenceSystemDatumName) &
           !(HorizontalCoordinateReferenceSystemDatumName %in%
               c("UNKWN", "OTHER", "GUAM", "OLDHI", "AMSMA", "Unknown"))
  )

states_sf <- states(progress_bar = FALSE) %>%
  st_transform(crs = 9311)

# Split up the raw data by CRS and transform to the desired one, then recombine
# into a single sf object of a single CRS
aq_sf <- inner_join(x = raw_aq, 
                    y = aq_site_locs %>%
                      select(MonitoringLocationIdentifier, 
                             OrganizationFormalName,
                             OrganizationIdentifier, 
                             HorizontalCoordinateReferenceSystemDatumName, 
                             LatitudeMeasure, LongitudeMeasure),
                    by = c("MonitoringLocationIdentifier",
                           "OrganizationFormalName",
                           "OrganizationIdentifier")) %>%
  split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
  map_df(.x = .,
         .f = ~ .x %>%
           st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"),
                    crs = unique(.x$HorizontalCoordinateReferenceSystemDatumName)) %>%
           st_transform(crs = 4326)) %>%
  st_transform(crs = 9311) %>%
  rownames_to_column()

# Make the sf object smaller in hopes of makes computation more efficient
aq_sf_reduce <- aq_sf %>%
  select(rowname)

# Remove big extra stuff
rm(raw_aq2, aq2_chars)
gc()

wbd_4 <- st_read(dsn = "data/WBD_National_GDB/WBD_National_GDB.gdb",
                 layer = "WBDHU4") %>%
  st_transform(crs = 9311)

wbd_4_conus <- wbd_4[states_sf %>%
                       filter(!NAME %in% c(#"Alaska",
                         "Hawaii", "American Samoa",
                         "Guam", "Puerto Rico",
                         "United States Virgin Islands",
                         "Commonwealth of the Northern Mariana Islands")), ] %>%
  select(states, huc4, name, shape)

# This next part is LENGTHY, like 30-40 hours, which is why it's commented out.
# The overall idea is to iteratively reduce the size of the sf object so that
# each HUC is processed separately and then removed from the sf object that was
# previously bottlenecking the computation.
tic()

sf_result <- list()

# For each HUC in the sf object...
for (i in seq_along(split(wbd_4_conus, rownames(wbd_4_conus)))) {
  
  # If it's the first iteration, then just use the normal object
  if (i == 1) {
    temp_aq_sf <- aq_sf_reduce
    # If it's NOT the first iteration then use a temporary version of the object
    # that has all previously used HUCs removed
  } else {
    temp_aq_sf <- anti_join(
      x = aq_sf_reduce,
      y = as.data.frame(do.call("rbind", sf_result)),
      by = c("rowname"))
  }
  
  # Get the HUC shape for this iteration
  wbd_subset <- split(wbd_4_conus, rownames(wbd_4_conus))[[i]]
  
  # Subset the data to only those records falling inside the HUC and tag them
  # with the HUC ID
  record_subset <- temp_aq_sf[wbd_subset, ] %>%
    mutate(huc4 = wbd_subset$huc4)
  
  # Store the output
  sf_result[[i]] <- record_subset
  
  # Progress message
  print(paste0(i, " complete"))
}

# 30+ hours (30.3, 40.8...)
toc()

# Bind all records with HUC tags into a single sf object
bind_rows(sf_result)

# Export
write_sf(obj = bind_rows(sf_result), 
         dsn = "data/aq_point_tagged_huc4.shp",
         append = FALSE)

# Count the number of records per HUC 4 and export
bind_rows(sf_result) %>%
  as.data.frame() %>%
  count(huc4) %>%
  write_csv("data/huc4_counts.csv")
