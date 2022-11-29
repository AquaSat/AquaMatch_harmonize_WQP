library(targets)

library(tidyverse)
library(dataRetrieval)

wqp_state_codes = tar_read(state_codes)
# value                 name
# 1  US:00          Unspecified
# 2  US:01              Alabama
# 3  US:02               Alaska
# 4  US:04              Arizona
wqp_states = tar_read(wqp_states)
# [1] "Alabama"              "Alaska"               "Arizona"              "Arkansas"
wqp_codes = tar_read(wqp_codes)
# $tss
# [1] "Total suspended solids"                 "Suspended sediment concentration (SSC)"
# [3] "Suspended Sediment Concentration (SSC)" "Total Suspended Particulate Matter"    
# [5] "Fixed suspended solids"                
# 
# $chlorophyll
# [1] "Chlorophyll"                                 "Chlorophyll A"                              


#############

# inventory_wqp <- function(ind_file, wqp_state_codes, wqp_states, wqp_codes) {

# Convert states list to FIPS list
state_codes <- wqp_state_codes %>%
  filter(name %in% wqp_states) %>%
  pull(value)

# Identify available constituent sets
constituents <- names(wqp_codes$characteristicName)

# Prepare the args to whatWQPdata. All arguments will be the same every time
# except characteristicName, which we'll loop through to get separate counts
# for each
wqp_args <- list(
  statecode = state_codes,
  siteType = wqp_codes$siteType,
  # To be updated each time through loop:
  characteristicName = NA,
  sampleMedia = wqp_codes$sampleMedia
  # We'd include dates, but they get ignored by the service behind whatWQPdata
)

# This works...
whatWQPdata(statecode = "US:08",
            siteType = c("Lake, Reservoir, Impoundment", "Stream", "Estuary", "Facility"),
            sampleMedia = c("Water", "water"))
















# Loop over the constituents, getting rows for each
sample_time <- system.time({
  samples <- bind_rows(lapply(constituents, function(constituent) {
    
    message(Sys.time(), ": getting inventory for ", constituent)
    
    wqp_args$characteristicName <- wqp_codes$characteristicName[[constituent]]
    
    tryCatch({
      wqp_wdat <- do.call(whatWQPdata, wqp_args)
      mutate(wqp_wdat, constituent = constituent)
    }, error = function(e) {
      # Keep going IFF the only error was that there weren't any matching sites
      if(grepl("arguments imply differing number of rows", e$message)) {
        NULL
      } else {
        stop(e)
      }
    })
  }))
})
message(sprintf("sample inventory complete, required %0.0f seconds", sample_time[["elapsed"]]))

# Get additional site information
message(Sys.time(), ": getting additional site data")

site_time <- system.time({
  
  wqp_site_args <- wqp_args[names(wqp_args) != "characteristicName"]
  sites <- do.call(whatWQPsites, wqp_site_args)
  
})
message(sprintf("site inventory complete, required %0.0f seconds", site_time[["elapsed"]]))

# merge constituent info with site info
wqp_info <- left_join(
  x = samples %>%
    select(Constituent = constituent, MonitoringLocationIdentifier, resultCount,
           MonitoringLocationName, MonitoringLocationTypeName,
           ResolvedMonitoringLocationTypeName),
  y = sites %>%
    select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure,
           HorizontalCoordinateReferenceSystemDatumName, HUCEightDigitCode,
           CountryCode, StateCode, CountyCode, OrganizationFormalName) %>%
    # Replace lat/lon numeric flags with NAs
    mutate(LatitudeMeasure = ifelse(LatitudeMeasure < 10, NA, LatitudeMeasure),
           LongitudeMeasure = ifelse(LongitudeMeasure > -10, NA, LongitudeMeasure)),
  by = "MonitoringLocationIdentifier")

# Write the data file and the indicator file
# data_file <- as_data_file(ind_file)
# feather::write_feather(wqp_info, path=data_file)
# sc_indicate(ind_file, data_file=data_file)
# invisible()

return(wqp_info)
}


