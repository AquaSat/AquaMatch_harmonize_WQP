library(tidyverse)
library(dataRetrieval)


# Load necessary data structures ------------------------------------------

wqp_state_codes <- structure(list(value = c("US:00", "US:01", "US:02", "US:04", 
                                        "US:05", "US:06", "US:08", "US:09", "US:10", "US:11", "US:12", 
                                        "US:13", "US:15", "US:16", "US:17", "US:18", "US:19", "US:20", 
                                        "US:21", "US:22", "US:23", "US:24", "US:25", "US:26", "US:27", 
                                        "US:28", "US:29", "US:30", "US:31", "US:32", "US:33", "US:34", 
                                        "US:35", "US:36", "US:37", "US:38", "US:39", "US:40", "US:41", 
                                        "US:42", "US:44", "US:45", "US:46", "US:47", "US:48", "US:49", 
                                        "US:50", "US:51", "US:53", "US:54", "US:55", "US:56"),
                              name = c("Unspecified", 
                                       "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                       "Connecticut", "Delaware", "District of Columbia", "Florida", 
                                       "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                                       "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                                       "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                       "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                       "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                       "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                                       "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                                       "West Virginia", "Wisconsin", "Wyoming")),
                         row.names = c(NA, 
                                       -52L),
                         class = "data.frame")

wqp_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                "Connecticut", "Delaware", "District of Columbia", "Florida", 
                "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                "West Virginia", "Wisconsin", "Wyoming")

wqp_codes <- list(
  characteristicName = list(
    tss = c("Total suspended solids", 
            "Suspended sediment concentration (SSC)", "Suspended Sediment Concentration (SSC)", 
            "Total Suspended Particulate Matter", "Fixed suspended solids"),
    chlorophyll = c("Chlorophyll", "Chlorophyll A", "Chlorophyll a", 
                    "Chlorophyll a (probe relative fluorescence)", "Chlorophyll a (probe)", 
                    "Chlorophyll a - Periphyton (attached)", "Chlorophyll a - Phytoplankton (suspended)", 
                    "Chlorophyll a, corrected for pheophytin", "Chlorophyll a, free of pheophytin", 
                    "Chlorophyll a, uncorrected for pheophytin", "Chlorophyll b", 
                    "Chlorophyll c", "Chlorophyll/Pheophytin ratio"),
    secchi = c("Depth, Secchi disk depth", 
               "Depth, Secchi disk depth (choice list)", "Secchi Reading Condition (choice list)", 
               "Secchi depth", "Water transparency, Secchi disc"),
    cdom = "Colored dissolved organic matter (CDOM)", 
    doc = c("Organic carbon", "Total carbon", "Hydrophilic fraction of organic carbon", 
            "Non-purgeable Organic Carbon (NPOC)")),
  sampleMedia = c("Water", 
                  "water"),
  siteType = c("Lake, Reservoir, Impoundment", "Stream", 
               "Estuary", "Facility")
)


# Do some data prep -------------------------------------------------------

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


# WQP data info calls -----------------------------------------------------

# The goal is to iterate through the WQP codes in the loop that's below `test`
# Here's an example of something that works (doesn't include WQP codes though):
test_co <- whatWQPdata(statecode = "US:08",
            siteType = c("Lake, Reservoir, Impoundment", "Stream", "Estuary", "Facility"),
            characteristicName = c("Total suspended solids", 
                                   "Suspended sediment concentration (SSC)",
                                   "Suspended Sediment Concentration (SSC)", 
                                   "Total Suspended Particulate Matter",
                                   "Fixed suspended solids"),
            sampleMedia = c("Water", "water"))

test_al <- whatWQPdata(statecode = "US:01",
                       siteType = c("Lake, Reservoir, Impoundment", "Stream", "Estuary", "Facility"),
                       characteristicName = c("Total suspended solids", 
                                              "Suspended sediment concentration (SSC)",
                                              "Suspended Sediment Concentration (SSC)", 
                                              "Total Suspended Particulate Matter",
                                              "Fixed suspended solids"),
                       sampleMedia = c("Water", "water"))

head(test)

# Loop over the constituents, getting rows for each. With tryCatch I thought this
# would continue but the 504 error seems to be wrecking the attempts?
sample_time <- system.time({
  samples <- bind_rows(lapply(constituents, function(constituent) {
    
    message(Sys.time(), ": getting inventory for ", constituent)
    
    wqp_args$characteristicName <- wqp_codes$characteristicName[[constituent]]
    
    tryCatch({
      print(do.call(whatWQPdata, wqp_args))
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

safe_wqp_inv <- safely(.f = ~{
  
  message(Sys.time(), ": getting inventory for ", .x)
  
  wqp_args$characteristicName <- wqp_codes$characteristicName[[.x]]
  
  wqp_wdat <- whatWQPdata(wqp_args)
  
  # Returned
  mutate(wqp_wdat, constituent = .x)
  
})

# This only fails for TSS...why?
test_map_wqp <- map(.x = constituents,
                    .f = ~ safe_wqp_inv(.x))























