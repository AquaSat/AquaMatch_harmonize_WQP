# A function to repare a data frame that maps state names to the FIPS codes
# used by WQP
get_wqp_state_codes <- function() {
  
  states_df <- stateCd %>%
    filter(STATE < 60) %>%
    mutate(
      value = paste0("US:", STATE),
      name = STATE_NAME) %>%
    select(value, name)
  
  return(states_df)
  
}

# A function to acquire a data frame of site x constituent information, with
# counts of observations per site-constituent combination and all the site
# metadata that looks useful
# inventory_wqp <- function(ind_file, wqp_state_codes, wqp_states, wqp_codes) {
#   
#   # Convert states list to FIPS list
#   state_codes <- wqp_state_codes %>%
#     filter(name %in% wqp_states) %>%
#     pull(value)
#   
#   # Identify available constituent sets
#   constituents <- names(wqp_codes$characteristicName)
#   
#   # Prepare the args to whatWQPdata. All arguments will be the same every time
#   # except characteristicName, which we'll loop through to get separate counts
#   # for each
#   wqp_args <- list(
#     statecode = state_codes,
#     siteType = wqp_codes$siteType,
#     # To be updated each time through loop:
#     characteristicName = NA,
#     sampleMedia = wqp_codes$sampleMedia
#     # We'd include dates, but they get ignored by the service behind whatWQPdata
#   )
#   
#   # Loop over the constituents, getting rows for each
#   sample_time <- system.time({
#     samples <- bind_rows(lapply(constituents, function(constituent) {
#       
#       message(Sys.time(), ": getting inventory for ", constituent)
#       
#       wqp_args$characteristicName <- wqp_codes$characteristicName[[constituent]]
#       
#       tryCatch({
#         wqp_wdat <- do.call(whatWQPdata, wqp_args)
#         mutate(wqp_wdat, constituent=constituent)
#       }, error = function(e) {
#         # Keep going IFF the only error was that there weren't any matching sites
#         if(grepl("arguments imply differing number of rows", e$message)) {
#           NULL
#         } else {
#           stop(e)
#         }
#       })
#     }))
#   })
#   message(sprintf("sample inventory complete, required %0.0f seconds", sample_time[["elapsed"]]))
#   
#   # Get additional site information
#   message(Sys.time(), ": getting additional site data")
#   
#   site_time <- system.time({
#     
#     wqp_site_args <- wqp_args[names(wqp_args) != "characteristicName"]
#     sites <- do.call(whatWQPsites, wqp_site_args)
#     
#   })
#   message(sprintf("site inventory complete, required %0.0f seconds", site_time[["elapsed"]]))
#   
#   # merge constituent info with site info
#   wqp_info <- left_join(
#     x = samples %>%
#       select(Constituent = constituent, MonitoringLocationIdentifier, resultCount,
#              MonitoringLocationName, MonitoringLocationTypeName,
#              ResolvedMonitoringLocationTypeName),
#     y = sites %>%
#       select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure,
#              HorizontalCoordinateReferenceSystemDatumName, HUCEightDigitCode,
#              CountryCode, StateCode, CountyCode, OrganizationFormalName) %>%
#       # Replace lat/lon numeric flags with NAs
#       mutate(LatitudeMeasure = ifelse(LatitudeMeasure < 10, NA, LatitudeMeasure),
#              LongitudeMeasure = ifelse(LongitudeMeasure > -10, NA, LongitudeMeasure)),
#     by = "MonitoringLocationIdentifier")
#   
#   # Write the data file and the indicator file
#   # data_file <- as_data_file(ind_file)
#   # feather::write_feather(wqp_info, path=data_file)
#   # sc_indicate(ind_file, data_file=data_file)
#   # invisible()
#   
#   return(wqp_info)
# }


# A function to pull the parameter codes from the USGS website and save them
# as a table for use in the cleaning process
get_p_codes <- function(){
  
  # Scrape URL
  site_url <- "https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
  
  # Pull table from website
  code_table <- read_html(site_url) %>%
    html_node("table") %>%
    html_table()
  
  # Get parameter codes from table
  p_codes <- code_table %>%
    clean_names() %>%
    mutate(parm_cd = str_pad(string = as.character(parameter_code), 
                             width = 5,
                             pad = "0"))
  
  return(p_codes)
}

# A function that allows you to knit an Rmd and also return a value from that
# Rmd back to the pipeline. Based on:
# https://stackoverflow.com/questions/58315771/can-rmarkdown-return-a-value-to-a-target
# Targets does not allow this otherwise:
# https://github.com/ropensci/tarchetypes/discussions/125
render_and_return <- function(input_var, input_file, output_file) {
  # Knit the Rmd:
  rmarkdown::render(input = input_file, output_file = output_file,
                    quiet = TRUE)
  
  # Variable to be returned. Assigned in the report:
  return_value
}

# Function for making a nice table that gets a summary of units and the number 
# of observations with that unit code. (Adapted from AquaSat)
unit_kable <- function(data){
  
  data %>%
    group_by(units) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    kable(., "html", caption = "All  parameter and unit combinations") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}

# Function for making a nice table that gets a summary of units and the number 
# of observations with that analytical method. (Adapted from AquaSat)
analytical_kable <- function(data){
  
  data %>%
    group_by(analytical_method) %>%
    summarize(count = n()) %>% 
    arrange(desc(count)) %>%
    kable(., "html", caption = "All analytical methods and their count") %>%
    kable_styling() %>%
    scroll_box(width = "600px", height = "400px")
  
}

# Function for making a nice table that gets a summary of nonsensical units
# and the number of observations with that analytical method.
# (Adapted from AquaSat)
unit_disharmony <- function(data, lookup){
  
  data %>%
    anti_join(x = ., y = lookup, by = "units") %>%
    group_by(units) %>%
    summarize(count = n())  %>%
    kable(.,"html", caption = "The following measurements
          were dropped because the units do not make sense") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}
