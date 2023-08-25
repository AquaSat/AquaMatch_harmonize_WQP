# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse"),
  error = "continue"
)

# Run the R scripts with custom functions:
tar_source(files = c(
  "1_inventory.R",
  "2_download.R",
  "3_harmonize.R",
  "create_bookdown.R"))

# The list of targets/steps
config_targets <- list(
  
  # WQP config --------------------------------------------------------------
  
  # Things that often used to be YAMLs, and which probably should be again in 
  # the future
  
  # Date range of interest
  tar_target(p0_wq_dates,
             list(
               start_date = "1970-01-01",
               end_date = Sys.Date()
             )),
  
  # Define which parameter groups (and CharacteristicNames) to return from WQP. 
  # Different options for parameter groups are represented in the first level of 
  # 1_inventory/cfg/wqp_codes.yml. The yml file can be edited to 
  # omit characteristic names or include others, to change top-level parameter names,
  # or to customize parameter groupings. 
  tar_target(p0_param_groups_select,
             c(# "alkalinity", "cdom", "depth", "nitrogen",
               # "ssc", "temperature", "phosphorus", "poc", silica",
               "chlorophyll", "doc", "secchi", "tss"
             )),
  
  
  # WQP inventory -----------------------------------------------------------
  
  # Specify arguments to WQP queries
  # see https://www.waterqualitydata.us/webservices_documentation for more information 
  tar_target(p0_wqp_args,
             list(sampleMedia = c("Water", "water"),
                  siteType = c("Lake, Reservoir, Impoundment",
                               "Stream",
                               "Estuary"),
                  # Return sites with at least one data record
                  minresults = 1, 
                  startDateLo = p0_wq_dates$start_date,
                  startDateHi = p0_wq_dates$end_date))
)


# Full targets list
c(config_targets,
  p1_targets_list, p2_targets_list, p3_targets_list,
  bookdown_targets_list)

