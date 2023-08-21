# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse"),
  # error = "continue"#,
  workspace_on_error = TRUE
)

# Run the R scripts with custom functions:
tar_source(files = c(
  "1_inventory.R",
  "2_download.R",
  "3_harmonize.R",
  "create_bookdown.R"))

# The list of targets/steps (these steps from MRB)
config_targets <- list(
  
  # WQP config --------------------------------------------------------------
  
  # Things that often used to be YAMLs, and which probably should be again in 
  # the future. For right now I'm putting them as targets so I can conceptualize
  # the workflow components more easily
  
  # Date range of interest
  tar_target(p0_wq_dates,
             list(
               start_date = "1970-01-01",
               end_date = Sys.Date()
             )),
  
  # Define which parameter groups (and CharacteristicNames) to return from WQP. 
  # Different options for parameter groups are represented in the first level of 
  # 1_inventory/cfg/wqp_codes.yml. This yml file is meant to provide a starting 
  # place for an analysis and does not represent a definitive list of characteristic 
  # names. Which characteristic names to include for any given parameter group may 
  # change depending on the user or application, so the yml file can be edited to 
  # omit characteristic names or include others, to change top-level parameter names,
  # or to customize parameter groupings. 
  tar_target(p0_param_groups_select,
             c(#"alkalinity", "cdom", "depth", "nitrogen",
               "chlorophyll", "doc", "secchi", "tss"#,
               # "ssc", "temperature", "phosphorus", "poc", silica"
               )),
  
  
  # WQP inventory -----------------------------------------------------------
  
  # Specify arguments to WQP queries
  # see https://www.waterqualitydata.us/webservices_documentation for more information 
  tar_target(p0_wqp_args,
             list(sampleMedia = c("Water","water"),
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

