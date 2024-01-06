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
                  startDateHi = p0_wq_dates$end_date)),
  
  
  # Targets imported from the previous pipeline: ----------------------------
  
  # Retrieve links to data from Google Drive
  tar_file_read(p1_wqp_params_link,
                command = "../AquaMatch_download_WQP/1_inventory/out/p1_wqp_params_out_link.csv",
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_wqp_inventory_aoi_link,
                command = "../AquaMatch_download_WQP/1_inventory/out/p1_wqp_inventory_aoi_out_link.csv",
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_global_grid_link,
                command = "../AquaMatch_download_WQP/1_inventory/out/p1_global_grid_out_link.csv",
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_char_names_crosswalk_link,
                command = "../AquaMatch_download_WQP/1_inventory/out/p1_char_names_crosswalk_out_link.csv",
                read = read_csv(file = !!.x)),
  
  tar_file_read(p2_site_counts_link,
                command = "../AquaMatch_download_WQP/2_download/out/p2_site_counts_out_link.csv",
                read = read_csv(file = !!.x)),
  
  tar_file_read(p2_wqp_data_aoi_out_links,
                command = "../AquaMatch_download_WQP/2_download/out/p2_wqp_data_aoi_out_links.csv",
                read = read_csv(file = !!.x)),
  
  # Download data sets of interest:
  tar_target(p1_wqp_params,
             retrieve_data(link_table = p1_wqp_params_link,
                           folder_pattern = "1_inventory/out/")),
  
  tar_target(p1_wqp_inventory_aoi,
             retrieve_data(link_table = p1_wqp_inventory_aoi_link,
                           folder_pattern = "1_inventory/out/")),
  
  tar_target(p1_global_grid,
             retrieve_data(link_table = p1_global_grid_link,
                           folder_pattern = "1_inventory/out/")),
  
  tar_target(p1_char_names_crosswalk,
             retrieve_data(link_table = p1_char_names_crosswalk_link,
                           folder_pattern = "1_inventory/out/")),
  
  tar_target(p2_site_counts,
             retrieve_data(link_table = p2_site_counts_link,
                           folder_pattern = "2_download/out/")),
  
  tar_target(p2_wqp_data_aoi_chl,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "chlorophyll"),
             format = "feather"),
  
  tar_target(p2_wqp_data_aoi_doc,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "doc"),
             format = "feather"),
  
  tar_target(p2_wqp_data_aoi_sdd,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "sdd"),
             format = "feather"),
  
  tar_target(p2_wqp_data_aoi_tss,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "tss"),
             format = "feather")
  
)


# Full targets list
c(config_targets,
  p3_targets_list,
  bookdown_targets_list)

