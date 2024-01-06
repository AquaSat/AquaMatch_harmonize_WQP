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
  "3_harmonize.R",
  "create_bookdown.R"))

# The list of targets/steps
config_targets <- list(
  
  # Targets imported from the previous pipeline: ----------------------------
  
  # The local directory where the first {targets} pipeline is located (i.e.,
  # the pipeline that runs the download step)
  tar_target(name = p0_AquaMatch_download_WQP_directory,
             command = "../AquaMatch_download_WQP/"),
  
  # Confirm the presence of the {targets} WQP download pipeline
  tar_target(name = p0_AquaMatch_download_WQP_confirm,
             command = if(!dir.exists(p0_AquaMatch_download_WQP_directory)) {
               # Throw an error if the pipeline does not exist
               stop("The WQP download pipeline is not at the specified location.")
             },
             # The presence of the first pipeline is necessary for this one to run
             error = "stop"),
  
  # Retrieve links to data from Google Drive
  tar_file_read(p1_wqp_params_link,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "1_inventory/out/p1_wqp_params_out_link.csv"),
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_wqp_inventory_aoi_link,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "1_inventory/out/p1_wqp_inventory_aoi_out_link.csv"),
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_global_grid_link,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "1_inventory/out/p1_global_grid_out_link.csv"),
                read = read_csv(file = !!.x)),
  
  tar_file_read(p1_char_names_crosswalk_link,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "1_inventory/out/p1_char_names_crosswalk_out_link.csv"),
                read = read_csv(file = !!.x)),
  
  tar_file_read(p2_site_counts_link,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "2_download/out/p2_site_counts_out_link.csv"),
                read = read_csv(file = !!.x)),
  
  tar_file_read(p2_wqp_data_aoi_out_links,
                command = paste0(p0_AquaMatch_download_WQP_directory,
                                 "2_download/out/p2_wqp_data_aoi_out_links.csv"),
                read = read_csv(file = !!.x)),
  
  # Download data sets of interest:
  tar_target(p1_wqp_params,
             retrieve_data(link_table = p1_wqp_params_link,
                           folder_pattern = "1_inventory/out/"),
             packages = c("tidyverse", "googledrive")),
  
  tar_target(p1_wqp_inventory_aoi,
             retrieve_data(link_table = p1_wqp_inventory_aoi_link,
                           folder_pattern = "1_inventory/out/"),
             packages = c("tidyverse", "googledrive")),
  
  tar_target(p1_global_grid,
             retrieve_data(link_table = p1_global_grid_link,
                           folder_pattern = "1_inventory/out/"),
             packages = c("tidyverse", "googledrive")),
  
  tar_target(p1_char_names_crosswalk,
             retrieve_data(link_table = p1_char_names_crosswalk_link,
                           folder_pattern = "1_inventory/out/"),
             packages = c("tidyverse", "googledrive")),
  
  tar_target(p2_site_counts,
             retrieve_data(link_table = p2_site_counts_link,
                           folder_pattern = "2_download/out/"),
             packages = c("tidyverse", "googledrive")),
  
  tar_target(p2_wqp_data_aoi_chl,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "chlorophyll"),
             format = "feather",
             packages = c("tidyverse", "googledrive", "feather")),
  
  tar_target(p2_wqp_data_aoi_doc,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "doc"),
             format = "feather",
             packages = c("tidyverse", "googledrive", "feather")),
  
  tar_target(p2_wqp_data_aoi_sdd,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "sdd"),
             format = "feather",
             packages = c("tidyverse", "googledrive", "feather")),
  
  tar_target(p2_wqp_data_aoi_tss,
             retrieve_param_data(link_table = p2_wqp_data_aoi_out_links,
                                 parameter_string = "tss"),
             format = "feather",
             packages = c("tidyverse", "googledrive", "feather"))
  
)


# Full targets list
c(config_targets,
  p3_targets_list,
  bookdown_targets_list)

