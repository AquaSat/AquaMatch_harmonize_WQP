# Created by use_targets()

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse"),
  memory = "transient",
  garbage_collection = TRUE,
  seed = 1
)

# Run the R scripts with custom functions:
tar_source(files = c(
  "src/",
  "3a_harmonize.R",
  "3b_harmonize_chla.R",
  "3c_harmonize_doc.R",
  "3d_harmonize_sdd.R",
  "3e_harmonize_tss.R",
  "create_bookdown.R"))

# The list of targets/steps
config_targets <- list(
  
  # General config ----------------------------------------------------------
  
  # Grab configuration information for the workflow run (config.yml)
  tar_target(
    name = p0_harmonization_config,
    # The config package does not like to be used with library()
    command = config::get(config = "admin_update"),
    cue = tar_cue("always")
  ),
  
  # Set Google Drive directory paths for parameter objects
  tar_target(
    name = p0_chl_output_path,
    command = paste0(p0_harmonization_config$drive_project_folder,
                     "chlorophyll/")
  ),
  
  tar_target(
    name = p0_doc_output_path,
    command = paste0(p0_harmonization_config$drive_project_folder,
                     "doc/")
  ), 
  
  tar_target(
    name = p0_sdd_output_path,
    command = paste0(p0_harmonization_config$drive_project_folder,
                     "sdd/")
  ),
  
  tar_target(
    name = p0_tss_output_path,
    command = paste0(p0_harmonization_config$drive_project_folder,
                     "tss/")
  ),
  
  tar_target(
    name = p0_tc_output_path,
    command = paste0(p0_harmonization_config$drive_project_folder,
                     "true_color/")
  ), 
  
  # Check for Google Drive folder for harmonized file output, create it if it
  # doesn't exist
  tar_target(
    name = p0_check_drive_parent_folder,
    command = tryCatch({
      drive_auth(p0_harmonization_config$google_email)
      drive_ls(p0_harmonization_config$drive_project_folder)
    }, error = function(e) {
      drive_mkdir(str_sub(p0_harmonization_config$drive_project_folder, 1, -2))  
    }),
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  # Check for chlorophyll subfolder, create if not present
  tar_target(
    name = p0_check_chl_drive,
    command = {
      p0_check_drive_parent_folder
      tryCatch({
        drive_auth(p0_harmonization_config$google_email)
        drive_ls(p0_chl_output_path)
      }, error = function(e) {
        # if the outpath doesn't exist, create it along with a "stable" subfolder
        drive_mkdir(name = "chlorophyll",
                    path = p0_harmonization_config$drive_project_folder)
        drive_mkdir(name = "stable",
                    path = paste0(p0_harmonization_config$drive_project_folder,
                                  "chlorophyll"))
      })
    },
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  # Check for doc subfolder, create if not present
  tar_target(
    name = p0_check_doc_drive,
    command = {
      p0_check_drive_parent_folder
      tryCatch({
        drive_auth(p0_harmonization_config$google_email)
        drive_ls(p0_doc_output_path)
      }, error = function(e) {
        # if the outpath doesn't exist, create it along with a "stable" subfolder
        drive_mkdir(name = "doc",
                    path = p0_harmonization_config$drive_project_folder)
        drive_mkdir(name = "stable",
                    path = paste0(p0_harmonization_config$drive_project_folder,
                                  "doc"))
      })
    },
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  # Check for sdd subfolder, create if not present
  tar_target(
    name = p0_check_sdd_drive,
    command = {
      p0_check_drive_parent_folder
      tryCatch({
        drive_auth(p0_harmonization_config$google_email)
        drive_ls(p0_sdd_output_path)
      }, error = function(e) {
        # if the outpath doesn't exist, create it along with a "stable" subfolder
        drive_mkdir(name = "sdd",
                    path = p0_harmonization_config$drive_project_folder)
        drive_mkdir(name = "stable",
                    path = paste0(p0_harmonization_config$drive_project_folder,
                                  "sdd"))
      })
    },
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  # Check for tss subfolder, create if not present
  tar_target(
    name = p0_check_tss_drive,
    command = {
      p0_check_drive_parent_folder
      tryCatch({
        drive_auth(p0_harmonization_config$google_email)
        drive_ls(p0_tss_output_path)
      }, error = function(e) {
        # if the outpath doesn't exist, create it along with a "stable" subfolder
        drive_mkdir(name = "tss",
                    path = p0_harmonization_config$drive_project_folder)
        drive_mkdir(name = "stable",
                    path = paste0(p0_harmonization_config$drive_project_folder,
                                  "tss"))
      })
    },
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  # Check for true color subfolder, create if not present
  tar_target(
    name = p0_check_tc_drive,
    command = {
      p0_check_drive_parent_folder
      tryCatch({
        drive_auth(p0_harmonization_config$google_email)
        drive_ls(p0_tc_output_path)
      }, error = function(e) {
        # if the outpath doesn't exist, create it along with a "stable" subfolder
        drive_mkdir(name = "true_color",
                    path = p0_harmonization_config$drive_project_folder)
        drive_mkdir(name = "stable",
                    path = paste0(p0_harmonization_config$drive_project_folder,
                                  "true_color"))
      })
    },
    packages = "googledrive",
    cue = tar_cue("always"),
    error = "stop"
  ),
  
  
  # Import targets from the previous pipeline -------------------------------
  
  # Grab location of the local {targets} WQP download pipeline OR error if
  # the location doesn't exist yet
  tar_target(
    name = p0_AquaMatch_download_WQP_directory,
    command = if(dir.exists(p0_harmonization_config$download_repo_directory)){
      p0_harmonization_config$download_repo_directory
    } else if(!dir.exists(p0_AquaMatch_download_WQP_directory)) {
      # Throw an error if the pipeline does not exist
      stop("The WQP download pipeline is not at the location specified in the 
           config.yml file. Check the location specified as `download_repo_directory`
           in the config.yml file and rerun the pipeline.")
    },
    cue = tar_cue("always")
  ),
  
  
  # Retrieve Drive IDs ------------------------------------------------------
  
  # Google Drive IDs of exported files from the download pipeline
  
  tar_file_read(
    name = p2_general_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/general_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p2_chl_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/chl_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p2_doc_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/doc_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p2_sdd_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/sdd_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p2_tss_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/tss_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p2_tc_drive_ids,
    command = paste0(p0_AquaMatch_download_WQP_directory,
                     "2_download/out/tc_drive_ids.csv"),
    cue = tar_cue("always"),
    read = read_csv(file = !!.x)
  ),
  
  
  # Download files from Google Drive ----------------------------------------
  
  # AOI grid
  tar_target(
    name = p1_global_grid,
    command = retrieve_data(target = "p1_global_grid",
                            id_df = p2_general_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$general_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$general_stable_date),
    packages = c("tidyverse", "googledrive")
  ), 
  
  # CharacteristicNames by param
  # Chl
  tar_target(
    name = p1_wqp_params_chl,
    command = retrieve_data(target = "p1_wqp_params_chl",
                            id_df = p2_chl_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$chl_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # DOC
  tar_target(
    name = p1_wqp_params_doc,
    command = retrieve_data(target = "p1_wqp_params_doc",
                            id_df = p2_doc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$doc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$doc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),  
  
  # SDD
  tar_target(
    name = p1_wqp_params_sdd,
    command = retrieve_data(target = "p1_wqp_params_sdd",
                            id_df = p2_sdd_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$sdd_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$sdd_stable_date),
    packages = c("tidyverse", "googledrive")
  ),  
  
  # TSS
  tar_target(
    name = p1_wqp_params_tss,
    command = retrieve_data(target = "p1_wqp_params_tss",
                            id_df = p2_tss_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tss_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tss_stable_date),
    packages = c("tidyverse", "googledrive")
  ),  
  
  # True color
  tar_target(
    name = p1_wqp_params_tc,
    command = retrieve_data(target = "p1_wqp_params_tc",
                            id_df = p2_tc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tc_stable_date),
    packages = c("tidyverse", "googledrive")
  ), 
  
  # CharacteristicName x Parameter name crosswalk tables
  # Chl
  tar_target(
    name = p1_char_names_crosswalk_chl,
    command = retrieve_data(target = "p1_char_names_crosswalk_chl",
                            id_df = p2_chl_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$chl_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # DOC
  tar_target(
    name = p1_char_names_crosswalk_doc,
    command = retrieve_data(target = "p1_char_names_crosswalk_doc",
                            id_df = p2_doc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$doc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$doc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # SDD
  tar_target(
    name = p1_char_names_crosswalk_sdd,
    command = retrieve_data(target = "p1_char_names_crosswalk_sdd",
                            id_df = p2_sdd_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$sdd_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$sdd_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # TSS
  tar_target(
    name = p1_char_names_crosswalk_tss,
    command = retrieve_data(target = "p1_char_names_crosswalk_tss",
                            id_df = p2_tss_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tss_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tss_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # True color
  tar_target(
    name = p1_char_names_crosswalk_tc,
    command = retrieve_data(target = "p1_char_names_crosswalk_tc",
                            id_df = p2_tc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # Inventory info from WQP
  # Chl
  tar_target(
    name = p1_wqp_inventory_aoi_chl,
    command = retrieve_data(target = "p1_wqp_inventory_aoi_chl",
                            id_df = p2_chl_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$chl_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # DOC
  tar_target(
    name = p1_wqp_inventory_aoi_doc,
    command = retrieve_data(target = "p1_wqp_inventory_aoi_doc",
                            id_df = p2_doc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$doc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$doc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # SDD
  tar_target(
    name = p1_wqp_inventory_aoi_sdd,
    command = retrieve_data(target = "p1_wqp_inventory_aoi_sdd",
                            id_df = p2_sdd_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$sdd_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$sdd_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # TSS
  tar_target(
    name = p1_wqp_inventory_aoi_tss,
    command = retrieve_data(target = "p1_wqp_inventory_aoi_tss",
                            id_df = p2_tss_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tss_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tss_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # True color
  tar_target(
    name = p1_wqp_inventory_aoi_tc,
    command = retrieve_data(target = "p1_wqp_inventory_aoi_tc",
                            id_df = p2_tc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # Site counts
  # Chl
  tar_target(
    name = p2_site_counts_chl,
    command = retrieve_data(target = "p2_site_counts_chl",
                            id_df = p2_chl_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$chl_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # DOC
  tar_target(
    name = p2_site_counts_doc,
    command = retrieve_data(target = "p2_site_counts_doc",
                            id_df = p2_doc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$doc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$doc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # SDD
  tar_target(
    name = p2_site_counts_sdd,
    command = retrieve_data(target = "p2_site_counts_sdd",
                            id_df = p2_sdd_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$sdd_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$sdd_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # TSS
  tar_target(
    name = p2_site_counts_tss,
    command = retrieve_data(target = "p2_site_counts_tss",
                            id_df = p2_tss_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tss_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tss_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # True color
  tar_target(
    name = p2_site_counts_tc,
    command = retrieve_data(target = "p2_site_counts_tc",
                            id_df = p2_tc_drive_ids,
                            local_folder = "3_harmonize/in",
                            stable = p0_harmonization_config$tc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tc_stable_date),
    packages = c("tidyverse", "googledrive")
  ),
  
  # Data from WQP
  # Chl
  tar_target(
    name = p2_wqp_data_aoi_chl,
    command = retrieve_data(target = "p2_wqp_data_aoi_chl_anon",
                            id_df = p2_chl_drive_ids,
                            local_folder = "3_harmonize/in",
                            file_type = ".feather",
                            stable = p0_harmonization_config$chl_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive", "feather"),
    format = "feather"
  ),
  
  # DOC
  tar_target(
    name = p2_wqp_data_aoi_doc,
    command = retrieve_data(target = "p2_wqp_data_aoi_doc_anon",
                            id_df = p2_doc_drive_ids,
                            local_folder = "3_harmonize/in",
                            file_type = ".feather",
                            stable = p0_harmonization_config$doc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$doc_stable_date),
    packages = c("tidyverse", "googledrive", "feather"),
    format = "feather"
  ),
  
  # SDD
  tar_target(
    name = p2_wqp_data_aoi_sdd,
    command = retrieve_data(target = "p2_wqp_data_aoi_sdd_anon",
                            id_df = p2_sdd_drive_ids,
                            local_folder = "3_harmonize/in",
                            file_type = ".feather",
                            stable = p0_harmonization_config$sdd_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$sdd_stable_date),
    packages = c("tidyverse", "googledrive", "feather"),
    format = "feather"
  ),
  
  # TSS
  tar_target(
    name = p2_wqp_data_aoi_tss,
    command = retrieve_data(target = "p2_wqp_data_aoi_tss_anon",
                            id_df = p2_tss_drive_ids,
                            local_folder = "3_harmonize/in",
                            file_type = ".feather",
                            stable = p0_harmonization_config$tss_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tss_stable_date),
    packages = c("tidyverse", "googledrive", "feather"),
    format = "feather"
  ),
  
  # True color
  tar_target(
    name = p2_wqp_data_aoi_tc,
    command = retrieve_data(target = "p2_wqp_data_aoi_tc_anon",
                            id_df = p2_tc_drive_ids,
                            local_folder = "3_harmonize/in",
                            file_type = ".feather",
                            stable = p0_harmonization_config$tc_use_stable, 
                            google_email = p0_harmonization_config$google_email,
                            stable_date = p0_harmonization_config$tc_stable_date),
    packages = c("tidyverse", "googledrive", "feather"),
    format = "feather"
  )
  
)


# Full targets list
c(config_targets,
  p3_targets_list,
  p3_chla_targets_list,
  p3_doc_targets_list,
  p3_sdd_targets_list,
  p3_tss_targets_list,
  bookdown_targets_list)