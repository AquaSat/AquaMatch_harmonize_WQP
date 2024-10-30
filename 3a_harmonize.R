# General purpose targets list for the harmonization step

# Source the functions that will be used to build the targets in p3_targets_list
tar_source(files = "3_harmonize/src/")

p3_targets_list <- list(
  
  # Harmonization process ---------------------------------------------------
  
  # Get parameter codes for use in cleaning processes
  tar_target(
    name = p3_p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  
  # Documentation of dropped records ----------------------------------------
  
  # Runs after parameter-specific harmonization targets (chla, DOC, etc.)
  tar_target(
    name = p3_documented_drops,
    command = map_df(.x = c(
      # chla
      p3_wqp_data_aoi_ready_chl$compiled_drops_path,
      p3_chla_harmonized$compiled_drops_path,
      # SDD
      p3_wqp_data_aoi_ready_sdd$compiled_drops_path,
      p3_sdd_harmonized$compiled_drops_path,
      # DOC
      p3_wqp_data_aoi_ready_doc$compiled_drops_path,
      p3_doc_harmonized$compiled_drops_path
    ),
    .f = read_csv)
  )
)

