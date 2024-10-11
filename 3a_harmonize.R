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
  
  # A file containing sf objects to be used in maps during harmonization
  tar_file(
    name = p3_us_territories_list,
    command = {
      
      out_path <- "3_harmonize/in/us_territories_list.rds"
      
      states_sf <- states(progress_bar = FALSE) %>%
        st_transform(crs = 9311)
      
      list(
        conterminous_us = states_sf %>%
          filter(!(NAME %in% c("Alaska", "Hawaii", "American Samoa",
                               "Guam", "Puerto Rico",
                               "United States Virgin Islands",
                               "Commonwealth of the Northern Mariana Islands"))),
        alaska = states_sf %>%
          filter(NAME == "Alaska"),
        hawaii = states_sf %>%
          filter(NAME == "Hawaii"),
        american_samoa = states_sf %>%
          filter(NAME == "American Samoa"),
        guam_marianas = states_sf %>%
          filter(NAME %in% c("Guam", "Commonwealth of the Northern Mariana Islands")),
        puerto_rico_virgin_islands = states_sf %>%
          filter(NAME %in% c("Puerto Rico", "United States Virgin Islands"))
      ) %>%
        write_rds(file = out_path)
      
      out_path
    },
    packages = c("tidyverse", "sf", "tigris")
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

