# Source the functions that will be used to build the targets in p3_targets_list
tar_source(files = "3_harmonize/src/")

p3_targets_list <- list(
  
  # Pre-harmonization data prep ---------------------------------------------
  
  # All columns in p2_wqp_data_aoi are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    p3_wqp_data_aoi_formatted,
    format_columns(p2_wqp_data_aoi),
    format = "feather"
  ),
  
  # Creates a match table with column names from WQP and shorter names to use
  # in renaming them
  tar_target(p3_wqp_col_match,
             create_match_table()),
  
  # Cleaning steps before breaking out by parameter: 
  # Remove duplicates, ensure meaningful results present, check data status,
  # check media, remove white spaces
  tar_target(p3_wqp_data_aoi_ready,
             clean_wqp_data(wqp_data = p3_wqp_data_aoi_formatted,
                            char_names_crosswalk = p1_char_names_crosswalk,
                            # Convert list of sites by param to single df
                            site_data = bind_rows(p2_site_counts),
                            match_table = p3_wqp_col_match, 
                            wqp_metadata = p1_wqp_inventory_aoi),
             packages = c("tidyverse", "lubridate"),
             format = "feather"),
  
  # Connect cleaned data output to the pipeline
  tar_target(p3_cleaned_wqp_data,
             read_feather(p3_wqp_data_aoi_ready$wqp_data_clean_path),
             packages = "feather",
             format = "feather",
             cue = tar_cue("always")),
  
  # Get parameter codes for use in cleaning processes
  tar_target(
    name = p3_p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  # A quick separate step to export the dataset to a file for easier review
  # Not integrating it deeper into existing targets for now
  # tar_file(p3_wqp_data_aoi_ready_out,
  #          {
  #            out_path <- "data/out/p3_wqp_data_aoi_ready.feather"
  #            
  #            write_feather(x = p3_wqp_data_aoi_ready,
  #                          path = out_path)
  #            
  #            out_path
  #          },
  #          packages = c("feather")),
  # 
  
  # Matchup tables ----------------------------------------------------------
  
  # Secchi depth method matchup table
  tar_file_read(name = p3_sdd_analytical_method_matchup,
                command = "data/in/sdd_analytical_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Secchi sample method matchup table
  tar_file_read(name = p3_sdd_sample_method_matchup,
                command = "data/in/sdd_sample_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Secchi equipment matchup table
  tar_file_read(name = p3_sdd_equipment_matchup,
                command = "data/in/sdd_collection_equipment_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  # Chla depth method matchup table
  tar_file_read(name = p3_chla_analytical_method_matchup,
                command = "data/in/chla_analytical_method_matchup.csv",
                read = read_csv(file = !!.x),
                cue = tar_cue("always")),
  
  
  # Harmonization process ---------------------------------------------------
  
  tar_target(p3_harmonized_tss,
             harmonize_tss(raw_tss = p3_cleaned_wqp_data %>%
                             filter(parameter == "tss"),
                           p_codes = p3_p_codes),
             packages = c("tidyverse", "lubridate", "pander", "feather")),
  
  tar_target(p3_harmonized_chla,
             harmonize_chla(raw_chla = p3_cleaned_wqp_data %>%
                              filter(parameter == "chlorophyll"),
                            p_codes = p3_p_codes,
                            chla_analytical_method_matchup = p3_chla_analytical_method_matchup),
             packages = c("tidyverse", "lubridate", "feather")),
  
  tar_target(p3_harmonized_sdd,
             harmonize_sdd(raw_sdd = p3_cleaned_wqp_data %>%
                             filter(parameter == "secchi"),
                           p_codes = p3_p_codes,
                           sdd_analytical_method_matchup = p3_sdd_analytical_method_matchup,
                           sdd_sample_method_matchup = p3_sdd_sample_method_matchup,
                           sdd_equipment_matchup = p3_sdd_equipment_matchup),
             packages = c("tidyverse", "lubridate", "feather")),
  
  tar_target(p3_harmonized_doc,
             harmonize_doc(raw_doc = p3_cleaned_wqp_data %>%
                             filter(parameter == "doc"),
                           p_codes = p3_p_codes),
             packages = c("tidyverse", "lubridate", "feather")),
  
  tar_target(p3_documented_drops,
             map_df(.x = c(p3_wqp_data_aoi_ready$compiled_drops_path,
                           p3_harmonized_chla$compiled_drops_path,
                           p3_harmonized_sdd$compiled_drops_path,
                           p3_harmonized_doc$compiled_drops_path,
                           p3_harmonized_tss$compiled_drops_path),
                    .f = read_csv),
             cue = tar_cue("always")),
  
  # A target using the harmonized outputs to prepare a dataset for the later
  # analysis steps
  tar_target(p3_harmonized_wqp_w_methods,
             {
               # Read in the exported harmonized datasets
               
               map_df(.x = c(p3_harmonized_chla, p3_harmonized_doc,
                             p3_harmonized_tss, p3_harmonized_sdd),
                      .f = ~ read_feather(.x) %>%
                        select(SiteID, date, lat, lon,
                               harmonized_parameter = parameter, orig_parameter,
                               analytical_method))
             },
             packages = c("tidyverse", "feather"))
  
)

