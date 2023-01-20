# Source the functions that will be used to build the targets in p3_targets_list
source("3_harmonize/src/clean_wqp_data.R")
source("3_harmonize/src/create_match_table.R")
source("3_harmonize/src/flag_duplicated_records.R")
source("3_harmonize/src/flag_missing_results.R")
source("3_harmonize/src/format_columns.R")
source("3_harmonize/src/get_p_codes.R")
source("3_harmonize/src/harmonize_silica.R")
source("3_harmonize/src/harmonization_report_helper_functions.R")
source("3_harmonize/src/harmonize_sdd.R")
source("3_harmonize/src/harmonize_true_color.R")
source("3_harmonize/src/harmonize_tss.R")
source("3_harmonize/src/remove_duplicates.R")


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
  tar_target(wqp_col_match,
             create_match_table()),
  
  # Get parameter codes for use in cleaning processes
  tar_target(
    name = p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  # The input data
  tar_target(wqp_data_aoi_formatted_filtered,
             p3_wqp_data_aoi_formatted %>%
               left_join(x = .,
                         y = p1_char_names_crosswalk,
                         by = c("CharacteristicName" = "char_name"))),
  
  # A quick separate step to export the dataset to a file for easier review
  # Not integrating it deeper into existing targets for now
  tar_file(wqp_data_aoi_formatted_filtered_out,
           {
             out_path <- "data/wqp_data_aoi_formatted_filtered.feather"
             
             write_feather(x = wqp_data_aoi_formatted_filtered,
                           path = out_path)
             
             out_path
           },
           packages = c("feather")),
  
  
  # Matchup tables ----------------------------------------------------------
  
  # Secchi depth method matchup table
  tar_file_read(name = sdd_analytical_method_matchup,
                command = "data/sdd_analytical_method_matchup.csv",
                read = read_csv(file = !!.x)),
  
  # Secchi sample method matchup table
  tar_file_read(name = sdd_sample_method_matchup,
                command = "data/sdd_sample_method_matchup.csv",
                read = read_csv(file = !!.x)),
  
  # Secchi equipment matchup table
  tar_file_read(name = sdd_equipment_matchup,
                command = "data/sdd_collection_equipment_matchup.csv",
                read = read_csv(file = !!.x)),
  
  
  # Harmonization process ---------------------------------------------------
  
  # For some reason the plots in this target don't work if you don't have
  # forcats loaded when using tar_read()
  tar_target(harmonized_silica,
             harmonize_silica(raw_silica = wqp_data_aoi_formatted_filtered %>%
                                filter(parameter == "silica"),
                              p_codes = p_codes,
                              commenttext_missing = c("analysis lost", "not analyzed", 
                                                      "not recorded", "not collected", 
                                                      "no measurement taken"),
                              duplicate_definition = c("org_id",
                                                       "SiteID",
                                                       "date", 
                                                       "time",
                                                       "orig_parameter", 
                                                       "fraction"),
                              match_table = wqp_col_match),
             packages = c("tidyverse", "lubridate", "forcats", "scales",
                          "ggthemes")),
  
  tar_target(harmonized_true_color,
             harmonize_true_color(raw_true_color = wqp_data_aoi_formatted_filtered %>%
                                    filter(parameter == "true_color"),
                                  p_codes = p_codes,
                                  match_table = wqp_col_match),
             packages = c("tidyverse", "lubridate")),
  
  tar_target(harmonized_tss,
             harmonize_tss(raw_tss = wqp_data_aoi_formatted_filtered %>%
                             filter(parameter == "tss"),
                           p_codes = p_codes,
                           match_table = wqp_col_match),
             packages = c("tidyverse", "lubridate", "pander")),
  
  tar_target(harmonized_sdd,
             harmonize_sdd(raw_sdd = wqp_data_aoi_formatted_filtered %>%
                             filter(parameter == "secchi"),
                           p_codes = p_codes,
                           # Column renaming
                           match_table = wqp_col_match,
                           sdd_analytical_method_matchup = sdd_analytical_method_matchup,
                           sdd_sample_method_matchup = sdd_sample_method_matchup,
                           sdd_equipment_matchup = sdd_equipment_matchup),
             packages = c("tidyverse", "lubridate")),
  
  
  # Report rendering --------------------------------------------------------
  
  tar_render(silica_report,
             path = "src/silica_update_usgs_placeholder.Rmd",
             output_file = "../docs/silica_update_usgs_placeholder.Rmd",
             packages = c("tidyverse", "lubridate", "forcats")),
  
  tar_render(true_color_report,
             path = "src/true_color_update_usgs_placeholder.Rmd",
             output_file = "../docs/true_color_update_usgs_placeholder.Rmd",
             packages = c("tidyverse", "lubridate", "forcats", "kableExtra")),
  
  tar_render(tss_report,
             path = "src/tss_update_usgs_placeholder.Rmd",
             output_file = "../docs/tss_update_usgs_placeholder.Rmd",
             packages = c("tidyverse", "lubridate", "forcats", "kableExtra")),
  
  tar_render(sdd_report,
             path = "src/sdd_update_usgs_placeholder.Rmd",
             output_file = "../docs/sdd_update_usgs_placeholder.Rmd",
             packages = c("tidyverse", "lubridate", "forcats", "kableExtra"))
  
  
)
