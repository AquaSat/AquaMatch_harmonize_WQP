# Created by use_targets().
# https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Packages that all targets need
  packages = c("tidyverse"), 
  # Default
  format = "rds"
)

# Run the R scripts with custom functions:
tar_source(files = c(
  "1_inventory.R",
  "2_download.R",
  "3_harmonize.R",
  "src/functions.R"))

# The list of targets/steps (these steps from MRB)
mrb_targets <- list(
  
  # WQP config --------------------------------------------------------------
  
  # Things that often used to be YAMLs, and which probably should be again in 
  # the future. For right now I'm putting them as targets so I can conceptualize
  # the workflow components more easily
  
  # Date range of interest
  tar_target(wq_dates,
             list(
               start_date = "1984-01-01",
               end_date = "2019-05-01"
             )),
  
  # Define which parameter groups (and CharacteristicNames) to return from WQP. 
  # Different options for parameter groups are represented in the first level of 
  # 1_inventory/cfg/wqp_codes.yml. This yml file is meant to provide a starting 
  # place for an analysis and does not represent a definitive list of characteristic 
  # names. Which characteristic names to include for any given parameter group may 
  # change depending on the user or application, so the yml file can be edited to 
  # omit characteristic names or include others, to change top-level parameter names,
  # or to customize parameter groupings. 
  tar_target(param_groups_select,
             c("chlorophyll", "secchi", "cdom", "doc", "silica", "true_color", 
               "tss")
             # c("tss", "secchi")
  ),
  
  
  # WQP inventory -----------------------------------------------------------
  
  # Specify arguments to WQP queries
  # see https://www.waterqualitydata.us/webservices_documentation for more information 
  tar_target(wqp_args,
             list(sampleMedia = c("Water","water"),
                  siteType = c("Lake, Reservoir, Impoundment",
                               "Stream",
                               "Estuary",
                               "Facility"),
                  # Return sites with at least one data record
                  minresults = 1, 
                  startDateLo = wq_dates$start_date,
                  startDateHi = wq_dates$end_date)),
  
  
  # Work with pulled WQP data -----------------------------------------------
  
  # How many rows per parameter?
  # tar_target(param_counts,
  #            p3_wqp_data_aoi_clean_grp %>%
  #              count(parameter)),
  
  # Creates a match table with column names from WQP and shorter names to use
  # in renaming them
  tar_target(wqp_col_match,
             create_match_table()),
  
  
  # Input file tracking -----------------------------------------------------
  
  # Parameter datasets (pre-downloaded for early phases of dev)
  # # Color
  # tar_file_read(
  #   name = raw_true_color,
  #   command = "data/raw_color_202211.feather",
  #   read = read_feather(path = !!.x),
  #   packages = c("feather"),
  #   format = "feather"
  # ),
  # 
  # # Secchi
  # tar_file_read(
  #   name = raw_sdd,
  #   command = "data/raw_secchi_202211.feather",
  #   read = read_feather(path = !!.x),
  #   packages = c("feather"),
  #   format = "feather"
  # ),
  # 
  # # Silica
  # tar_file_read(
  #   name = raw_silica,
  #   command = "data/raw_silica_202211.feather",
  #   read = read_feather(path = !!.x),
  #   packages = c("feather"),
  #   format = "feather"
  # ),
  # 
  # # TSS: MRB downloaded from the AquaSat figshare
  # tar_file_read(
  #   name = raw_tss,
  #   command = "data/raw_tss.feather",
  #   read = read_feather(path = !!.x),
  #   packages = c("feather"),
  #   format = "feather"
  # ),
  
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
  
  
  # Parameter cleaning ------------------------------------------------------
  
  tar_target(
    # Get parameter codes for use in cleaning processes
    name = p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  
  # Parameter cleaning but with USGS WQP inputs -----------------------------
  
  # Same process as above, just using inputs that were pulled by this pipeline
  # as opposed to those provided by KW
  
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
  
  tar_render(silica_report,
             path = "src/silica_update_usgs_placeholder.Rmd",
             output_file = "../docs/silica_update_usgs_placeholder.Rmd",
             packages = c("tidyverse", "lubridate", "forcats")#,
             # In the future reports like this could be paramaterized instead
             # of using tar_read()/_load() calls inside the Rmd because
             # this makes it more readable when updating the pipeline
             # params = list(harmonized_silica = harmonized_silica,
             #               p_codes = p_codes,
             #               raw_silica = wqp_data_aoi_formatted_filtered %>%
             #                 filter(parameter == "silica"))
  ),
  
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

# Full targets list
c(p1_targets_list, p2_targets_list, p3_targets_list, mrb_targets)


















