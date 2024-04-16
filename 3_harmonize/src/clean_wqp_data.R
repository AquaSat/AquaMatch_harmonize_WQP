#' @title Clean WQP data
#' 
#' @description 
#' Function to harmonize WQP data in preparation for further analysis. Included
#' in this function are steps to unite diverse characteristic names by assigning
#' them to more commonly-used water quality parameter names; to flag missing
#' records as well as duplicate records; and to carry out parameter-specific
#' harmonization steps for temperature and conductivity data, including
#' harmonizing units where possible. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. 
#' @param char_names_crosswalk data frame containing columns "char_name" and 
#' "parameter". The column "char_name" contains character strings representing 
#' known WQP characteristic names associated with each parameter.
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values. By 
#' default, the column "ResultCommentText" will be searched for the following 
#' strings: "analysis lost", "not analyzed", "not recorded", "not collected", 
#' and "no measurement taken", but other values may be added by passing in a new
#' vector with all values to be treated as missing.  
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`. By 
#' default, a record will be considered duplicated if it shares the same 
#' organization, site id, date, time, characteristic name, and sample fraction. 
#' However, these options can be customized by passing a vector of column names 
#' to the argument `duplicate_definition`.
#' @param remove_duplicated_rows logical; should duplicated records be omitted
#' from the cleaned dataset? Defaults to FALSE 
#' 
#' @returns 
#' Returns a formatted and harmonized data frame containing data downloaded from 
#' the Water Quality Portal, where each row represents a unique data record.
#' 

clean_wqp_data <- function(wqp_data,
                           char_names_crosswalk,
                           site_data,
                           # match_table,
                           wqp_metadata,
                           commenttext_missing = c('analysis lost', 'not analyzed',
                                                   'not recorded', 'not collected',
                                                   'no measurement taken'),
                           duplicate_definition = c('OrganizationIdentifier',
                                                    'MonitoringLocationIdentifier',
                                                    'ActivityStartDate',
                                                    'ActivityStartTime.Time',
                                                    'CharacteristicName',
                                                    'ResultSampleFractionText'),
                           remove_duplicated_rows = FALSE){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = NULL,
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(wqp_data),
    order = 0
  )
  
  # Clean data and assign flags if applicable
  wqp_data_no_dup <- wqp_data %>%
    # Harmonize characteristic names by assigning a common parameter name
    # to the groups of characteristics supplied in `char_names_crosswalk`.
    left_join(y = char_names_crosswalk, by = c("CharacteristicName" = "char_name")) %>%
    # Add in coordinate data alongside records
    left_join(x = .,
              y = site_data %>%
                select(MonitoringLocationIdentifier, CharacteristicName,
                       lon, lat, datum),
              by = c("MonitoringLocationIdentifier", "CharacteristicName")) %>%
    # Flag true missing results
    flag_missing_results(., commenttext_missing)
  
  
  rm(wqp_data)
  gc()
  
  # Remove records flagged as having missing results
  wqp_data_no_missing <- wqp_data_no_dup %>%
    filter(!flag_missing_result) %>%
    # All will now be FALSE; remove col
    select(-flag_missing_result)
  
  # Inform the user what we found for missing rows
  message(sprintf(paste0("Removed %s records with missing results."),
                  nrow(wqp_data_no_dup) - nrow(wqp_data_no_missing)))
  
  dropped_missing <- tibble(
    step = NULL,
    reason = "Removed missing results",
    short_reason = "Remove missing",
    number_dropped = nrow(wqp_data_no_dup) - nrow(wqp_data_no_missing),
    n_rows = nrow(wqp_data_no_missing),
    order = 1
  )
  
  rm(wqp_data_no_dup)
  gc()
  
  # Remove records that don't meet needs for status
  wqp_data_pass_status <- wqp_data_no_missing %>%
    filter(ResultStatusIdentifier %in%
             c('Accepted', 'Final', 'Historical', 'Validated', 'Preliminary')|
             is.na(ResultStatusIdentifier))
  
  # Inform the user what we found for status checks
  message(sprintf(paste0("Removed %s records with unacceptable statuses."), 
                  nrow(wqp_data_no_missing) - nrow(wqp_data_pass_status)))
  
  dropped_status <- tibble(
    step = NULL,
    reason = "Keep only status = accepted/final/historical/validated/preliminary/NA",
    short_reason = "Finalized statuses",
    number_dropped = nrow(wqp_data_no_missing) - nrow(wqp_data_pass_status),
    n_rows = nrow(wqp_data_pass_status),
    order = 2
  )
  
  rm(wqp_data_no_missing)
  gc()
  
  # Remove white space before export
  wqp_data_clean <- wqp_data_pass_status %>%
    mutate(year = year(ActivityStartDate),
           ResultMeasure.MeasureUnitCode = trimws(ResultMeasure.MeasureUnitCode))
  
  data_out_path <- paste0("3_harmonize/out/",
                          unique(wqp_data_clean$parameter),
                          "_wqp_data_aoi_ready.feather")
  
  write_feather(x = wqp_data_clean, path = data_out_path)
  
  rm(wqp_data_pass_status)
  gc()
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_missing,
                                dropped_status)
  
  # Make sure only one parameter was cleaned
  if(length(unique(wqp_data_clean$parameter)) > 1){
    stop("More than one parameter detected in final data frame.")
  }
  
  # Identify the parameter that's been cleaned in this script:
  compiled_dropped <- compiled_dropped %>%
    mutate(step = paste0(unique(wqp_data_clean$parameter), " pre-harmonization"))
  
  documented_drops_out_path <- paste0("3_harmonize/out/clean_wqp_data_",
                                      unique(wqp_data_clean$parameter),
                                      "_dropped_metadata.csv")
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  return(list(
    wqp_data_clean_path = data_out_path,
    compiled_drops_path = documented_drops_out_path))
  
}


#' @title Flag missing results
#' 
#' @description 
#' Function to flag true missing results, i.e. when the result measure value 
#' and detection limit value are both NA; when results, units, and the three
#' comment fields are NA; when "not reported" is found in the
#' column "ResultDetectionConditionText"; or when any of the strings from
#' `commenttext_missing` are found in the column "ResultCommentText".
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. Must contain the columns
#' "DetectionQuantitationLimitMeasure.MeasureValue", "ResultMeasureValue", 
#' "ResultDetectionConditionText", and "ResultCommentText".
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values.
#' 
#' @returns
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for missing results. 
#' 
flag_missing_results <- function(wqp_data, commenttext_missing){
  
  wqp_data_out <- wqp_data %>%
    mutate(flag_missing_result = 
             ( is.na(ResultMeasureValue) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ) |
             ( is.na(ResultMeasureValue) & is.na(ResultMeasure.MeasureUnitCode) &
                 is.na(ActivityCommentText) & is.na(ResultLaboratoryCommentText) &
                 is.na(ResultCommentText) ) |
             grepl(paste(commenttext_missing, collapse = "|"), ResultCommentText, ignore.case = TRUE)
    )
  
  return(wqp_data_out)
  
}



#' @title Flag duplicated records
#' 
#' @description 
#' Function to flag duplicated rows based on a user-supplied definition
#' of a duplicate record. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#'
#' @returns 
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for duplicated records. 
#' 
flag_duplicates <- function(wqp_data, duplicate_definition){
  
  # Flag duplicate records using the `duplicate_definition`
  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    mutate(n_duplicated = n(),
           flag_duplicated_row = n_duplicated > 1) %>% 
    ungroup() %>%
    select(-n_duplicated)
  
  return(wqp_data_out)
  
}


#' @title Remove duplicated records
#' 
#' @description
#' Function to append additional flags to sets of duplicate rows that are then 
#' used to drop duplicates from the dataset. Currently, we randomly retain the 
#' first record in a set of duplicated rows and drop all others.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#' 
#' @returns 
#' Returns a data frame containing data downloaded from the Water Portal in which
#' duplicated rows have been removed. 
#' 
remove_duplicates <- function(wqp_data, duplicate_definition){
  
  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines;
    # the rows should be ordered the same way across machines so that when we 
    # "randomly" select the first duplicated row below, the output is consistent
    # for all users.
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    # To help resolve duplicates, randomly select the first record
    # from each duplicated set and flag all others for exclusion.
    mutate(n_duplicated = n(),
           dup_number = seq(n_duplicated),
           flag_duplicate_drop_random = n_duplicated > 1 & dup_number != 1) %>%
    filter(flag_duplicate_drop_random == FALSE) %>%
    ungroup() %>%
    select(-c(n_duplicated, dup_number, flag_duplicate_drop_random))
  
  return(wqp_data_out)
  
}


#' @title Retrieve and clean site metadata for harmonized records
#' 
#' @description
#' Function to retrieve site metadata from the WQP using organization IDs. Once
#' metadata are retrieved they are filtered to only include sites used in the
#' final harmonized data product.
#' 
#' @param dataset The harmonized data product *before* simultaneous records are
#' aggregated. Should be a data frame including the columns `OrganizationIdentifier`
#' and `MonitoringLocationIdentifier`.
#' 
#' @returns 
#' Returns a data frame containing site metadata downloaded from the Water Portal
#' and filtered to only include sites present in the harmonized data product.
#' 
get_site_info <- function(dataset){
  
  # Use safely() when requesting data to prepare for the possibility of failures
  safe_pull <- safely(.f = ~whatWQPsites(organization = .x))
  
  # Vector of organizations whose sites we want to query (batch download process
  # will request one org's site data at a time)
  org_ids <- dataset %>%
    pull(OrganizationIdentifier) %>%
    unique()
  
  # Vector of site IDs that we're interested in
  site_ids <- dataset %>%
    pull(MonitoringLocationIdentifier) %>%
    unique()
  
  # Returns a list of result/error list item pairs for each org. Each result,
  # if not an error, will be a data frame
  raw_org_pull <- map(
    .x = org_ids,
    .f = ~ safe_pull(.x)
  )
  
  # Now clean the outputs of the pull:
  site_metadata <- raw_org_pull %>%
    # Transpose into a list with 2 items: result and error
    transpose() %>%
    # Keep only results
    pluck("result") %>%
    # Discard anything that failed
    purrr::discard(is.null) %>%
    # Ensure that all dfs have compatible column types
    map_df(.f = ~.x %>%
             mutate(across(everything(), ~as.character(.x)))
    ) %>%
    # Keep only the sites for which we've got harmonized data
    filter(MonitoringLocationIdentifier %in% site_ids)
  
  # Return the final product
  site_metadata
  
}
