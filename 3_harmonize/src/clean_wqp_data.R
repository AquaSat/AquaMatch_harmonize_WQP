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
#' from the cleaned dataset? Defaults to TRUE. 
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
                           remove_duplicated_rows = TRUE){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "pre-harmonization",
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
              by = c("MonitoringLocationIdentifier", "CharacteristicName",
                     "parameter")) %>%
    # Flag true missing results
    flag_missing_results(., commenttext_missing) %>%
    # Flag duplicate records
    flag_duplicates(., duplicate_definition) %>%
    {if(remove_duplicated_rows){
      remove_duplicates(., duplicate_definition)
    } else {.}
    }
  
  # Inform the user what we found for duplicated rows
  if(remove_duplicated_rows){
    message(sprintf(paste0("Removed %s duplicated records."), 
                    nrow(wqp_data) - nrow(wqp_data_no_dup)))
  }
  
  # Record info on the dropped rows
  dropped_duplicates <- tibble(
    step = "pre-harmonization",
    reason = "Removed duplicated records",
    short_reason = "Remove duplicates",
    number_dropped = nrow(wqp_data) - nrow(wqp_data_no_dup),
    n_rows = nrow(wqp_data_no_dup),
    order = 1
  )
  
  rm(wqp_data)
  gc()
  
  # Remove records flagged as having missing results
  wqp_data_no_missing <- wqp_data_no_dup %>%
    filter(!flag_missing_result)
  
  # # Inform the user what we found for missing rows
  message(sprintf(paste0("Removed %s records with missing results."),
                  nrow(wqp_data_no_dup) - nrow(wqp_data_no_missing)))
  
  dropped_missing <- tibble(
    step = "pre-harmonization",
    reason = "Removed missing results",
    short_reason = "Remove missing",
    number_dropped = nrow(wqp_data_no_dup) - nrow(wqp_data_no_missing),
    n_rows = nrow(wqp_data_no_missing),
    order = 2
  )
  
  rm(wqp_data_no_dup)
  gc()
  
  # Remove records that don't meet needs for status
  wqp_data_pass_status <- wqp_data_no_missing %>%
    filter(ResultStatusIdentifier %in%
             # MR's version had Preliminary & NA included too, so let's try those:
             c('Accepted', 'Final', 'Historical', 'Validated', 'Preliminary')|
             is.na(ResultStatusIdentifier))
  
  # Inform the user what we found for status checks
  message(sprintf(paste0("Removed %s records with unacceptable statuses."), 
                  nrow(wqp_data_no_missing) - nrow(wqp_data_pass_status)))
  
  dropped_status <- tibble(
    step = "pre-harmonization",
    reason = "Keep only status = accepted/final/historical/validated/preliminary",
    short_reason = "Finalized statuses",
    number_dropped = nrow(wqp_data_no_missing) - nrow(wqp_data_pass_status),
    n_rows = nrow(wqp_data_pass_status),
    order = 3
  )
  
  rm(wqp_data_no_missing)
  gc()
  
  # Remove records that don't meet needs for type
  wqp_data_pass_media <- wqp_data_pass_status %>%
    filter(ActivityMediaSubdivisionName %in% c('Surface Water', 'Water', 'Estuary') |
             is.na(ActivityMediaSubdivisionName))
  
  # Inform the user what we found for type checks
  message(sprintf(paste0("Removed %s records with unacceptable type."), 
                  nrow(wqp_data_pass_status) - nrow(wqp_data_pass_media)))
  
  dropped_media <- tibble(
    step = "pre-harmonization",
    reason = "Keep only media = surface water/water/estuary",
    short_reason = "Media types",
    number_dropped = nrow(wqp_data_pass_status) - nrow(wqp_data_pass_media),
    n_rows = nrow(wqp_data_pass_media),
    order = 4
  )
  
  rm(wqp_data_pass_status)
  gc()
  
  # Remove white space before export
  wqp_data_clean <- wqp_data_pass_media %>%
    mutate(year = year(ActivityStartDate),
           ResultMeasure.MeasureUnitCode = trimws(ResultMeasure.MeasureUnitCode))
  
  data_out_path <- "3_harmonize/out/wqp_data_aoi_ready.feather"
  
  write_feather(x = wqp_data_clean, path = data_out_path)
  
  # Inform the user about rows dropped due to non-target location types
  message(sprintf(paste0("Removed %s records due to non-target location types"), 
                  nrow(wqp_data_pass_media) - nrow(wqp_data_clean)))
  
  dropped_location <- tibble(
    step = "pre-harmonization",
    reason = "Keep only location type = estuary/lake, res, impoundment/stream",
    short_reason = "Location type",
    number_dropped = nrow(wqp_data_pass_media) - nrow(wqp_data_clean),
    n_rows = nrow(wqp_data_clean),
    order = 5
  )
  
  rm(wqp_data_pass_media)
  gc()
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_duplicates, dropped_missing,
                                dropped_status, dropped_media, dropped_location)
  documented_drops_out_path <- "3_harmonize/out/clean_wqp_data_dropped_metadata.csv"
  
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
             grepl("not reported", ResultDetectionConditionText, ignore.case = TRUE) |
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



