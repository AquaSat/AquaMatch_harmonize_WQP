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
    # Ensure that all dfs are compatible w.r.t. column type
    map_df(.f = ~.x %>%
             mutate(across(everything(), ~as.character(.x)))
    ) %>%
    # Keep only the sites for which we've got harmonized data
    filter(MonitoringLocationIdentifier %in% site_ids)
  
  # Return the final product
  site_metadata
  
}

#' @title Fill in gaps in the `ActivityStartDateTime` column and create a UTC
#' version.
#' 
#' @description
#' Function to identify and fill missing or partial data in the `ActivityStartDateTime`
#' column and then create a harmonized version in UTC. Includes spatial join to
#' fill timezones for data missing an `ActivityStartTime.TimeZoneCode`, convert
#' timezones to location-based format, and provide columns containing harmonized
#' time zone infomation and harmonized times in UTC.
#' 
#' @param dataset A data frame containing minimum columns `MonitoringLocationIdentifier`,
#' `ActivityStartDateTime`, `ActivityStartDate`, `ActivityStartTime.Time`, and
#' `ActivityStartTime.TimeZoneCode`. 
#' 
#' @param site_data A data frame containing site metadata for WQP monitoring sites.
#' At minimum should contain `MonitoringLocationIdentifier`, `datum`, `lat`,
#' `lon`.
#' 
#' @returns 
#' Returns a modified version of `dataset` with the additional columns,
#' `harmonized_tz` and `harmonized_utc`.
#' 
fill_date_time <- function(dataset, site_data){
  
  # 1. Complete the timezone records using lat/lon
  
  # Datum varies throughout the dataset
  epsg_codes <- tribble(
    ~datum, ~epsg,
    "NAD83", 4269,
    "WGS84", 4326,
    "NAD27", 4267,
    "OLDHI", 4135,
    "WGS72", 4322,
    # Less certain about these two but they seem to match up
    "AMSMA", 4169,
    "PR", 4139,
    # Assume these are WGS84
    "UNKWN", 4326,
    "OTHER", 4326
  )
  
  # Add EPSG codes
  site_w_epsg <- site_data %>%
    left_join(x = .,
              y = epsg_codes,
              by = "datum")
  
  # Unify CRS so we can have a single sf object
  site_sf_unified <- site_w_epsg %>%
    # Group by CRS 
    split(f = .$datum) %>%
    # Transform and re-stack
    map_df(.x = .,
           .f = ~ .x %>%
             st_as_sf(coords = c("lon", "lat"),
                      crs = unique(.x$epsg)) %>%
             st_transform(crs = 4326)) 
  
  # Identify timezones for all unique lat/longs to fill gaps when needed. Note
  # that this is not perfect, because the location isn't a guarantee of the
  # timezone that was used in recording. It's our best guess
  site_sf_unified$fetched_tz <- tz_lookup(site_sf_unified, method = "accurate")
  
  # Get clean site x tz data
  site_tz <- site_sf_unified %>%
    as_tibble() %>%
    distinct(MonitoringLocationIdentifier, fetched_tz)
  
  # Big
  rm(site_sf_unified)
  gc()
  
  # Add tz data to main dataset and create col using old where possible
  # but filling in the fetched data where needed
  dataset_tz <- dataset %>%
    left_join(x = ., y = site_tz) %>%
    mutate(
      harmonized_tz = if_else(
        condition = is.na(ActivityStartTime.TimeZoneCode),
        true = fetched_tz,
        false = ActivityStartTime.TimeZoneCode
      ),
      # We'll want to convert the abbreviations into location-based tz:
      harmonized_tz = case_when(
        grepl(x = harmonized_tz, pattern = "EDT|EST") ~ "America/New_York",
        grepl(x = harmonized_tz, pattern = "GMT") ~ "Europe/London",
        grepl(x = harmonized_tz, pattern = "CDT|CST") ~ "America/Chicago",
        grepl(x = harmonized_tz, pattern = "MDT|MST") ~ "America/Denver",
        grepl(x = harmonized_tz, pattern = "ADT|AST") ~ "America/Halifax",
        grepl(x = harmonized_tz, pattern = "BST") ~ "Europe/London",
        grepl(x = harmonized_tz, pattern = "AKDT|AKST") ~ "America/Anchorage",
        grepl(x = harmonized_tz, pattern = "PDT|PST") ~ "America/Los_Angeles",
        # Not all locations in HST observe DST. All in current dataset are
        # HI based, so using Pacific here
        grepl(x = harmonized_tz, pattern = "HST") ~ "Pacific/Honolulu",
        # If HDT shows up it's likely AK
        grepl(x = harmonized_tz, pattern = "HDT") ~ "America/Adak",
        # Suspect this is Guam Standard Time; data is prior to tz change in 2000
        grepl(x = harmonized_tz, pattern = "GST") ~ "Pacific/Guam",
        # Others use the tz from joining
        .default = fetched_tz
      )
    ) 
  
  rm(dataset)
  gc()
  
  # 2. Create a harmonized UTC time column that also fills in blank times with
  # 11:59:59 AM, which we've found is used rarely or not at all in WQP data
  dataset_utc <- dataset_tz %>%
    mutate(
      temp_local_time = case_when(
        # DateTime has date only OR is NA + Date is filled + Time is empty:
        # assign noon
        ( (!is.na(ActivityStartDateTime) & (nchar(ActivityStartDateTime) <= 10)) |
            is.na(ActivityStartDateTime) ) &
          !is.na(ActivityStartDate) &
          is.na(ActivityStartTime.Time) ~ paste0(ActivityStartDate, " 11:59:59"),
        # DateTime has date only OR is NA + Date is filled + Time is filled:
        # combine date + time.
        # Note: A small subset of ActivityStartDate != dates in ActivityStartDateTime
        ( (!is.na(ActivityStartDateTime) & (nchar(ActivityStartDateTime) <= 10)) |
            is.na(ActivityStartDateTime) ) &
          !is.na(ActivityStartDate) &
          !is.na(ActivityStartTime.Time) ~ paste0(ActivityStartDate,
                                                  " ",
                                                  ActivityStartTime.Time),
        # If has StartDateTime then good
        (!is.na(ActivityStartDateTime) &
           (nchar(ActivityStartDateTime) > 10)) ~ ActivityStartDateTime,
        # Nothing to go on, then NA
        is.na(ActivityStartDateTime) & is.na(ActivityStartDate) ~ NA_character_
      )
    ) %>%
    # ymd_hms() below doesn't vectorize without rowwise():
    rowwise() %>%
    # Temporary column to generate local time
    mutate(
      temp_local_time = ymd_hms(temp_local_time, tz = harmonized_tz)
    ) %>%
    ungroup() %>%
    # Create a converted UTC time column
    mutate(
      harmonized_utc = with_tz(temp_local_time, "UTC")
    )
  
  # Return the final product without temporary cols
  dataset_utc %>%
    select(-c(fetched_tz, temp_local_time))
  
}
