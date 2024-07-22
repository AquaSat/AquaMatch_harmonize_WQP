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
                           wqp_metadata,
                           commenttext_missing = c("analysis lost", "not analyzed",
                                                   "not recorded", "not collected",
                                                   "no measurement taken"),
                           duplicate_definition = c("OrganizationIdentifier",
                                                    "MonitoringLocationIdentifier",
                                                    "ActivityStartDate",
                                                    "ActivityStartTime.Time",
                                                    "CharacteristicName",
                                                    "ResultSampleFractionText"),
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
             c("Accepted", "Final", "Historical", "Validated", "Preliminary")|
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
             ( is.na(ResultMeasureValue_original) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ) |
             ( is.na(ResultMeasureValue_original) & is.na(ResultMeasure.MeasureUnitCode) &
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

#' @title Create columns with consistent harmonized times in local time zones and
#' UTC.
#' 
#' @description
#' Function to identify and fill partial date and time data.
#' 
#' @param dataset A data frame containing WQP output. At minimum should contain
#' the columns `MonitoringLocationIdentifier`, `ActivityStartDateTime`,
#' `ActivityStartDate`, `ActivityStartTime.Time`, and `ActivityStartTime.TimeZoneCode`. 
#' 
#' @param site_data A data frame containing site metadata for WQP monitoring sites.
#' At minimum should contain `MonitoringLocationIdentifier`, `datum`, `lat`,
#' `lon`.
#' 
#' @details
#' Uses lat/lon data for monitoring sites to fill any missing time zones,
#' replaces times that are NA or 00:00:00 with 11:59:59 (local time), produces
#' harmonized versions of UTC and local date-time records, and reformats time
#' zones into GMT offset format (e.g., "Etc/GMT+7"). New columns
#' `harmonized_local_time`, `harmonized_tz`, and `harmonized_utc` are created.
#'  
#' `harmonized_local_time` is the local time of the sampling event determined by
#' this function and `harmonized_tz` contains the GMT offset timezone corresponding
#' to that local time point. `harmonized_utc` is the equivalent time in UTC
#' and will agree with `ActivityStartDateTime` in most, but not all cases. These
#' occur because 1) `ActivityStartDateTime` is NA for
#' `ActivityStartTime.TimeZoneCode` values of NA, "AST", "ADT", "GST", "IDLE"; 
#' or 2) we handle "00:00:00" values of `ActivityStartTime.Time` the same as NAs
#' whereas `ActivityStartDateTime` does not.
#' 
#' @returns 
#' Returns a modified version of `dataset` with the additional columns,
#' `harmonized_tz`, `harmonized_local_time`, and `harmonized_utc`.
#' 
fill_date_time <- function(dataset, site_data){
  
  # Prep: Define a table of time zones formatted as GMT offset for later. Will
  # be referenced shortly for tz filters and again later in the function
  gmt_offsets <- tribble(
    ~tz, ~gmt,
    
    # First cover those present in WQX Domain Values:
    # https://cdx.epa.gov/wqx/download/DomainValues/TimeZone.csv
    
    # Atlantic Daylight Time
    "ADT",      "Etc/GMT+3",
    # Alaska-Hawaii Standard Time (retired)
    "AHST",     "Etc/GMT+10",
    # Alaska Daylight Time
    "AKDT",     "Etc/GMT+8",
    # Alaska Standard Time
    "AKST",     "Etc/GMT+9",
    # Atlantic Standard Time
    "AST",      "Etc/GMT+4",
    # Bering Standard Time (retired)
    "BST",      "Etc/GMT+11",
    # Central Daylight Time
    "CDT",      "Etc/GMT+5", 
    # Sweden Daylight Time or Central European Standard Time
    "CEST",     "Etc/GMT-2",
    # Stockholm Sweden Time or Central European Time
    "CET",      "Etc/GMT-1",
    # Central Standard Time
    "CST",      "Etc/GMT+6",
    # Eastern Daylight Time
    "EDT",      "Etc/GMT+4",
    # Eastern Standard Time
    "EST",      "Etc/GMT+5",
    # Leave out GMT
    # Guam Standard Time Zone
    "GST",      "Etc/GMT-10",
  	# Hawaii-Aleutian Daylight Time
    "HADT",     "Etc/GMT+9",
    # Hawaii-Aleutian Standard Time
    "HAST",     "Etc/GMT+10",
    # Korea Standard Time
    "KST",      "Etc/GMT-9",
    # Mountain Daylight Time
    "MDT",      "Etc/GMT+6",
    # Mountain Standard Time
    "MST",      "Etc/GMT+7",
    # Newfoundland Daylight Time
    "NDT",      "Etc/GMT+2.5",
    # Newfoundland Standard Time
    "NST",      "Etc/GMT+3.5",
    # Pacific Daylight Time
    "PDT",      "Etc/GMT+7",
    # Pacific Standard Time
    "PST",      "Etc/GMT+8",
    # American Samoa Standard Time
    "SST",     "Etc/GMT+11",
    # Leave out UTC
    # Yukon Standard Time (retired)
    "YST",      "Etc/GMT+9",

    # Other tz codes we've encountered
    
    # Alaska-Hawaii Daylight Time
    "AHDT",     "Etc/GMT+9",
    # Chamorro Standard Time
    "ChST",     "Etc/GMT+10",
    # Hawaii–Aleutian Daylight Time
    "HDT",      "Etc/GMT+9",
    # Hawaii Standard Time
    "HST",      "Etc/GMT+10"
  )
  
  # 1. Complete the time zone records using lat/lon
  
  # Datum varies throughout the dataset; build a conversion table.
  epsg_codes <- tribble(
    ~datum, ~epsg,
    # American Samoa Datum
    "AMSMA", 4169,
    # Midway Astro 1961
    "ASTRO", 37224,
    # Guam 1963
    "GUAM", 4675,
    # High Accuracy Reference Network for NAD83
    "HARN", 4957,
    # Johnston Island 1961 (Spelled Johnson in WQX)
    "JHNSN", 6725,
    # North American Datum 1927
    "NAD27", 4267,
    # North American Datum 1983
    "NAD83", 4269,
    # Old Hawaiian Datum
    "OLDHI", 4135,
    # Assume WGS84
    "OTHER", 4326,
    # Puerto Rico Datum
    "PR", 4139,
    # St. George Island Datum
    "SGEOR", 4138,
    # St. Lawrence Island Datum
    "SLAWR", 4136,
    # St. Paul Island Datum
    "SPAUL", 4137,
    # Assume WGS84
    "UNKWN", 4326,
    # Wake-Eniwetok 1960
    "WAKE", 37229,
    # World Geodetic System 1972
    "WGS72", 4322,
    # World Geodetic System 1984
    "WGS84", 4326
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
  
  # Identify time zones for all unique lat/longs to fill gaps when needed. Note
  # that this is not perfect because the location isn't a guarantee of the
  # time zone that was used in recording. It's our best guess
  site_sf_unified$fetched_tz <- tz_lookup(site_sf_unified, method = "accurate")
  
  # Get clean site * tz data
  site_tz <- site_sf_unified %>%
    as_tibble() %>%
    distinct(MonitoringLocationIdentifier, fetched_tz)
  
  # Remove big object
  rm(site_sf_unified)
  gc()
  
  # Join fetched time zones to existing dataset as new column
  dataset_tz <- dataset %>%
    left_join(x = ., y = site_tz) %>%
    mutate(
      harmonized_tz = if_else(
        # Use fetched_tz if NA or not a recognizable tz. This is most often NA
        condition = is.na(ActivityStartTime.TimeZoneCode) |
          !(ActivityStartTime.TimeZoneCode %in% c(gmt_offsets$tz, OlsonNames())),
        true = fetched_tz,
        false = ActivityStartTime.TimeZoneCode
      )
    )
  
  rm(dataset)
  gc()
  
  # 2. Draft the local time column. Note that we also fill in blank
  # times or those with 00:00:00 with 11:59:59 AM (local), which we've found
  # is rarely reported in WQP data.
  dataset_keep_tz <- dataset_tz %>%
    mutate(
      # A column indicating local time (output class will be char)
      harmonized_local_time = case_when(
        # If has StartTime and StartDate then good unless 00:00:00. Paste them
        # together
        !is.na(ActivityStartDate) &
          !is.na(ActivityStartTime.Time) &
          (ActivityStartTime.Time != "00:00:00") ~ paste0(ActivityStartDate,
                                                          " ",
                                                          ActivityStartTime.Time),
        # If has StartDate and StartTime of 00:00:00 then reassign 11:59:59
        !is.na(ActivityStartDate) &
          !is.na(ActivityStartTime.Time) &
          (ActivityStartTime.Time == "00:00:00") ~ paste0(ActivityStartDate,
                                                          " 11:59:59"),
        # If StartDate only with no StartTime: assign 11:59:59
        !is.na(ActivityStartDate) &
          is.na(ActivityStartTime.Time) ~ paste0(ActivityStartDate, " 11:59:59"),
        # If nothing to go on, then NA record
        is.na(ActivityStartDate) & is.na(ActivityStartTime.Time) ~ NA_character_
      )
    )
  
  
  # Split-apply-combine over tz to allow temporary work with local times in
  # datetime format. Then create a local time column as a *character string*
  # for each separate tz so that the local times are retained somehow but can
  # also be stacked in the same data frame. This wouldn't work if they were in
  # a datetime format.
  dataset_local_list <- split(dataset_keep_tz, dataset_keep_tz$harmonized_tz)
  
  dataset_w_times <- dataset_local_list %>%
    map_df(
      .x = .,
      .f = ~{
        # Save the tz string for this group
        unique_tz <- unique(.x$harmonized_tz)
        
        # Check if ActivityStartTime.TimeZoneCode entries are UTC/GMT. Some are,
        # and we want a column of true local times, so these will need to be handled
        # differently.
        if(unique_tz %in% c("UTC", "GMT")) {
          .x %>%
            # We'll process these separately by their lat/lon acquired tz
            split(.$fetched_tz) %>%
            # For each different fetched_tz we assign a new harmonized_tz. This
            # takes us to the level of, e.g., "America/Chicago"
            map_df(
              .x = .,
              .f = ~ .x %>%
                mutate(
                  # If there's an 11:59:59 time stamp then we will force this
                  # to be 11:59:59 local time via fetched_tz. If it's not a
                  # "synthetic" time then we start in UTC/GMT and then convert
                  # it to local time.
                  harmonized_local_time = if_else(
                    condition = grepl(pattern = "11:59:59",
                                      x = harmonized_local_time),
                    true = ymd_hms(harmonized_local_time,
                                   tz = unique(fetched_tz)),
                    false = ymd_hms(harmonized_local_time,
                                    tz = unique_tz) %>%
                      with_tz(tzone = unique(fetched_tz))
                    # Both options above end up in the same local tz so
                    # it doesn't break the rule of multiple tz in a column
                  ),
                  # Grab the short time zone code, which includes DST info
                  new_tz = format(harmonized_local_time, "%Z"),
                  # Save a UTC version as our UTC column
                  harmonized_utc = with_tz(harmonized_local_time, tzone = "UTC")
                ) %>%
                # And now the challenge is that while all the records in each
                # group share a location-based tz like "America/Chicago" they
                # have the potential to vary by DST (e.g., "CST" vs "CDT") in
                # the new_tz column. So we once again split them up.
                split(.$new_tz) %>%
                map_df(.x = .,
                       .f = ~ .x %>%
                         mutate(
                           # Switch out the DST sensitive tz for its GMT offset
                           harmonized_tz = filter(gmt_offsets,
                                                  tz == unique(new_tz))$gmt,
                           # Get the character version of local time
                           harmonized_local_time = as.character(harmonized_local_time)
                         )
                )
            )
          
          # Now for the tz groups that aren't UTC/GMT.
          # First, those included in our GMT match up data frame:
        } else if(unique_tz %in% gmt_offsets$tz){
          
          # Short time zone codes (e.g., "CST") are not intelligible to R so we
          # need to convert the time zones when they are short codes. We
          # ultimately want to convert the local time zones into a GMT offset,
          # so we go right to that here:
          new_tz <- filter(gmt_offsets, tz == unique_tz)$gmt 
          
          .x %>%
            mutate(
              harmonized_local_time = ymd_hms(harmonized_local_time,
                                              tz = new_tz),
              harmonized_tz = new_tz,
              harmonized_utc = with_tz(harmonized_local_time, tzone = "UTC"),
              # Now convert local time to character so that it can be stacked
              # with other time zones
              harmonized_local_time = as.character(harmonized_local_time)
            )
          
          # Second, those that were filled by fetched_tz are in the longer
          # location-based format, which R understands but which isn't readily
          # converted to GMT offset to complete harmonization of the 
          # `harmonized_tz` column. It needs to be converted to short codes
          # (e.g. "EST" or "EDT") to reflect DST and THEN into the GMT offset.
          # This is similar to how we handled UTC/GMT above
        } else if(unique_tz %in% OlsonNames())
          
          .x %>%
          mutate(
            harmonized_local_time = ymd_hms(harmonized_local_time,
                                            tz = unique(harmonized_tz)),
            # Save a UTC version for our UTC column
            harmonized_utc = with_tz(harmonized_local_time, tzone = "UTC"),
            # Overwrite the harmonized_tz with the new short code
            new_tz = format(harmonized_local_time, "%Z")
          ) %>%
          # Now split along new_tz "short" codes
          split(.$new_tz) %>%
          map_df(.x = .,
                 .f = ~ .x %>%
                   mutate(
                     # Get the GMT offset code
                     harmonized_tz = filter(gmt_offsets, tz == unique(new_tz))$gmt,
                     # Get the character version of this column, with GMT offset
                     harmonized_local_time = as.character(harmonized_local_time)
                   )
          )
      }
    )
  
  # Return the final product without the temporary cols
  output_dataset <- dataset_w_times %>%
    select(-c(fetched_tz, new_tz))
  
  output_dataset
}

