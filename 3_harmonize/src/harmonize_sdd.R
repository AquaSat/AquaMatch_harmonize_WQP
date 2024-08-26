harmonize_sdd <- function(raw_sdd, p_codes){

  # Starting values for dataset
  starting_data <- tibble(
    step = "sdd harmonization",
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(raw_sdd),
    order = 0
  )

  # Minor data prep ---------------------------------------------------------

  # Grab the column names of the dataset coming in
  raw_names <- names(raw_sdd)

  # First step is to read in the data and do basic formatting and filtering
  sdd <- raw_sdd %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    # Filter out non-target media types
    filter(ActivityMediaSubdivisionName %in% c("Surface Water", "Water", "Estuary") |
             is.na(ActivityMediaSubdivisionName)) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")

  # record info on any dropped rows
  dropped_media <- tibble(
    step = "sdd harmonization",
    reason = "Filtered for only specific water media",
    short_reason = "Target water media",
    number_dropped = nrow(raw_sdd) - nrow(sdd),
    n_rows = nrow(sdd),
    order = 1
  )

  rm(raw_sdd)
  gc()


  # Remove fails ------------------------------------------------------------

  # The values that will be considered fails for each column:
  fail_text <- c(
    "error", "fail", "invalid", "no result",  "questionable", "suspect",
    "unable", "reject", "no data", "Not Reported", "no reading", "-99",
    "upper quantitation limit"
  )

  # Now get counts of fail-related string detections for each column:
  # Note that ActivityCommentText not included because fail comments were mostly about non-SDD measurements
  fail_counts <- list("ResultCommentText", "ResultMeasureValue_original",
                      "ResultDetectionConditionText") %>%
    # Set list item names equal to each item in the list so that map will return
    # a named list
    set_names() %>%
    map(
      .x = .,
      .f = ~ {
        # Pass column name into the next map()
        col_name <- .x

        # Check each string pattern separately and count instances
        map_df(.x = fail_text,
               .f = ~{
                 hit_count <- sdd %>%
                   filter(grepl(pattern = .x,
                                x = !!sym(col_name),
                                ignore.case = TRUE)) %>%
                   nrow()

                 # Return two-col df
                 tibble(
                   word = .x,
                   record_count = hit_count
                 )
               }) %>%
          # Ignore patterns that weren't detected
          filter(record_count > 0)
      }) %>%
    # If there's any data frames with 0 rows (i.e., no fails detected) then
    # drop them to avoid errors in the next step. This has happened with
    # ResultMeasureValue in the past
    keep(~nrow(.) > 0)

  # Plot and export the plots as png files
  walk2(.x = fail_counts,
        .y = names(fail_counts),
        .f = ~ ggsave(filename = paste0("3_harmonize/out/sdd_",
                                        .y,
                                        "_fail_pie.png"),
                      plot = plot_fail_pie(dataset = .x, col_name = .y),
                      width = 6, height = 6, units = "in", device = "png"))

  # Now that the fails have been documented, remove them:
  sdd_fails_removed <- sdd %>%
    filter(
      if_all(.cols = c(ResultCommentText, ResultMeasureValue_original, ResultDetectionConditionText),
        .fns = ~
          !grepl(
            pattern = paste0(fail_text, collapse = "|"),
            x = .x,
            ignore.case = T
          )))

  # How many records removed due to fails language?
  print(
    paste0(
      "Percentage of rows removed due to fail-related language: ",
      round((nrow(sdd) - nrow(sdd_fails_removed)) / nrow(sdd) * 100, 3)
    )
  )

  dropped_fails <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(sdd) - nrow(sdd_fails_removed),
    n_rows = nrow(sdd_fails_removed),
    order = 2)

  rm(sdd)
  gc()

  # Clean up special characters ------------------------------------------------

  sdd_specialcharacters <- sdd_fails_removed %>%
    # Filter for special characters followed by numbers (integers or floats)
    filter(grepl("^[-=\\*]\\d+(\\.\\d+)?$", ResultMeasureValue_original)) %>% 
    select(index)
  
  sdd_specialcharacters_removed <- sdd_fails_removed %>%
    # Remove special characters from those rows that start with them
    mutate(ResultMeasureValue = if_else(index %in% sdd_specialcharacters$index,
                                        abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),
                                        ResultMeasureValue)
           )
  
  print(
    paste(
      round(nrow(sdd_specialcharacters) / nrow(sdd_fails_removed) * 100, 3),
      "% of samples had values with '-', '=', or '*' special character."
    )
  )
  
  dropped_specialcharacters <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning special characters",
    short_reason = "Clean special characters",
    number_dropped = nrow(sdd_fails_removed) - nrow(sdd_specialcharacters_removed),
    n_rows = nrow(sdd_specialcharacters_removed),
    order = 3
  )

  # Clean up MDLs --------------------------------------------------------------

  # Find MDLs and make them usable as numeric data
  mdl_updates <- sdd_specialcharacters_removed %>%
    # only want NAs in the altered value column and where there is a `<` and a number in the original values column...
    filter(is.na(ResultMeasureValue) &
           grepl("<", ResultMeasureValue_original) &
           grepl("[0-9]", ResultMeasureValue_original)) %>%
    # Extract the first numeric value after '<' from ResultMeasureValue_original and
    # convert it to a number. This captures values like "<0.5", ignoring any text after the number.
    # There are no instances of "<-" strings in the data, but an abs() is included
    # just in case.
    mutate(harmonized_value = abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),
           harmonized_units = ResultMeasure.MeasureUnitCode,
           harmonized_comments = "Approximated during MDL step") %>%
    # keep important data
    select(index, harmonized_value, harmonized_units, harmonized_comments)

  print(
    paste(
      round(nrow(mdl_updates) / nrow(sdd_specialcharacters_removed) * 100, 3),
      "% of samples had values with '>' special character."
      )
    )

  # Replace "harmonized_value" field with these new values
  sdd_mdls_added <- sdd_specialcharacters_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse((index %in% mdl_updates$index),
                                     harmonized_value, ResultMeasureValue),
           harmonized_units = ifelse((index %in% mdl_updates$index),
                                     harmonized_units, ResultMeasure.MeasureUnitCode),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated during 'MDLs' step",
                                        harmonized_comments))

  dropped_mdls <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(sdd_specialcharacters_removed) - nrow(sdd_mdls_added),
    n_rows = nrow(sdd_mdls_added),
    order = 4
  )

  # Free up memory
  rm(sdd_fails_removed, sdd_specialcharacters_removed)
  gc()

  # Clean up "greater than" values ------------------------------------------

  # Next step, incorporating "greater than" values. Using a similar approach to
  # gap filling the MDL ("less than") values, we can gap fill the results that
  # contain values greater than some amount.

  greater_vals <- sdd_mdls_added %>%
    # First, remove the samples that we've already approximated:
    filter((!index %in% mdl_updates$index)) %>%
    # only want NAs in the altered value column and where there is a `>` and a number in the original values column...
    filter(is.na(ResultMeasureValue) &
            grepl(">", ResultMeasureValue_original) &
             grepl("[0-9]", ResultMeasureValue_original)) %>%
    # Extract the first numeric value after '>' from ResultMeasureValue_original and
    # convert it to a number. This captures values like ">0.5", ignoring any text after 
    # the number, and ensures the result is positive.
    # There are no instances of ">-" strings in the data, but an abs() is included
    # just in case.
    mutate(harmonized_value = abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),
      harmonized_units = ResultMeasure.MeasureUnitCode,
      harmonized_comments = "Approximated during 'greater than' step")

  # this will cause coercion errors, which is expected, as some values have additional characters in
  # them that cause this step to have errors.
  greater_vals$greater_value <- as.numeric(
    str_replace_all(
      greater_vals$ResultMeasureValue_original,
      c("\\>" = "", "\\=" = "" )))

  greater_vals$greater_value[is.nan(greater_vals$greater_value)] <- NA

  # Keep important data
  greater_vals <- greater_vals %>%
    select(index, greater_value)

  print(
    paste(
      round((nrow(greater_vals)) / nrow(sdd_mdls_added) * 100, 3),
      "% of samples had values listed have `>` in the original value column."
    )
  )

  # Replace "harmonized_value" field with these new values
  sdd_greaterthan_added <- sdd_mdls_added %>% # renaming sdd_harmonized_values to sdd_greaterthan_added here
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           harmonized_units = ifelse(index %in% greater_vals$index,
                                     ResultMeasure.MeasureUnitCode, harmonized_units),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        "Approximated during 'greater than' step",
                                        harmonized_comments))

  dropped_greaterthan <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(sdd_mdls_added) - nrow(sdd_greaterthan_added),
    n_rows = nrow(sdd_greaterthan_added),
    order = 5
  )

  # Free up memory
  rm(sdd_mdls_added)
  gc()

  # Gap filling harmonized values --------------------
  
  # Gap fill missing values using depth columns when appropriate.

  gap_fill_comment_cols <- c("ActivityCommentText","ResultCommentText", "ResultMeasureValue_original")

  unit_cols <- c("ResultMeasure.MeasureUnitCode", "harmonized_units",
  "ActivityDepthHeightMeasure.MeasureUnitCode", "ActivityBottomDepthHeightMeasure.MeasureUnitCode")

  bottom_text <- "bottom"

  negate_bottom_text <- paste(
    "no bottom", "not.*bottom", "couldn't.*bottom", "could not.*bottom", "unable to.*bottom",
    "too deep", "too shallow", "doesn't reach", "did not reach", "blocked", "hidden", "disappeared",
    "hose frozen", "pump frozen", "cable not long enough", "cord not long enough", "won't stay on bottom",
    "current too strong", "too windy", "fast current", "flooding", "readings not on bottom",
    "measurements not collected", "sample taken at", "depth too great", "can't get true bottom", "seen on bottom: no",
    "bottom reading not taken", "afraid to touch bottom", "forgot bottom reading", "didn't hit bottom", "did not hit bottom",
    "vegetation blocks", "turbid", "probe not on bottom", "sonde.*not.*bottom", "hydrolab not on bottom",
    "profile not taken to bottom", "unable to collect bottom", "bottom.*not.*collected", "bottom.*not.*recorded",
    sep = "|")
  
  # Gap fill harmonized_value field with values from depth columns
  sdd_approx_added <- sdd_greaterthan_added %>%
    # clean the unit columns for consistency in comparisons
    mutate(
      across(all_of(unit_cols), ~ {
        . <- tolower(trimws(.))
        . <- gsub("feet", "ft", ., ignore.case = TRUE)
        . <- gsub("meters", "m", ., ignore.case = TRUE)
      }),
      # create a bottom tag to detect bottom language detected in relevant comment columns to be used for flagging
      bottom_tag = case_when(
        rowSums(across(all_of(gap_fill_comment_cols), ~grepl("bottom", .x, ignore.case = TRUE))) > 0 ~ 1,
        TRUE ~ 0
      ),
      # create a negate bottom tag to detect negate language in relevant comment columns
      negate_bottom_tag = case_when(
        rowSums(across(all_of(gap_fill_comment_cols), ~grepl(negate_bottom_text, .x, ignore.case = TRUE))) > 0 ~ 1,
        TRUE ~ 0
      ),
      # flag 0 = value and units not adjusted
      # flag 1 = value filled using activity depth measure column
      # flag 2 = value filled using activity bottom depth measure column
      approx_flag = case_when(
        # If the harmonized value is not NA, set flag to 0
        !is.na(harmonized_value) ~ 0,
        # If the harmonized value is NA and there is bottom language and no negate language, and
        # ActivityBottomDepthHeightMeasure.MeasureValue is not NA or 0 set flag to 2
        is.na(harmonized_value) &
          bottom_tag == 1 &
          negate_bottom_tag == 0 &
          !is.na(ActivityBottomDepthHeightMeasure.MeasureValue) &
          ActivityBottomDepthHeightMeasure.MeasureValue != 0 ~ 2,
        # If the harmonized value is NA and there is no bottom language or there is negate language, and
        # ActivityDepthHeightMeasure.MeasureValue is not NA or 0 set flag to 1
        is.na(harmonized_value) &
          (bottom_tag == 0 | negate_bottom_tag == 1) &
          !is.na(ActivityDepthHeightMeasure.MeasureValue) &
          ActivityDepthHeightMeasure.MeasureValue != 0~ 1,
        # For all other cases, set flag to NA
        .default = NA_integer_ # it is possible to get NA values here, but we will drop them in the next step
      ),
      harmonized_value = case_when(
        approx_flag == 2 ~ as.numeric(ActivityBottomDepthHeightMeasure.MeasureValue),
        approx_flag == 1 ~ as.numeric(ActivityDepthHeightMeasure.MeasureValue),
        .default = harmonized_value
      ),
      harmonized_units = case_when(
        approx_flag == 2 ~ ActivityBottomDepthHeightMeasure.MeasureUnitCode,
        approx_flag == 1 ~ ActivityDepthHeightMeasure.MeasureUnitCode,
        .default = harmonized_units
      ),
      harmonized_comments = case_when(
        approx_flag == 2 ~ "Value 'approximated' from `ActivityBottomDepthHeightMeasure.MeasureValue` columns.",
        approx_flag == 1 ~ "Value 'approximated' from `ActivityDepthHeightMeasure.MeasureValue` columns.",
        .default = NA_character_
      )
    )

  print(
    paste(
      round((nrow(filter(sdd_approx_added, approx_flag != 0))) / nrow(sdd_greaterthan_added) * 100, 3),
      "% of samples have missing numeric values or missing units that can be approximated from depth measure columns."
    )
  )

  dropped_approximates <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while approximating values",
    short_reason = "Clean approximates",
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_greaterthan_added),
    n_rows = nrow(sdd_approx_added),
    order = 6
  )
  
  
  rm(sdd_greaterthan_added)
  gc()

  # Remove remaining NAs ----------------------------------------------------

  # At this point we've processed MDLs, processed values containing
  # symbols like ">", and approximated values with the depth columns.
  # If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.

  # maintain harmonized values that are not NA
  sdd_no_na <- sdd_approx_added %>%
    # Keep rows if non-NA in harmonized values and units columns
    filter((!is.na(harmonized_value) & !is.na(harmonized_units))
    )

  print(
    paste(
      round((nrow(sdd_no_na)) / nrow(sdd_approx_added) * 100, 3),
      "% of samples remain after removing NA values."
    )
  )

  dropped_na <- tibble(
    step = "sdd drop NAs",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_no_na),
    n_rows = nrow(sdd_no_na),
    order = 7
  )

  # Free up memory
  rm(sdd_approx_added)
  gc()

  # Harmonize value units ------------------------------------------------------

  # Now count the units column:
  unit_counts <- sdd_no_na %>%
    count(harmonized_units) %>%
    arrange(desc(n))

  unit_conversion_table <- tibble(
    harmonized_units = c("m", "ft", "cm", "in", "mm", "feet", "meters", "mi"),
    conversion = c(1, 0.3048, 0.01, 0.0254, 0.001, 0.3048, 1, 1609.34)
  )

  unit_table_out_path <- "3_harmonize/out/sdd_unit_table.csv"

  write_csv(unit_conversion_table, unit_table_out_path)

  converted_units_sdd <- sdd_no_na %>%
    left_join(x = .,
               y = unit_conversion_table,
               by = "harmonized_units") %>%
    mutate(harmonized_value = as.numeric(harmonized_value) * conversion,
           harmonized_units = "m") %>%
    # filter for successful converted units and bottoms
    filter(!is.na(harmonized_value))

    # Plot and unit codes that didn't make through joining
    sdd_no_na %>%
      anti_join(x = .,
                y = converted_units_sdd,
                by = "ResultMeasure.MeasureUnitCode") %>%
      count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
      plot_unit_pie() %>%
      ggsave(filename = "3_harmonize/out/sdd_unit_drop_pie.png",
            plot = .,
            width = 6, height = 6, units = "in", device = "png")

  # How many records removed due to unit harmonization?
  print(
    paste0(
      "Percentage of rows removed while converting units: ", 
      round(((nrow(sdd_no_na)) - nrow(converted_units_sdd)/nrow(sdd_no_na)) * 100, 3)
    )
  )

  dropped_harmonization <- tibble(
    step = "sdd unit conversion",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(sdd_no_na) - nrow(converted_units_sdd),
    n_rows = nrow(converted_units_sdd),
    order = 8
  )

  # Clean and flag bottom depth data -------------------------------------------

  sdd_bottom_depth <- converted_units_sdd %>%
    # filter for samples that have bottom text in the comment columns and have no information in the bottom depth columns.
    filter(bottom_tag == 1 & negate_bottom_tag != 1 &
             (is.na(ActivityBottomDepthHeightMeasure.MeasureValue)&is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode)))

  # Keep important data
  sdd_bottom_depth <- sdd_bottom_depth %>%
    select(
      index, harmonized_value, harmonized_units,
      ActivityBottomDepthHeightMeasure.MeasureValue,
      ActivityBottomDepthHeightMeasure.MeasureUnitCode,
      bottom_tag, negate_bottom_tag
    )

  print(
    paste(
      round((nrow(sdd_bottom_depth)) / nrow(converted_units_sdd) * 100, 3),
      "% of samples have no reported ActivityBottomDepthHeightMeasure.MeasureValu. These will be 
      back filled with harmonized values where Secchi on bottom was indicated."
    )
  )

  # Harmonize and gap fill ActivityBottomDepthHeightMeasure.MeasureValue field with these new values
  sdd_bottom_depth_added <- converted_units_sdd %>%
    mutate(
      # flag 0 = bottom depth value not adjusted and no indication that SD hit bottom
      # flag 1 = bottom depth value filled in with harmonized_value due to indication that SD hit bottom
      depth_flag = case_when(
        !index %in% sdd_bottom_depth$index ~ 0,
        index %in% sdd_bottom_depth$index ~ 1,
        .default = NA_integer_
      ),
      harmonized_bottom_depth_value = if_else(
        depth_flag == 1, harmonized_value, as.numeric(ActivityBottomDepthHeightMeasure.MeasureValue)
      ),
      harmonized_bottom_depth_unit = if_else(
        depth_flag == 1, harmonized_units, as.character(ActivityBottomDepthHeightMeasure.MeasureUnitCode)
      ),
      harmonized_comments = case_when(
        depth_flag == 1 ~ "bottom depth value filled in with harmonized_value due to indication that SD hit bottom.",
        .default = NA_character_
      ),
      # fill in the rest of the harmonized depth columns with NA.
      harmonized_top_depth_value=NA,
      harmonized_top_depth_unit=NA,
      harmonized_discrete_depth_value=NA,
      harmonized_discrete_depth_unit=NA
    ) %>%
    rename(conversion_value = conversion) %>%
    left_join(x = .,
              y = unit_conversion_table,
              by = c("harmonized_bottom_depth_unit" = "harmonized_units")) %>%
    rename(conversion_bottom = conversion) %>%
    mutate(
      harmonized_bottom_depth_value = harmonized_bottom_depth_value * conversion_bottom,
      harmonized_bottom_depth_unit = if_else(!is.na(harmonized_bottom_depth_unit), "m", NA)
      )


  dropped_depths <- tibble(
    step = "sdd bottom depth harmonization",
    reason = "Dropped rows while cleaning bottom depth values",
    short_reason = "Clean bottom depths",
    number_dropped = nrow(converted_units_sdd) - nrow(sdd_bottom_depth_added),
    n_rows = nrow(sdd_bottom_depth_added),
    order = 9
  )


  # Aggregate and tier analytical methods -----------------------------------

  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of secchi analytical methods present: ",
      length(unique(sdd_bottom_depth_added$ResultAnalyticalMethod.MethodName))
    )
  )

  analytical_counts <- sdd_bottom_depth_added %>%
    count(ResultAnalyticalMethod.MethodName) %>%
    arrange(desc(n))

  unrelated_text <- paste0(c("chlorophyll", "sulfate", "sediment", "5310", "counting",
                             "plasma", "NTU", "nephelometry", "coliform", "carbon",
                             "2540", "conductance", "nitrate", "nitrite", "nitrogen",
                             "alkalin", "phosphorus", "periphyton", "peri", "biomass",
                             "temperature", "elemental analyzer", "2320", "chemical",
                             "unknown", "unspecified", "no information exists for method",
                             "other or unknown procedure", "laboratory calculation",
                             "2550","total suspended solids", "not available", "qa", "unkown",
                             "calculation", "10200G", "meteorological"),
                           collapse = "|")

  sdd_relevant <- sdd_bottom_depth_added %>%
    filter(!grepl(pattern = unrelated_text,
                  x = ResultAnalyticalMethod.MethodName,
                  ignore.case = TRUE) |
             # keep methods that are relevant but contain unrelated text
             grepl(pattern = paste0(c("Secchi Disk Depth Readings-Unspecified Details",
                                      "Secchi Disk-Unspecified cm"), collapse = "|"),
                   x = ResultAnalyticalMethod.MethodName,
                   ignore.case = TRUE)
           )

  # How many records removed due method harmonization?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(sdd_bottom_depth_added) - nrow(sdd_relevant)
    )
  )
  # Percentage of records removed due method harmonization?
  print(
    paste0(
      "Percentage of rows removed due to unlikely analytical methods: ",
      round((nrow(sdd_bottom_depth_added)-nrow(sdd_relevant))/nrow(sdd_bottom_depth_added)*100,3),
      " %"
    )
  )

  dropped_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_units_sdd) - nrow(sdd_relevant),
    n_rows = nrow(sdd_relevant),
    order = 10
  )

  # time_tag
  # Time tagging: Categorize timestamps to identify optimal conditions for Secchi disk depth measurements.
  # Tag 0 (10am-2pm) represents the ideal time range for measurements due to consistent lighting conditions.
  # This helps in assessing data quality and reliability of Secchi disk depth readings.
  sdd_time_tag <- sdd_relevant %>%
    mutate(
      harmonized_local_time_temp = as.POSIXct(harmonized_local_time,
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "UTC"), # prevent time zone conversions
      time_tag = case_when(
        # tag 0: time is within 10a-2p and is not 11:59:59
        (format(harmonized_local_time_temp, "%H:%M:%S") != "11:59:59" &
           hour(harmonized_local_time_temp) >= 10 & hour(harmonized_local_time_temp) < 14) ~ 0,
        # tag 1: time is not within 10a-2p and is not 11:59:59
        (format(harmonized_local_time_temp, "%H:%M:%S") != "11:59:59" &
           (hour(harmonized_local_time_temp) < 10 | hour(harmonized_local_time_temp) >= 14)) ~ 1,
        # tag 2: time is 11:59:59 or NA
        (format(harmonized_local_time_temp, "%H:%M:%S") == "11:59:59" | is.na(harmonized_local_time_temp)) ~ 2,
        .default = NA_integer_
      )
    ) %>% 
    select(-harmonized_local_time_temp) # remove temporary column

  # scope_tag
  # Scope tagging: Identify whether a viewscope was used during Secchi disk depth measurements.
  # Using a viewscope can significantly improve measurement accuracy by reducing surface glare 
  # and wave effects. This tagging helps in assessing the reliability and consistency of the 
  # Secchi disk depth data across different measurement conditions.
  scope_text <- "scope"

  negate_scope_text <- paste(
    "no.*viewscope", "no.*scope", "without.*scope","not using scope",
    "no scope.*used","viewscope.*not.*sent","viewscope.*not.*collected",
    "leaky.*scope","scope.*needs.*resealing","unable.*get.*readings.*scope",
    "hard to use.*scope","issues with viewscope","no readings.*viewscope",
    "no.*s\\.?d\\.?.*viewscope","secchi.*not using","Viewscope: No",
    "Viewscope used\\? - No","-Viewscope used\\? - No","Without Scope",
    "without viewscope","no viewscope used","Viewscope: No;",
    "scope of cable", "scope of collection", "scope of sample collection",
    sep = "|")

  scope_comment_cols <- c("ResultAnalyticalMethod.MethodName", "ActivityCommentText",
                    "ResultCommentText")

  sdd_scope_tag <- sdd_time_tag %>%
    # tag: 0 = Either the method/comments/p-code indicate that a viewscope was used
    #      1 = P-code does not match viewscope, No indication that a viewscope was used, or there is negate language regarding viewscope used
    mutate(scope_tag = case_when(
      rowSums(across(all_of(scope_comment_cols), ~grepl(negate_scope_text, .x, ignore.case = TRUE))) > 0  ~ 1, # negate language detected
      rowSums(across(all_of(scope_comment_cols), ~grepl(scope_text, .x, ignore.case = TRUE))) > 0 | USGSPCode == 72187 ~ 0, # scope language detected or correct USGS code
      .default = 1) # if negate language is not detected, scope language is not detected and the p-code does not match the one for view scope default to 1
    )

  # tier for SDD
  tiered_methods_sdd <- sdd_scope_tag %>%
    mutate(tier = case_when(
      # tier: 0 = Restrictive. Time reported within the allotted window and method indicates use of viewscope.
      #       1 = Narrowed. Either time reported within the allotted window or method indicates use of viewscope, but not both.
      #       2 = Inclusive. Time not reported/time reported outside of window, and no indication of viewscope used.
      #       3 = Inclusive. harmonized value gap filled with depth column.
      (time_tag == 0) & (scope_tag == 0) ~ 0,
      xor((time_tag == 0), (scope_tag == 0)) ~ 1,
      (time_tag != 0) & (scope_tag != 0) ~ 2,
      .default = 2
      ),
      tier = if_else(approx_flag != 0, 3, tier)
      )

  # Export a record of how methods were tiered and their respective row counts
  tiering_record <- tiered_methods_sdd %>%
    count(CharacteristicName, ResultAnalyticalMethod.MethodName, USGSPCode,
          time_tag, scope_tag, approx_flag, tier) %>%
    arrange(desc(n))

  tiering_record_out_path <- "3_harmonize/out/sdd_tiering_record.csv"
  
  write_csv(tiering_record,
            tiering_record_out_path)

  # Slim the tiered product (post flagging)

  # Confirm that no rows were lost during tiering
  if(nrow(sdd_relevant) != nrow(tiered_methods_sdd)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }

  dropped_tiers <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(sdd_bottom_depth_added) - nrow(tiered_methods_sdd),
    n_rows = nrow(tiered_methods_sdd),
    order = 11
  )

  rm(list = c("sdd_relevant", "sdd_time_tag", "sdd_scope_tag"))
  gc()

  # Flag --------------------------------------------------------------------
  # Now that we have cleaned and tiered our data we are able to conduct a final
  # flagging sweep. In this step we will create the mdl_flag, greater_flag, 
  # field_flag, and misc_flag.

  # comment_cols
  flag_comment_cols <- c("ActivityCommentText", "ResultCommentText")
  # environmental indicator text
  environmental_indicator <- paste0(c("wind", "chop", "choppy", "precipitation",
                                      "rain", "calm", "clear"),
                                    collapse = "|")

  flagged_sdd <- tiered_methods_sdd %>%
    mutate(
      # MDL
      # Flag: 0 = value greater than 0.01m with NO "<" character value
      #       1 = value greater than 0.01m with "<" character
      #       2 = value less than 0.01m with NO "<" character (these values will get mutated to be 0.01m)
      #       3 = value less than 0.01m with "<" character (these values will get mutated to be 0.01m)
      mdl_flag = case_when(
        (harmonized_value > 0.01 & !(index %in% mdl_updates$index)) ~ 0,
        (harmonized_value > 0.01 & (index %in% mdl_updates$index)) ~ 1,
        (harmonized_value <= 0.01 & !(index %in% mdl_updates$index)) ~ 2,
        (harmonized_value <= 0.01 & (index %in% mdl_updates$index)) ~ 3,
        .default = NA_integer_
        ),
      greater_flag = case_when(
        # greater than
        # Flag: 0 = value less than 31m with NO ">" character
        #       1 = value less than 31m with ">" character
        #       2 = value greater than 31m with NO ">" character
        #       3 = value greater than 31m with ">" character
        (harmonized_value < 31 & !(index %in% greater_vals$index)) ~ 0,
        (harmonized_value < 31 & (index %in% greater_vals$index)) ~ 1,
        (harmonized_value >= 31 & !(index %in% greater_vals$index)) ~ 2,
        (harmonized_value >= 31 & (index %in% greater_vals$index)) ~ 3,
        .default = NA_integer_
      ),
      # create adverse language tags for field flag
      environmental_indicator_tag = if_else(
        rowSums(across(all_of(flag_comment_cols), ~grepl(environmental_indicator, .x, ignore.case = TRUE))) > 0,
        1,
        0),
      # field flag
      # Flag: 0 = no environmental indicator present
      #       1 = environmental indicator present
      field_flag = case_when(
        environmental_indicator_tag == 0 ~ 0,
        .default = 1
      ),
      # miscellaneous flag
      # Flag: 0 = no misc flag
      #       1 = bottom indicated
      #       2 = Harmonized value > bottom value
      #       3 = Special character removed (-,=,*)
      misc_flag = case_when(
        bottom_tag == 1 & negate_bottom_tag == 0 ~ 1,
        harmonized_value > ActivityBottomDepthHeightMeasure.MeasureValue ~ 2,
        index %in% sdd_specialcharacters$index ~ 3,
        .default = 0
        )
      ) %>% 
    select(-harmonized_comments) # Offload the harmonized_comments column

  # Slim the tiered and completely flagged product
  cleaned_flagged_sdd <- flagged_sdd %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))

  # Print how many records removed due to assigning flags
  print(
    paste0(
      "Percentage of rows removed while assigning flags: ",
      round((nrow(tiered_methods_sdd) - nrow(cleaned_flagged_sdd)) / nrow(tiered_methods_sdd) * 100, 3),
      " %"
    )
  )

  # Export a record of how records were flagged and their respective row counts
  flagging_record <- cleaned_flagged_sdd %>%
    count(mdl_flag, greater_flag, field_flag, misc_flag) %>%
    arrange(desc(n))

  flagged_record_out_path <- "3_harmonize/out/sdd_flagged_record.csv"

  write_csv(flagging_record, flagged_record_out_path)

  dropped_flags <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while assigning flags",
    short_reason = "Field flagging",
    number_dropped = nrow(cleaned_flagged_sdd) - nrow(tiered_methods_sdd),
    n_rows = nrow(cleaned_flagged_sdd),
    order = 12
  )

  rm(tiered_methods_sdd, flagged_sdd)
  gc()

  # Realistic range of values ------------------------------------------------------
  
  # We exclude reported Secchi disk depth (SDD) measurements exceeding 62 meters from the final dataset.
  # This threshold is established based on the following rationale:
  # 1. SDD values greater than 31 meters are flagged as potentially anomalous in our quality control process.
  # 2. The 62-meter threshold represents a doubling of this initial flagging criterion.
  # 3. Measurements beyond this point are considered improbable and likely erroneous.
  # 4. Removing these extreme outliers enhances the overall reliability and interpretability of the dataset.
  # 5. This conservative approach aims to minimize the inclusion of potentially spurious data while 
  #    preserving legitimate measurements within expected ranges for typical aquatic environments.
  
  # We convert those values less than 0.01m to 0.01m.
  # This conversion is established because a human can't discern less than a 1cm resolution

  realistic_sdd <- cleaned_flagged_sdd %>%
    filter(harmonized_value <= 62) %>% 
    mutate(harmonized_value = if_else(mdl_flag %in% c(2,3), 0.01, harmonized_value))

  dropped_unreal <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows with unrealistic values",
    short_reason = "Unrealistic values",
    number_dropped = nrow(cleaned_flagged_sdd) - nrow(realistic_sdd),
    n_rows = nrow(realistic_sdd),
    order = 13
  )
  
  print(
    paste0(
      "Percentage of rows removed while removing unrealistic values: ",
      round((nrow(cleaned_flagged_sdd) - nrow(realistic_sdd)) / nrow(cleaned_flagged_sdd) * 100, 3),
      " %"
    )
  )
  
  print(
    paste0(
      "Percentage of rows where harmonized_value was converted to 0.01m: ",
      round((nrow(cleaned_flagged_sdd) - nrow(filter(realistic_sdd, mdl_flag %in% c(2,3)))) / nrow(cleaned_flagged_sdd) * 100, 3),
      " %"
    )
  )

  # generate plots with harmonized dataset -------------------------------------

  # We'll generate plots now before aggregating across simultaneous records
  # because it won't be possible to use CharacteristicName after that point.

  # Plot harmonized measurements by CharacteristicName

  plotting_subset <- realistic_sdd %>%
    select(CharacteristicName, USGSPCode, tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001)

  char_dists <- plotting_subset %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(CharacteristicName), scales = "free_y") +
    xlab(expression("Harmonized SDD (m, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized chl a values by CharacteristicName",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/sdd_charname_dists.png",
         plot = char_dists,
         width = 8, height = 6, units = "in", device = "png")

  # aggregate simultaneous records ------------------------------------------

  # There are true duplicate entries in the WQP or records with non-identical values recorded at the same time and place and by the same organization (field and/or lab replicates/duplicates)
  # We take the mean of those values here

  # First tag aggregate subgroups with group IDs

  grouped_sdd <- realistic_sdd %>%
    group_by(parameter, OrganizationIdentifier, MonitoringLocationIdentifier,
             MonitoringLocationTypeName, ResolvedMonitoringLocationTypeName,
             ActivityStartDate, ActivityStartTime.Time,
             ActivityStartTime.TimeZoneCode, harmonized_tz,
             harmonized_local_time, harmonized_utc, ActivityStartDateTime,
             harmonized_top_depth_value, harmonized_top_depth_unit,
             harmonized_bottom_depth_value, harmonized_bottom_depth_unit,
             harmonized_discrete_depth_value, harmonized_discrete_depth_unit,
             depth_flag, mdl_flag, approx_flag, greater_flag, tier, field_flag,
             misc_flag, harmonized_units) %>%
    mutate(subgroup_id = cur_group_id())

  # Export the dataset with subgroup IDs for joining future aggregated product
  # back to original raw data
  grouped_sdd_out_path <- "3_harmonize/out/sdd_harmonized_group.feather"

  grouped_sdd %>%
    select(all_of(c(raw_names,
                    "parameter_code", "group_name", "parameter_name_description",
                    "subgroup_id")),
           group_cols(),
           harmonized_value) %>%
    write_feather(path = grouped_sdd_out_path)

  # Now aggregate at the subgroup level to take care of simultaneous observations
  no_simul_sdd <- grouped_sdd %>%
    group_by(subgroup_id, .add = TRUE) %>%
    summarize(
      harmonized_row_count = n(),
      harmonized_value_sd = sd(harmonized_value),
      harmonized_value = mean(harmonized_value),
      lon = unique(lon),
      lat = unique(lat),
      datum = unique(datum)
    ) %>%
    # Calculate coefficient of variation as the standardard deviation divided by
    # the mena value (harmonized_value in this case)
    mutate(
      harmonized_value_cv = harmonized_value_sd/harmonized_value
    ) %>%
    ungroup() %>%
    select(
      # no longer needed
      -harmonized_value_sd) %>%
    relocate(misc_flag, .after = last_col()) %>% 
    relocate(
      c(subgroup_id, harmonized_row_count, harmonized_units,
        harmonized_value, harmonized_value_cv, lat, lon, datum),
      .after = misc_flag
    ) %>% 
    # round the final harmonized_value so we don't give false estimates of precision
    mutate(harmonized_value = round(harmonized_value, 2))

  rm(grouped_sdd)
  gc()

  # Plot harmonized measurements by Tier:

  # 1. Harmonized values
  tier_dists <- no_simul_sdd %>%
    select(tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value+0.001,
           tier = factor(tier, 
                               levels = c(0, 1, 2, 3), 
                               labels = c("Restrictive (Tier 0)", "Narrowed (Tier 1)", "Inclusive (Tier 2)", "Inclusive (Tier 3)")
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized SDD (m, " ~ log[10] ~ "transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized SDD values by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))

    ggsave(filename = "3_harmonize/out/sdd_tier_dists_postagg.png",
            plot = tier_dists,
            width = 6, height = 4, units = "in", device = "png")

  # 2: Harmonized CVs
  # Count NA CVs in each tier for plotting
  na_labels <- no_simul_sdd %>%
    filter(is.na(harmonized_value_cv)) %>%
    count(tier)

  tier_0 <- filter(na_labels, tier == 0)$n %>%
    number(scale_cut = cut_short_scale())

  tier_1 <- filter(na_labels, tier == 1)$n %>%
    number(scale_cut = cut_short_scale())

  tier_2 <- filter(na_labels, tier == 2)$n %>%
    number(scale_cut = cut_short_scale())

  tier_3 <- filter(na_labels, tier == 3)$n %>%
    number(scale_cut = cut_short_scale())

  tier_cv_dists <- no_simul_sdd %>%
    select(tier, harmonized_value_cv) %>%
    mutate(plot_value = harmonized_value_cv + 0.001,
           tier = factor(tier, 
                         levels = c(0, 1, 2, 3), 
                         labels = c("Restrictive (Tier 0)", "Narrowed (Tier 1)", "Inclusive (Tier 2)", "Inclusive (Tier 3)")
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized coefficient of variation, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized SDD CVs by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    theme_bw() +
    theme(strip.text = element_text(size = 7))

  ggsave(filename = "3_harmonize/out/sdd_tier_cv_dists_postagg.png",
         plot = tier_cv_dists,
         width = 6, height = 4, units = "in", device = "png")

  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Percentage of rows removed while aggregating simultaneous records: ",
      round((nrow(realistic_sdd) - nrow(no_simul_sdd)) / nrow(realistic_sdd) * 100, 3),
      " %"
    )
  )

  dropped_simul <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(realistic_sdd) - nrow(no_simul_sdd),
    n_rows = nrow(no_simul_sdd),
    order = 14
  )

  # Export ---------------------------------------------------------------------

  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_media,
                                dropped_fails, dropped_specialcharacters, 
                                dropped_mdls, dropped_greaterthan, 
                                dropped_approximates, dropped_na, 
                                dropped_harmonization, dropped_depths, 
                                dropped_methods, dropped_tiers,
                                dropped_flags, dropped_unreal, 
                                dropped_simul)

  documented_drops_out_path <- "3_harmonize/out/sdd_harmonize_dropped_metadata.csv"

  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)

  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/sdd_harmonized_final.csv"

  write_csv(no_simul_sdd,
            data_out_path)

  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(no_simul_sdd)
    )
  )

  return(list(
    sdd_tiering_record_path = tiering_record_out_path,
    sdd_grouped_preagg_path = grouped_sdd_out_path,
    sdd_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))

}