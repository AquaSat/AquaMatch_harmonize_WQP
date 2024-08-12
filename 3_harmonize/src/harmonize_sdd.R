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
    # Turn every value into an absolute value
    mutate(ResultMeasureValue = abs(ResultMeasureValue)) %>% 
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
    "unable", "reject", "no data", "Not Reported", "no reading"
  )
  
  # Now get counts of fail-related string detections for each column: 
  fail_counts <- list("ResultCommentText", "ResultMeasureValue_original") %>%
    # need to check activity comment column ***
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
      if_all(.cols = c(ResultCommentText, ResultMeasureValue_original),
        .fns = ~
          !grepl(
            pattern = paste0(fail_text, collapse = "|"),
            x = .x,
            ignore.case = T
          )))
  
  # How many records removed due to fails language?
  print(
    paste0(
      "Rows removed due to fail-related language: ",
      nrow(sdd) - nrow(sdd_fails_removed)
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
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- sdd_fails_removed %>%
    # only want NAs in the altered value column and where there is a `<` and a number in the original values column... 
    filter(is.na(ResultMeasureValue) &
           grepl("<", ResultMeasureValue_original) &
           grepl("[0-9]", ResultMeasureValue_original)) %>%
    # Extract the first numeric value after '<' from ResultMeasureValue_original,
    # convert it to a number, and take its absolute value. This captures values 
    # like "<0.5" or "<-1.0", ignoring any text after the number, and ensures 
    # the result is positive.
    mutate(harmonized_value = abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),           # Preserve the units if they are provided 
           harmonized_units = ResultMeasure.MeasureUnitCode,
           harmonized_comments = "Approximated during MDL step") %>% 
    # keep important data
    select(index, harmonized_value, harmonized_units, harmonized_comments)
  
  print(
    paste(
      round(nrow(mdl_updates) / nrow(sdd_fails_removed) * 100, 3),
      "% of samples had values with '>' special character."
      )
    )
  
  # Replace "harmonized_value" field with these new values
  sdd_mdls_added <- sdd_fails_removed %>% 
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
    number_dropped = nrow(sdd_fails_removed) - nrow(sdd_mdls_added),
    n_rows = nrow(sdd_mdls_added),
    order = 3
  )
  
  # Clean up "greater than" values ------------------------------------------
  
  # Next step, incorporating "greater than" values. Using a similar approach to 
  # gap filling the MDL ("less than") values, we can gap fill the results that 
  # contain values greater than some amount.
  
  greater_vals <- sdd_mdls_added %>% 
    # First, remove the samples that we've already approximated:
    filter((!index %in% mdl_updates$index)) %>%
    # only want NAs in the altered value column and where there is a `<` or `=` and a number in the original values column... 
    filter(is.na(ResultMeasureValue) & 
            grepl(">|=", ResultMeasureValue_original) &
             grepl("[0-9]", ResultMeasureValue_original)) %>%
    # Extract the first numeric value after '>' from ResultMeasureValue_original,
    # convert it to a number, and take its absolute value. This captures values
    # like ">0.5" or ">-1.0" or "=-1.0" or "=1.0", ignoring any text after the number, and ensures
    # the result is positive.
    mutate(harmonized_value = abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),
      harmonized_units = ResultMeasure.MeasureUnitCode,
      harmonized_comments = "Approximated during 'greater than' step")
  
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
  sdd_harmonized_values <- sdd_mdls_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index, 
                                     greater_value, harmonized_value),
           harmonized_units = ifelse(index %in% greater_vals$index,
                                     ResultMeasure.MeasureUnitCode, harmonized_units),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        "Approximated during 'greater than' step",
                                        harmonized_comments))
  
  dropped_greater_than <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(sdd_mdls_added) - nrow(sdd_harmonized_values),
    n_rows = nrow(sdd_harmonized_values),
    order = 4
  )
  
  # Free up memory
  rm(sdd)
  gc()
  
  # Clean up approximated values/Gap fill harmonized values --------------------
  
  # Next step, incorporating "approximated" values. There are instances where
  # data is missing because it was put into one of the depth columns instead of
  # the result measure value column. We can approximate these values by using
  # the activity and bottom depth measure columns. 
  
  # For this process we are assuming that ResultMeasureValue has the most correct 
  # value and that ResultMeasureValue.UnitCode has the most correct units for an
  # observation. We will only attempt to gap fill the harmonized_value column with
  # information from the depth columns. We will never try to gap fill missing 
  # units. For the cases where there are missing values but not missing units,
  # we will fill in the missing values with the depth columns under certain 
  # conditions*. For the cases where there are missing values and missing units,
  # we will fill in both the missing values and units with the depth columns under
  # certain conditions*. We will always remove instances where the units are empty 
  # and the value is not empty. After this step, if there is no unit or value, the
  # row will be dropped during the NA removal step.
  
  # flag 0 = value not adjusted using and depth measure columns
  # flag 1 = value filled using activity depth measure column 
  # flag 2 = value filled using bottom depth measure column 
  
  comment_cols <- c("ActivityCommentText","ResultCommentText", "ResultMeasureValue_original")

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
  
  # Approximate Secchi Disk Depth (SDD) Calculation
  #
  # This code chunk creates a dataframe 'sdd_approx' containing indices of rows 
  # where Secchi Disk Depth (SDD) can be approximated using available depth measurements.
  #
  # The process involves:
  # 1. Selecting relevant columns from the 'sdd_harmonized_values' dataframe
  # 2. Cleaning and standardizing unit columns for consistency
  # 3. Filtering rows based on specific criteria:
  #    - Rows where harmonized_value is NA
  #    - Rows with valid depth measurements (either ActivityDepthHeightMeasure or ActivityBottomDepthHeightMeasure)
  #    - Excluding rows where both depth measurements are present and equal
  #    - Ensuring depth measurement units match harmonized units when applicable
  # 4. Selecting only the 'index' column for the final output
  #
  # The resulting 'sdd_approx' dataframe will be used to identify rows where SDD 
  # can be approximated, while non-included will be removed in subsequent steps
  # if they have missing values or units.

  sdd_approx <- sdd_harmonized_values %>%
    # subset data for efficiency
    select(index, ResultMeasureValue_original, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
           harmonized_value, harmonized_units, 
           ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode,
           ActivityBottomDepthHeightMeasure.MeasureValue, ActivityBottomDepthHeightMeasure.MeasureUnitCode,
           ResultCommentText, ActivityCommentText) %>%
    # clean the unit columns for consistency in comparisons 
    mutate(across(all_of(unit_cols), ~ { 
        . <- tolower(trimws(.))
        . <- gsub("feet", "ft", ., ignore.case = TRUE)
        . <- gsub("meters", "m", ., ignore.case = TRUE)
      })) %>%
    # Filter rows to include as many relevant conditions as possible
    filter(
      # INCLUDE harmonized_value is NA AND...
      is.na(harmonized_value) &
        # either ActivityDepthHeightMeasure OR... 
        ((!is.na(ActivityDepthHeightMeasure.MeasureValue) & !is.na(ActivityDepthHeightMeasure.MeasureUnitCode)) | 
        # ActivityBottomDepthHeightMeasure (and relevant unit columns) are not NA AND...
        (!is.na(ActivityBottomDepthHeightMeasure.MeasureValue) & !is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode))) &
        # EXCLUDE instances where both depth columns are present and are equal to each other, since we can't use that information AND...
        (is.na(ActivityDepthHeightMeasure.MeasureValue) | is.na(ActivityBottomDepthHeightMeasure.MeasureValue) |
           # handle NA comparisons returning FALSE
           ActivityDepthHeightMeasure.MeasureValue != ActivityBottomDepthHeightMeasure.MeasureValue) &
        # EXCLUDE cases where the depth columns to be used are not in the same units as the harmonized units
        ((!is.na(ActivityDepthHeightMeasure.MeasureUnitCode) | !is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode)) &
          (is.na(harmonized_units) |
            (is.na(ActivityDepthHeightMeasure.MeasureUnitCode) & ActivityBottomDepthHeightMeasure.MeasureUnitCode == harmonized_units) |
            (is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode) & ActivityDepthHeightMeasure.MeasureUnitCode == harmonized_units) |
            ActivityDepthHeightMeasure.MeasureUnitCode == harmonized_units |
            ActivityBottomDepthHeightMeasure.MeasureUnitCode == harmonized_units)
        )
    ) %>% 
    # Keep important information
    select(index)
    # anything not included in sdd_approx will be flagged as 0, and will be removed
    # during the NA removal step
    
  
  print(
    paste(
      round((nrow(sdd_approx)) / nrow(sdd_mdls_added) * 100, 3),
      "% of samples have missing numeric values or missing units that can be approximated from depth measure columns."
    )
  )
  
  # Approximation of Harmonized Values Using Depth Measurements
  #
  # This code chunk processes the 'sdd_mdls_added' dataframe to approximate 
  # missing harmonized values using available depth measurements. The process involves:
  #
  # 1. Creating tags for "bottom" and "negate bottom" language in comment columns
  # 2. Applying a set of rules to determine which depth measurement to use for approximation
  # 3. Generating an approximation flag to indicate the source of the approximated value
  # 4. Filling in missing harmonized values and units based on the approximation rules
  # 5. Adding comments to explain the approximation process for each affected row
  #
  # The approximation logic prioritizes:
  # - Using bottom depth when "bottom" language is present without negation
  # - Using activity depth when no "bottom" language is present or when negated
  # - Only approximating when both value and units are available for the chosen depth measure
  #
  # The resulting 'sdd_approx_added' dataframe includes:
  # - Approximated harmonized values and units where applicable
  # - An 'approx_flag' column indicating the source of approximation (0, 1, or 2)
  # - Updated 'harmonized_comments' explaining the approximation process
  
  # Approximate harmonized_value field with values from depth columns
  sdd_approx_added <- sdd_harmonized_values %>%
    mutate(
      # create a bottom tag to detect bottom language detected in relevant comment columns to be used for flagging
      bottom_tag = case_when(
        rowSums(across(all_of(comment_cols), ~grepl("bottom", .x, ignore.case = TRUE))) > 0 ~ 1,
        TRUE ~ 0
      ),
      # create a negate bottom tag to detect negate language in relevant comment columns
      negate_bottom_tag = case_when(
        rowSums(across(all_of(comment_cols), ~grepl(negate_bottom_text, .x, ignore.case = TRUE))) > 0 ~ 1,
        TRUE ~ 0
      )) %>%
    # so now we have indexes and a bottom tag. now we can gap fill with the depth columns
    # prefer to fill with bottom column if bottom language is indicated with no negate language
    # if activity and bottom values are equal, fill it in
    # if activity and bottom values are not equal, do NOT fill it in
    # if one or the other is present, use the one that is present
    # prefer to fill with activity column if bottom language is not indicated or there is negate language
    # can only gap fill when both value and units are present for the depth columns we are going to use
    mutate(
      # flag 0 = value and units not adjusted
      # flag 1 = value filled using activity depth measure column
      # flag 2 = value filled using activity bottom depth measure column
      approx_flag = case_when(
        # If the index is not in the sdd_approx$index, set flag to 0
        !index %in% sdd_approx$index ~ 0, 
        # If the index is in sdd_approx$index and there is bottom language and no negate language, and
        # ActivityBottomDepthHeightMeasure.MeasureValue is not NA set flag to 2
        index %in% sdd_approx$index &
          bottom_tag == 1 & 
          negate_bottom_tag == 0 & 
          !is.na(ActivityBottomDepthHeightMeasure.MeasureValue) ~ 2, 
        # If the index is in sdd_approx$index, and either bottom tag is 1 and negate bottom tag is 0 and
        # ActivityBottomDepthHeightMeasure.MeasureValue is NA, or bottom tag is 0 or negate bottom tag is 1,
        # and ActivityDepthHeightMeasure.MeasureValue is not NA, and either ResultMeasure.MeasureUnitCode is NA or
        # ResultMeasure.MeasureUnitCode is equal to ActivityDepthHeightMeasure.MeasureUnitCode, set flag to 1
        index %in% sdd_approx$index & 
          ((bottom_tag == 1 & negate_bottom_tag == 0 & is.na(ActivityBottomDepthHeightMeasure.MeasureValue)) | 
         bottom_tag == 0 | 
         negate_bottom_tag == 1) & 
          !is.na(ActivityDepthHeightMeasure.MeasureValue) &
          (is.na(ResultMeasure.MeasureUnitCode) | # should this be harmonized_units?
             ResultMeasure.MeasureUnitCode == ActivityDepthHeightMeasure.MeasureUnitCode) ~ 1,
        # For all other cases, set flag to NA
        .default = NA_integer_ # it is possible to get NA values here, but we will drop them in the next step
      ),
      harmonized_value = case_when(
        index %in% sdd_approx$index & approx_flag == 2 ~ as.numeric(ActivityBottomDepthHeightMeasure.MeasureValue),
        index %in% sdd_approx$index & approx_flag == 1 ~ as.numeric(ActivityDepthHeightMeasure.MeasureValue),
        .default = harmonized_value
      ),
      harmonized_units = case_when(
        index %in% sdd_approx$index & approx_flag == 2 ~ ActivityBottomDepthHeightMeasure.MeasureUnitCode,
        index %in% sdd_approx$index & approx_flag == 1 ~ ActivityDepthHeightMeasure.MeasureUnitCode,
        .default = harmonized_units
      ),
      harmonized_comments = case_when(
        approx_flag == 2 ~ "Value 'approximated' from `ActivityBottomDepthHeightMeasure.MeasureValue` columns.",
        approx_flag == 1 ~ "Value 'approximated' from `ActivityDepthHeightMeasure.MeasureValue` columns.",
        .default = NA_character_
      )
    )
  
  dropped_approximates <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while approximating values",
    short_reason = "Clean approximates",
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_harmonized_values),
    n_rows = nrow(sdd_approx_added),
    order = 5
  )
  
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
      round((nrow(sdd_no_na)) / nrow(sdd_approx_added) * 100, 2),
      "% of samples remain after removing NA values."
    )
  )
  
  dropped_na <- tibble(
    step = "sdd drop NAs",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_no_na),
    n_rows = nrow(sdd_no_na),
    order = 6
  )
  
  # Free up memory
  rm(sdd_harmonized_values, sdd_approx_added, sdd_mdls_added,
     sdd_fails_removed)
  gc()

  # Harmonize value units ---------------------------------------------------
  
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
                y = unit_conversion_table,
                by = "ResultMeasure.MeasureUnitCode") %>%
      count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
      plot_unit_pie() %>%
      ggsave(filename = "3_harmonize/out/sdd_unit_drop_pie.png",
            plot = .,
            width = 6, height = 6, units = "in", device = "png") 
             
  # How many records removed due to unit harmonization?
  print(
    paste0(
      "Rows removed while converting units: ",
      nrow(sdd_no_na) - nrow(converted_units_sdd)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "sdd unit conversion",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(sdd_no_na) - nrow(converted_units_sdd),
    n_rows = nrow(converted_units_sdd),
    order = 7
  )
  
  # Clean and flag bottom depth data -----------------------------------------------
  
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
      "% of samples have bottom depths that can be back filled with harmonized values."
    )
  )
  
  # Harmonize and gap fill ActivityBottomDepthHeightMeasure.MeasureValue field with these new values
  sdd_bottom_depth_added <- converted_units_sdd %>%
    mutate(
      # flag 0 = bottom depth value not adjusted and no indication that SD hit bottom
      # flag 1 = bottom depth value adjusted due to indication that SD hit bottom
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
        depth_flag == 0 ~ "bottom depth value not adjusted and no indication that SD hit bottom.", # need to make sure that comments are consistent
        depth_flag == 1 ~ "bottom depth value adjusted due to indication that SD hit bottom.",
        .default = NA_character_
      ),
      harmonized_bottom_depth_value = as.numeric(harmonized_bottom_depth_value) * conversion,
      harmonized_bottom_depth_unit = if_else(!is.na(harmonized_bottom_depth_unit), "m", NA),
      # fill in the rest of the harmonized depth columns with NA.
      harmonized_top_depth_value=NA,
      harmonized_top_depth_unit=NA,
      harmonized_discrete_depth_value=NA,
      harmonized_discrete_depth_unit=NA
    )
  
  dropped_depths <- tibble(
    step = "sdd bottom depth harmonization",
    reason = "Dropped rows while cleaning bottom depth values",
    short_reason = "Clean bottom depths",
    number_dropped = nrow(converted_units_sdd) - nrow(sdd_bottom_depth_added),
    n_rows = nrow(sdd_bottom_depth_added),
    order = 8
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
      "Rows unaltered due to unlikely analytical methods: ",
      round(nrow(sdd_relevant)/nrow(sdd_bottom_depth_added)*100,3)
    )
  )
  
  dropped_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_units_sdd) - nrow(sdd_relevant),
    n_rows = nrow(sdd_relevant),
    order = 9
  )
  
  # time_tag 
  sdd_time_tag <- sdd_relevant %>% 
    mutate(
      harmonized_local_time = as.POSIXct(harmonized_local_time, 
                                         format = "%Y-%m-%d %H:%M:%S", 
                                         tz = "UTC"), # to prevent time zone conversions
      time_tag = case_when(
        # tag 0: time is within 10a-2p and is not 11:59:59
        (format(harmonized_local_time, "%H:%M:%S") != "11:59:59" &
           hour(harmonized_local_time) >= 10 & hour(harmonized_local_time) < 14) ~ 0,
        # tag 1: time is not within 10a-2p and is not 11:59:59
        (format(harmonized_local_time, "%H:%M:%S") != "11:59:59" &
           (hour(harmonized_local_time) < 10 | hour(harmonized_local_time) >= 14)) ~ 1,
        # tag 2: time is 11:59:59 or NA
        (format(harmonized_local_time, "%H:%M:%S") == "11:59:59" | is.na(harmonized_local_time)) ~ 2,
        .default = NA_integer_
      ),
      harmonized_local_time = format(harmonized_local_time, "%Y-%m-%d %H:%M:%S") # return harmonized_local_time to characters
    )
  
  # scope_tag
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
  
  comment_cols <- c("ResultAnalyticalMethod.MethodName", "ActivityCommentText",
                    "ResultCommentText")
  
  sdd_scope_tag <- sdd_time_tag %>% 
    # tag: 0 = Either the method/comments/p-code indicate that a viewscope was used
    #      1 = P-code does not match viewscope, No indication that a viewscope was used, or there is negate language regarding viewscope used
    mutate(scope_tag = case_when(
      rowSums(across(all_of(comment_cols), ~grepl(negate_scope_text, .x, ignore.case = TRUE))) > 0  ~ 1, # negate language detected
      rowSums(across(all_of(comment_cols), ~grepl(scope_text, .x, ignore.case = TRUE))) > 0 | USGSPCode == 72187 ~ 0, # scope language detected or correct USGS code
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
  
  # Slim the tiered product (post flagging)
  
  # Confirm that no rows were lost during tiering
  if(nrow(sdd_relevant) != nrow(tiered_methods_sdd)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_field <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(sdd_bottom_depth_added) - nrow(tiered_methods_sdd),
    n_rows = nrow(tiered_methods_sdd),
    order = 10
  )
  
  rm(list = c("sdd_relevant", "sdd_time_tag", "sdd_scope_tag")) 
  gc()
  
  # Flag --------------------------------------------------------------------
  # We are going to flag everything that hasn't been flagged yet during this step
  # we are not going to worry about bottom language in this step because it is 
  # already in the bottom tag. if people care about these they can make their own
  # combinations of the tags that already exist
  
  # comment_cols 
  comment_cols <- c("ActivityCommentText", "ResultCommentText")
  # key field text
  adverse_text <- paste0(c("wind", "chop", "choppy", "precipitation", "rain", "ice"),
                      collapse = "|")
  # negate field text
  negate_adverse_text <- paste0(c("calm", "clear"), 
                      collapse = "|")
  
  flagged_sdd <- tiered_methods_sdd %>% 
    mutate(
      # MDL
      # Flag: 0 = value not adjusted and value is greater than 0.01
      #       1 = original NA value adjusted using MDL method
      #       2 = provided value below 0.01; not adjusted
      mdl_flag = case_when(
        (harmonized_value > 0.01) ~ 0,
        (harmonized_value <= 0.01) ~ 2,
        .default = NA_integer_
        ),
      greater_flag = case_when(
        # greater than
        # Flag: 0 = value less than 31m with NO special characters
        #       1 = value less than 31m with special characters
        #       2 = value greater than 31m with NO special characters
        #       3 = value greater than 31m with special characters
        (harmonized_value < 31 & !(index %in% greater_vals$index)) ~ 0,
        (harmonized_value < 31 & (index %in% greater_vals$index)) ~ 1,
        (harmonized_value >= 31 & !(index %in% greater_vals$index)) ~ 2,
        (harmonized_value >= 31 & (index %in% greater_vals$index)) ~ 3,
        .default = NA_integer_
      ),
      # create adverse language tags for field flag
      adverse_tag = if_else(rowSums(across(all_of(comment_cols), ~grepl(adverse_text, .x, ignore.case = TRUE))) > 0,
                            1,
                            0),
      negate_adverse_tag = if_else(rowSums(across(all_of(comment_cols), ~grepl(negate_adverse_text, .x, ignore.case = TRUE))) > 0,
                                   1,
                                   0),
      # field flag
      # Flag: 0 = negate adverse text present
      #       1 = neither negate key words nor key words present
      #       2 = key words related to adverse weather conditions
      field_flag = case_when(
        negate_adverse_tag == 1 ~ 0,
        adverse_tag == 0 & negate_adverse_tag == 0 ~ 1,
        adverse_tag == 1 & negate_adverse_tag != 1 ~ 2,
        .default = NA_integer_
      ),
      # miscellaneous flag
      # Flag: 0 = no misc flag
      #       1 = bottom indicated
      #       2 = Harmonized value > bottom value
      misc_flag = case_when(
        bottom_tag == 1 ~ 1,
        harmonized_value > ActivityBottomDepthHeightMeasure.MeasureValue ~ 2,
        .default = 0
      )
      ) 
  
  # Slim the tiered and completely flagged product
  cleaned_flagged_sdd <- flagged_sdd %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))
  
  # Print how many records removed due to assigning flags
  print(
    paste0(
      "Rows removed while assigning flags: ",
      nrow(cleaned_flagged_sdd) - nrow(tiered_methods_sdd)
    )
  )
  
  # Export a record of how records were flagged and their respective row counts
  flagging_record <- flagged_sdd %>%
    count(mdl_flag, greater_flag, field_flag, misc_flag) %>%
    arrange(desc(n)) 

  flagged_record_out_path <- "3_harmonize/out/sdd_flagged_record.csv"
  
  write_csv(flagging_record, flagged_record_out_path)
  
  dropped_field <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while assigning flags",
    short_reason = "Field flagging",
    number_dropped = nrow(cleaned_flagged_sdd) - nrow(tiered_methods_sdd),
    n_rows = nrow(cleaned_flagged_sdd),
    order = 11
  )
  
  rm(tiered_methods_sdd)
  gc()
  
  # Unrealistic values ------------------------------------------------------
  
  # We remove unrealistically high values prior to the final data export
  
  # Not applicable to SDD
  
  # generate plots with harmonized dataset -------------------------------------
  
  # We'll generate plots now before aggregating across simultaneous records
  # because it won't be possible to use CharacteristicName after that point.
  
  # Plot harmonized measurements by CharacteristicName
  
  plotting_subset <- cleaned_flagged_sdd %>% 
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
  
  # ggsave(filename = "../AquaMatch_harmonize_WQP_old/scratch/figures/sdd_charname_dists.png",
  ggsave(filename = "3_harmonize/out/sdd_charname_dists.png",
         plot = char_dists,
         width = 8, height = 6, units = "in", device = "png")
  
  # aggregate simultaneous records ------------------------------------------
  
  # There are true duplicate entries in the WQP or records with non-identical values recorded at the same time and place and by the same organization (field and/or lab replicates/duplicates)
  # We take the mean of those values here
  
  # First tag aggregate subgroups with group IDs
  
  grouped_sdd <- cleaned_flagged_sdd %>%
    group_by(parameter.x, OrganizationIdentifier, MonitoringLocationIdentifier,
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
    relocate(
      c(subgroup_id, harmonized_row_count, harmonized_units,
        harmonized_value, harmonized_value_cv, lat, lon, datum),
      .after = misc_flag
    )
  
  rm(grouped_sdd)
  gc()
  
  # Plot harmonized measurements by Tier:
  
  # 1. Harmonized values
  tier_dists <- no_simul_sdd %>% 
    select(tier, harmonized_value) %>% 
    mutate(plot_value = harmonized_value+0.001,
           tier_label = case_when(
             tier == 0 ~ "Restrictive (Tier 0)",
             tier == 1 ~ "Narrowed (Tier 1)",
             tier == 2 ~ "Inclusive (Tier 2)",
             tier == 3 ~ "Inclusive (Tier 3)"
           )) %>% 
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
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
           tier_label = case_when(
             tier == 0 ~ paste0("Restrictive (Tier 0) NAs removed: ", tier_0),
             tier == 1 ~ paste0("Narrowed (Tier 1) NAs removed: ", tier_1),
             tier == 2 ~ paste0("Inclusive (Tier 2) NAs removed: ", tier_2),
             tier == 3 ~ paste0("Inclusive (Tier 2) NAs removed: ", tier_3)
           )) %>% 
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized coefficient of variation, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized SDD CVs by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/sdd_tier_cv_dists_postagg.png",
         plot = tier_cv_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Rows removed while aggregating simultaneous records: ",
      nrow(cleaned_flagged_sdd) - nrow(no_simul_sdd)
    )
  )
  
  dropped_simul <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(cleaned_flagged_sdd) - nrow(no_simul_sdd),
    n_rows = nrow(no_simul_sdd),
    order = 12
  )
  
  # Export ---------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_media,
                                dropped_fails, dropped_mdls,
                                dropped_greater_than, dropped_approximates,
                                dropped_na, dropped_harmonization,
                                dropped_depths, dropped_methods,
                                dropped_field, dropped_simul)
  
  documented_drops_out_path <- "3_harmonize/out/sdd_harmonize_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/sdd_harmonized_final.csv"
  
  write_csv(no_simul_chla,
            data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(no_simul_chla)
    )
  )
  
  return(list(
    sdd_tiering_record_path = tiering_record_out_path,
    sdd_grouped_preagg_path = grouped_chla_out_path,
    sdd_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))
  
}