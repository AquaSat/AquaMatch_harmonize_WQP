# nolint start
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
    filter(ActivityMediaSubdivisionName %in% c('Surface Water', 'Water', 'Estuary') |
             is.na(ActivityMediaSubdivisionName)) %>% 
    # Turn every value into an absolute value
    mutate(ResultMeasureValue = abs(ResultMeasureValue)) %>% # this will create NAs in non-numeric instances that we will try to gap fill
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
        .f = ~ ggsave(filename = paste0("../AquaMatch_harmonize_WQP_old/scratch/figures/sdd_", # update path
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
  
  # not using non-detect text because all of them in ResultCommentText were unrelated to SDD
  # and there was only one instance of it being used in ResultMeasureValue
  
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
    mutate(harmonized_value = ifelse((index %in% mdl_updates$index), harmonized_value, ResultMeasureValue),
           harmonized_units = ifelse((index %in% mdl_updates$index), harmonized_units, ResultMeasure.MeasureUnitCode))
    # Flag post unit harmonization
    
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
    # only want NAs in the altered value column and where there is a `<` and a number in the original values column... 
    filter(is.na(ResultMeasureValue) & 
            grepl(">|\\*|=", ResultMeasureValue_original) &
             # grepl(">", ResultMeasureValue_original) &
             grepl("[0-9]", ResultMeasureValue_original)) %>%
    # Extract the first numeric value after '>' from ResultMeasureValue_original,
    # convert it to a number, and take its absolute value. This captures values
    # like ">0.5" or ">-1.0", ignoring any text after the number, and ensures
    # the result is positive.
    mutate(harmonized_value = abs(as.numeric(str_extract(ResultMeasureValue_original, "-?\\d+(\\.\\d+)?"))),
      harmonized_units = ResultMeasure.MeasureUnitCode,
      harmonized_comments = "Approximated during 'greater than' step")
  
  greater_vals$greater_value <- as.numeric(
    str_replace_all(
      greater_vals$ResultMeasureValue_original,
      c("\\>" = "", "\\*" = "", "\\=" = "" )))
  
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
  # Replace harmonized_value field with these new values
  # flag after these values have been harmonized
    # flag 0 = value did not contain any special characters and was less than 31m after harmonizing
    # flag 1 = value contained special characters, but was less than 31m after harmonizing
    # flag 2 = value did not contain special characters, but was greater than 31m after harmonizing
    # flag 3 = value contained special characters, and was greater than 31m after harmonizing
  
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
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating "approximated" values. There are instances where
  # data is missing because it was put into one of the depth columns instead of
  # the result measure value column. We can approximate these values by using
  # the activity and bottom depth measure columns. 
  
  # For this process we are assuming that ResultMeasureValue has the most correct 
  # value and that ResultMeasureValue.UnitCode has the most correct units for an
  # observation. We will only attempt to gap fill the harmonized_value column with
  # information from the depth columns. We will never try to gap fill missing 
  # units. For the cases where there are missing values but not missing units,
  # we will fill in the missing values with the depth columns under specific 
  # conditions***. For the cases where there are missing values and missing units,
  # we will fill in both the missing values and units with the depth columns under
  # specific conditions***. We will always remove instances where the units are empty 
  # and the value is not empty. After this step, if there is no unit or value, the
  # row will be dropped during the NA removal step.
  
  # The approximate flag is different here because:
  # There is no approximated text in the values column and the approx language
  # in the comments are not related to secchi. There are two instances of approx text
  # actually being related to secchi. We are going to drop those instances. ignoring 
  # "*" because there is 5 counts of it being stored with numeric data
  
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
  
  # Now we will find the indexes that will be gap filled with the depth columns
    # keep columns that have NA values
    # keep columns that have NA values and NA units
    # ignore columns that have values but NA units
  
    # we will also check the depth columns
      # keep rows where either both the activity and bottom depth columns have values and units
  
  # At this point we have filled in some harmonized values, so we want to check 
  # for those harmonized values that are still NA

  sdd_approx <- sdd_harmonized_values %>%
    # subset data for efficiency
    select(index, ResultMeasureValue_original, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
           harmonized_value, harmonized_units, 
           ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode,
           ActivityBottomDepthHeightMeasure.MeasureValue, ActivityBottomDepthHeightMeasure.MeasureUnitCode,
           ResultCommentText, ActivityCommentText) %>%
    # clean the unit columns for consistency in comparisons
    mutate(across(all_of(unit_cols), ~ { # we will have to do this later as well
        . <- tolower(trimws(.))
        . <- gsub("feet", "ft", ., ignore.case = TRUE)
        . <- gsub("meters", "m", ., ignore.case = TRUE)
      })) %>%
    # Filter rows to include as many relevant conditions as possible
    filter(
      # INCLUDE harmonized_value is NA AND...
      is.na(harmonized_value) &
        # either ActivityDepthHeightMeasure or ActivityBottomDepthHeightMeasure (and relevant unit columns) are not NA AND...
        ((
          !is.na(ActivityDepthHeightMeasure.MeasureValue) &
            !is.na(ActivityDepthHeightMeasure.MeasureUnitCode)
        ) |
          (
            !is.na(ActivityBottomDepthHeightMeasure.MeasureValue) &
              !is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode)
          )) &
        # EXCLUDE instances where both depth columns are present and are equal to each other, since we can't use that information AND...
        (
          is.na(ActivityDepthHeightMeasure.MeasureValue) |
            is.na(ActivityBottomDepthHeightMeasure.MeasureValue) |
            # handle NA comparisons returning FALSE
            ActivityDepthHeightMeasure.MeasureValue != ActivityBottomDepthHeightMeasure.MeasureValue
        ) &
        # EXCLUDE cases where the depth columns to be used are not in the same units as the harmonized units
        ((
          !is.na(ActivityDepthHeightMeasure.MeasureUnitCode) |
            !is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode)
        ) &
          (
            is.na(harmonized_units) |
              (
                is.na(ActivityDepthHeightMeasure.MeasureUnitCode) &
                  ActivityBottomDepthHeightMeasure.MeasureUnitCode == harmonized_units
              ) |
              (
                is.na(ActivityBottomDepthHeightMeasure.MeasureUnitCode) &
                  ActivityDepthHeightMeasure.MeasureUnitCode == harmonized_units
              ) |
              ActivityDepthHeightMeasure.MeasureUnitCode == harmonized_units |
              ActivityBottomDepthHeightMeasure.MeasureUnitCode == harmonized_units
          )
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
  
  # Approximate harmonized_value field with values from depth columns
  sdd_approx_added <- sdd_mdls_added %>%
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
        # If the index is in sdd_approx$index, and either bottom tag is 1 and negate bottom tag is 0 but
        # ActivityBottomDepthHeightMeasure.MeasureValue is NA, or bottom tag is 0 or negate bottom tag is 1,
        # and ActivityDepthHeightMeasure.MeasureValue is not NA, and either ResultMeasure.MeasureUnitCode is NA or
        # ResultMeasure.MeasureUnitCode is equal to ActivityDepthHeightMeasure.MeasureUnitCode, set flag to 1
        index %in% sdd_approx$index & 
          ((bottom_tag == 1 & negate_bottom_tag == 0 & is.na(ActivityBottomDepthHeightMeasure.MeasureValue)) | 
         bottom_tag == 0 | 
         negate_bottom_tag == 1) & 
          !is.na(ActivityDepthHeightMeasure.MeasureValue) ~ 1,
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
  
  # need to check what kinds of observations were removed ***

  # Harmonize value units ---------------------------------------------------
  
  # Now count the units column: 
  unit_counts <- sdd_no_na %>%
    count(harmonized_units) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    harmonized_units = c("m", "ft", "cm", "in", "mm", "feet", "meters", "mi"),
    conversion = c(1, 0.3048, 0.01, 0.0254, 0.001, 0.3048, 1, 1609.34)
  )
  
  converted_units_sdd <- sdd_no_na %>%
    left_join(x = .,
               y = unit_conversion_table,
               by = "harmonized_units") %>%
    mutate(harmonized_value = as.numeric(harmonized_value) * conversion,
           harmonized_units = "m") %>% 
    # filter for successful converted units and bottoms
    filter(!is.na(harmonized_value))
    # keep rows if there are indications of bottom in the comments
    # grepl(bottom_text, ResultMeasureValue_original, ignore.case=T) |
    # grepl(bottom_text, ResultCommentText, ignore.case=T) |
    # grepl(bottom_text, ActivityCommentText, ignore.case=T
  
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
  
  # Clean and flag depth data -----------------------------------------------
  
  bottom_text <- paste(
    "secchi hit bottom",
    "secchi hit the bottom",
    "secchi disk hit bottom",
    "secchi disc hit bottom",
    "secchi disk hit the bottom",
    "secchi disc hit the bottom",
    "S. disk hit bottom",
    "S. disc hit bottom",
    "bottom",
    "on bottom",
    "Secchi disc measure clear to bottom depth",
    "to bottom",
    sep = "|"
  )
  
  negate_bottom_text <- paste0(
    "Seen on Bottom: No",
    sep = "|")
  
  sdd_bottom_depth <- sdd_harmonized_values %>%
    # filter for samples that have bottom text in the comment columns and have information in the relevant depth columns.
    filter(
      grepl(bottom_text, ResultMeasureValue_original, ignore.case=T) |
        grepl(bottom_text, ResultCommentText, ignore.case=T) |
        grepl(bottom_text, ActivityCommentText, ignore.case=T)) %>% 
    filter(!is.na(ActivityBottomDepthHeightMeasure.MeasureValue)|!is.na(ActivityDepthHeightMeasure.MeasureValue)) 
  
  # Keep important data
  sdd_bottom_depth <- sdd_bottom_depth %>%
    select(
      index, ResultMeasureValue_original, ResultMeasureValue, 
      ResultMeasure.MeasureUnitCode, harmonized_value, harmonized_units,
      ResultCommentText, ActivityCommentText,
      ActivityBottomDepthHeightMeasure.MeasureValue, 
      ActivityBottomDepthHeightMeasure.MeasureUnitCode,
      ActivityDepthHeightMeasure.MeasureValue,
      ActivityDepthHeightMeasure.MeasureUnitCode
    )
  
  print(
    paste(
      round((nrow(sdd_bottom_depth)) / nrow(sdd_harmonized_values) * 100, 3),
      "% of samples have bottom text that can be reconciled with activity depth columns."
    )
  )
  
  # Replace harmonized_value field with these new values
  sdd_bottom_depth_added <- sdd_harmonized_values %>%
    mutate(
      # flag 0 = value not adjusted and no indication that SD hit bottom (this includes negate text) 
      # flag 1 = value and units not adjusted, but there was bottom text (this includes when values are NA)
      # flag 2 = value or units adjusted and there was bottom text
      # where the fill came in from seems to me less important, unless there were values for both depth and bottom depth, but this is never the case
      # other flags later ***
      bottom_flag = case_when(
        !index %in% sdd_bottom_depth$index ~ 0,
        index %in% sdd_bottom_depth$index & is.na(harmonized_value) | is.na(harmonized_units) ~ 2,
        index %in% sdd_bottom_depth$index & !is.na(harmonized_value) & !is.na(harmonized_units) ~ 1,
        .default = NA_integer_
      ),
      harmonized_value = if_else(
        index %in% sdd_bottom_depth$index & is.na(harmonized_value),
        if_else(!is.na(ActivityDepthHeightMeasure.MeasureValue),
                ActivityDepthHeightMeasure.MeasureValue,
                ActivityBottomDepthHeightMeasure.MeasureValue),
        harmonized_value
      ),
      harmonized_units = if_else(
        index %in% sdd_bottom_depth$index & is.na(harmonized_units),
        if_else(!is.na(ActivityDepthHeightMeasure.MeasureUnitCode),
                ActivityDepthHeightMeasure.MeasureUnitCode,
                ActivityBottomDepthHeightMeasure.MeasureUnitCode),
        harmonized_units
      ),
      harmonized_comments = case_when(
        bottom_flag == 2 ~ "Adjustment to value, and bottom indicated in comments.",
        bottom_flag == 1 ~ "No adjustment to value, but bottom indicated in comments.",
        .default = NA_character_
      )
    )
  
  dropped_bottoms <- tibble( # update this ***
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning bottom depth values",
    short_reason = "Clean bottom depths",
    number_dropped = nrow(sdd_harmonized_values) - nrow(sdd_bottom_depth_added),
    n_rows = nrow(sdd_approx_added),
    order = 4
  )
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of secchi analytical methods present: ",
      length(unique(converted_units_sdd$ResultAnalyticalMethod.MethodName))
    )
  )
  
  analytical_counts <- converted_units_sdd %>%
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
  
  sdd_relevant <- converted_units_sdd %>%
    filter(!grepl(pattern = unrelated_text,
                  x = ResultAnalyticalMethod.MethodName,
                  ignore.case = TRUE))
  
  # How many records removed due method harmonization?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_units_sdd) - nrow(sdd_relevant)
    )
  )
  
  dropped_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_units_sdd) - nrow(sdd_relevant),
    n_rows = nrow(sdd_relevant),
    order = 7
  )
  
  # time_tag ***
  sdd_time_tag <- sdd_relevant
  
  # scope_tag
  scope_text <- paste0(c("scope", "viewscope", "viewscope: yes", 
                         "\\-Viewscope used\\? \\- yes", "viewscope used: yes",
                         "viewscope used"),
                       collapse = "|")
  
  negate_scope_text <- paste0(c("viewscope: no", "no viewscope", 
                                "\\-Viewscope used\\? \\- No", 
                                "scope measurements lost", 
                                "no scope", "without viewscope",
                                "not using scope", "viewscope used: no"), # scope leaks?
                              collapse = "|")
  
  comment_cols <- c("ResultAnalyticalMethod.MethodName", "ActivityCommentText",
                    "ResultCommentText", "ResultMeasureValue_original")
  
  sdd_scope_tag <- sdd_time_tag %>% 
    # tag: 0 = Either the method/comments/p-code indicate that a viewscope was used
    #      1 = P-code does not match viewscope, No indication that a viewscope was used, or there is negate language regarding viewscope used
    mutate(scope_tag = case_when(
      rowSums(across(all_of(comment_cols), ~grepl(negate_scope_text, .x, ignore.case = TRUE))) > 0  ~ 1, # negate language detected
      rowSums(across(all_of(comment_cols), ~grepl(scope_text, .x, ignore.case = TRUE))) > 0 | USGSPCode == 72187 ~ 0, # scope language detected or correct USGS code
      TRUE ~ 1) # if negate language is not detected, scope language is not detected and the p-code does not match the one for view scope default to 1
    )
  
  # tier for SDD
  tiered_methods_sdd <- sdd_scope_tag %>% 
    mutate(analytical_tier = case_when(
      # flag: 0 = Restrictive. Time reported within the allotted window and method indicates use of viewscope.
      #       1 = Narrowed. Either time reported within the allotted window or method indicates use of viewscope, but not both.
      #       2 = Inclusive. Time not reported/time reported outside of window, and no indication of viewscope used.
      (time_tag == 0) & (scope_tag == 0) ~ 0,
      xor((time_tag == 0), (scope_tag == 0)) ~ 1,
      (time_tag != 0) & (scope_tag == 1) ~ 2,
      .default = 2
    ))
  
  # Slim the tiered product
  cleaned_tiered_methods_sdd <- tiered_methods_sdd %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))
  
  # Confirm that no rows were lost during tiering
  if(nrow(sdd_relevant) != nrow(cleaned_tiered_methods_sdd)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(flagged_depth_sdd) - nrow(cleaned_tiered_methods_sdd),
    n_rows = nrow(sdd_relevant),
    order = 9
  )
  
  rm(list = c("sdd_relevant", "flagged_depth_sdd", "sdd_time_tag", "sdd_scope_tag", "tiered_methods_sdd"))
  gc()
  
  # flag --------------------------------------------------------------------
  # We are going to flag everything that hasn't been flagged yet during this step
  
  flagged_sdd <- cleaned_tiered_methods_sdd %>% 
    # MDL
    # Flag: 0 = value not adjusted and MDL not a concern
    #       1 = original NA value adjusted using MDL method
    #       2 = provided value below provided MDL; not adjusted
    mutate(mdl_flag = case_when(
        (harmonized_value > 0.01) ~ 0,
        (harmonized_value < 0.01) ~ 2,
        .default = NA_integer_
      )) 
  # approximated value
  # greater than
  
  # depth data
  # field methods
  
  # generate plots ----------------------------------------------------------
  
  # aggregate simultaneous records ------------------------------------------
  
  # export ------------------------------------------------------------------
}
# nolint end