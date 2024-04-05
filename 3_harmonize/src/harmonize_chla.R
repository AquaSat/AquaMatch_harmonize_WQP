
harmonize_chla <- function(raw_chla, p_codes){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "chla harmonization",
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(raw_chla),
    order = 0
  )
  
  # Minor data prep ---------------------------------------------------------
  
  # Grab the column names of the dataset coming in
  raw_names <- names(raw_chla)
  
  # First step is to read in the data and do basic formatting and filtering
  chla <- raw_chla %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    # Filter out non-target media types
    filter(ActivityMediaSubdivisionName %in% c('Surface Water', 'Water', 'Estuary') |
             is.na(ActivityMediaSubdivisionName)) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "chla harmonization",
    reason = "Filtered for only specific water media",
    short_reason = "Target water media",
    number_dropped = nrow(raw_chla) - nrow(chla),
    n_rows = nrow(chla),
    order = 1
  )
  
  rm(raw_chla)
  gc()
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", 
    "improper", "instrument down", "interference", "invalid", "no result", 
    "no test", "not accept", "outside of accept", "problem", "QC EXCEEDED", 
    "questionable", "suspect", "unable", "violation", "reject", "no data"
  )
  
  # Now get counts of fail-related string detections for each column: 
  fail_counts <- list("ActivityCommentText", "ResultLaboratoryCommentText",
                      "ResultCommentText", "ResultMeasureValue_original") %>%
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
                 hit_count <- chla %>%
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
        .f = ~ ggsave(filename = paste0("3_harmonize/out/chla_",
                                        .y,
                                        "_fail_pie.png"),
                      plot = plot_fail_pie(dataset = .x, col_name = .y),
                      width = 6, height = 6, units = "in", device = "png"))
  
  
  # Now that the fails have been documented, remove them:
  chla_fails_removed <- chla %>%
    filter(
      if_all(.cols = c(ActivityCommentText, ResultLaboratoryCommentText,
                       ResultCommentText, ResultMeasureValue_original),
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
      nrow(chla) - nrow(chla_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(chla) - nrow(chla_fails_removed),
    n_rows = nrow(chla_fails_removed),
    order = 2)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  non_detect_text <- "non-detect|not detect|non detect|undetect|below"
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- chla_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(ResultMeasureValue)) %>%
    # if the value is NA BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(
        test = (is.na(ResultMeasureValue_original) & 
                  (grepl(non_detect_text, ResultLaboratoryCommentText, ignore.case = TRUE) | 
                     grepl(non_detect_text, ResultCommentText, ignore.case = TRUE) |
                     grepl(non_detect_text, ResultDetectionConditionText, ignore.case = TRUE))) |
          #.... OR, there is non-detect language in the value column itself....
          grepl(non_detect_text, ResultMeasureValue_original, ignore.case = TRUE),
        #... use the DetectionQuantitationLimitMeasure.MeasureValue value.
        yes = DetectionQuantitationLimitMeasure.MeasureValue,
        # if there is a `<` and a number in the values column...
        no = ifelse(test = grepl("[0-9]", ResultMeasureValue_original) &
                      grepl("<", ResultMeasureValue_original),
                    # ... use that number as the MDL
                    yes = str_replace_all(string = ResultMeasureValue_original,
                                          pattern = c("\\<"="", "\\*" = "", "\\=" = "" )),
                    no = NA)
      ),
      # preserve the units if they are provided:
      mdl_units = ifelse(!is.na(mdl_vals), 
                         DetectionQuantitationLimitMeasure.MeasureUnitCode, 
                         ResultMeasure.MeasureUnitCode),
      # zero = 0,
      half = as.numeric(mdl_vals) / 2)
  
  # Using the EPA standard for non-detects, select a random number between zero and HALF the MDL:
  mdl_updates$std_value <- with(mdl_updates, runif(nrow(mdl_updates), 0, half))
  mdl_updates$std_value[is.nan(mdl_updates$std_value)] <- NA
  
  # Keep important data
  mdl_updates <- mdl_updates %>%
    select(index, std_value, mdl_vals, mdl_units) %>%
    filter(!is.na(std_value))
  
  
  print(
    paste(
      round((nrow(mdl_updates)) / nrow(chla_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
    )
  )
  
  # Replace "harmonized_value" field with these new values
  chla_mdls_added <- chla_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, ResultMeasureValue),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, ResultMeasure.MeasureUnitCode),
           # Flag: 0 = value not adjusted and MDL not a concern
           #       1 = original NA value adjusted using MDL method
           #       2 = provided value below provided MDL; not adjusted
           mdl_flag = case_when(
             index %in% mdl_updates$index ~ 1,
             (!(index %in% mdl_updates$index) & DetectionQuantitationLimitMeasure.MeasureValue <= ResultMeasureValue) |
               (!(index %in% mdl_updates$index) & is.na(DetectionQuantitationLimitMeasure.MeasureValue)) ~ 0,
             DetectionQuantitationLimitMeasure.MeasureValue > ResultMeasureValue ~ 2,
             .default = NA_integer_
           )) %>%
    # Remove negative measurement values
    filter(harmonized_value >= 0)
  
  dropped_mdls <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(chla_fails_removed) - nrow(chla_mdls_added),
    n_rows = nrow(chla_mdls_added),
    order = 3
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  approx_text <- "result approx|RESULT IS APPROX|value approx"
  
  chla_approx <- chla_mdls_added %>%
    # First, remove the samples that we've already approximated using the EPA method:
    filter(!index %in% mdl_updates$index,
           # Then select fields where the numeric value column is NA....
           is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             # ...AND any of the comment fields have approximation language...
             (grepl(approx_text, ResultLaboratoryCommentText, ignore.case = T)|
                grepl(approx_text, ResultCommentText, ignore.case = T )|
                grepl(approx_text, ResultDetectionConditionText, ignore.case = T)))
  
  chla_approx$approx_value <- as.numeric(str_replace_all(chla_approx$ResultMeasureValue_original, c("\\*" = "")))
  chla_approx$approx_value[is.nan(chla_approx$approx_value)] <- NA
  
  # Keep important data
  chla_approx <- chla_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(chla_approx)) / nrow(chla_mdls_added) * 100, 3),
      '% of samples had values listed as approximated'
    )
  )
  
  # Replace harmonized_value field with these new values
  chla_approx_added <- chla_mdls_added %>%
    left_join(x = ., y = chla_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% chla_approx$index,
                                     approx_value,
                                     harmonized_value),
           # Flag: 1 = used approximate adjustment, 0 = value not adjusted
           approx_flag = ifelse(index %in% chla_approx$index, 1, 0))
  
  dropped_approximates <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(chla_mdls_added) - nrow(chla_approx_added),
    n_rows = nrow(chla_approx_added),
    order = 4
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  greater_vals <- chla_approx_added %>%
    filter((!index %in% mdl_updates$index) & (!index %in% chla_approx$index)) %>%
    # Then select fields where the NUMERIC value column is NA....
    filter(is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             #... AND a `>` symbol
             grepl(">", ResultMeasureValue_original))
  
  greater_vals$greater_value <- as.numeric(
    str_replace_all(
      greater_vals$ResultMeasureValue_original,
      c("\\>" = "", "\\*" = "", "\\=" = "" )))
  greater_vals$greater_value[is.nan(greater_vals$greater_value)] <- NA
  
  # Keep important data
  greater_vals <- greater_vals %>%
    select(greater_value, index)
  
  print(
    paste(
      round((nrow(greater_vals)) / nrow(chla_approx_added) * 100, 9),
      '% of samples had values listed as being above a detection limit//greater than'
    )
  )
  
  # Replace harmonized_value field with these new values
  chla_harmonized_values <- chla_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           # Flag: 1 = used greater than adjustment, 0 = value not adjusted
           greater_flag = ifelse(index %in% greater_vals$index, 1, 0))
  
  dropped_greater_than <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(chla_approx_added) - nrow(chla_harmonized_values),
    n_rows = nrow(chla_harmonized_values),
    order = 5
  )
  
  
  # Free up memory
  rm(chla)
  gc()
  
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, and values containing
  # symbols like ">". If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.
  
  chla_no_na <- chla_harmonized_values %>%
    filter(!is.na(harmonized_value))
  
  dropped_na <- tibble(
    step = "chla harmonization",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(chla_harmonized_values) - nrow(chla_no_na),
    n_rows = nrow(chla_no_na),
    order = 6
  )
  
  # Free up memory
  rm(chla_harmonized_values, chla_approx_added, chla_mdls_added,
     chla_fails_removed)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Matchup table for expected chla units in the dataset
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c("mg/l", "mg/L", "ppm", "ug/l", "ug/L",
                                      "mg/m3", "ppb", "mg/cm3", "ug/ml",
                                      "mg/ml", "ppt", "ug/mL", "mg/mL"),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000,
                   1000000, 1000000, 1000, 1000000))
  
  unit_table_out_path <- "3_harmonize/out/chla_unit_table.csv"
  
  write_csv(x = unit_conversion_table,
            file = unit_table_out_path)
  
  converted_units_chla <- chla_no_na %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = ResultMeasureValue * conversion,
           harmonized_units = "ug/L")
  
  # Plot and export unit codes that didn't make through joining
  chla_no_na %>%
    anti_join(x = .,
              y = unit_conversion_table,
              by = "ResultMeasure.MeasureUnitCode")  %>%
    count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
    plot_unit_pie() %>%
    ggsave(filename = "3_harmonize/out/chla_unit_drop_pie.png",
           plot = .,
           width = 6, height = 6, units = "in", device = "png")
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(chla_no_na) - nrow(converted_units_chla)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(chla_no_na) - nrow(converted_units_chla),
    n_rows = nrow(converted_units_chla),
    order = 7
  )
  
  
  # Clean and flag depth data -----------------------------------------------
  
  # Recode any error-related character values to NAs
  recode_depth_na_chla <- converted_units_chla %>%
    mutate(across(.cols = c(ActivityDepthHeightMeasure.MeasureValue,
                            ResultDepthHeightMeasure.MeasureValue,
                            ActivityTopDepthHeightMeasure.MeasureValue,
                            ActivityBottomDepthHeightMeasure.MeasureValue),
                  .fns = ~if_else(condition = .x %in% c("NA", "999", "-999",
                                                        "9999", "-9999", "-99",
                                                        "99", "NaN"),
                                  true = NA_character_,
                                  false = .x)))
  
  # Reference table for unit conversion
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  # There are four columns with potential depth data that we need to convert
  # into meters:
  converted_depth_units_chla <- recode_depth_na_chla %>%
    # 1. Activity depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_activity_depth_value = as.numeric(ActivityDepthHeightMeasure.MeasureValue) * depth_conversion
    ) %>%
    # Drop conversion col to avoid interfering with next join
    select(-depth_conversion) %>%
    # 2. Result depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ResultDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_result_depth_value = as.numeric(ResultDepthHeightMeasure.MeasureValue) * depth_conversion
    ) %>%
    select(-depth_conversion) %>%
    # 3. Activity top depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityTopDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_top_depth_value = as.numeric(ActivityTopDepthHeightMeasure.MeasureValue) * depth_conversion,
      harmonized_top_depth_unit = "m"
    ) %>%
    select(-depth_conversion) %>%
    # 4. Activity bottom depth col
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityBottomDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(
      harmonized_bottom_depth_value = as.numeric(ActivityBottomDepthHeightMeasure.MeasureValue) * depth_conversion,
      harmonized_bottom_depth_unit = "m"
    )
  
  # Now combine the two columns with single point depth data into one and clean
  # up values generally:
  harmonized_depth_chla <- converted_depth_units_chla %>%
    rowwise() %>%
    mutate(
      # New harmonized discrete column:
      harmonized_discrete_depth_value = case_when(
        # Use activity depth mainly
        !is.na(harmonized_activity_depth_value) &
          is.na(harmonized_result_depth_value) ~ harmonized_activity_depth_value,
        # Missing activity depth but not result depth
        is.na(harmonized_activity_depth_value) &
          !is.na(harmonized_result_depth_value) ~ harmonized_result_depth_value,
        # Disagreeing activity and result depths
        (!is.na(harmonized_activity_depth_value) &
           !is.na(harmonized_result_depth_value)) &
          harmonized_activity_depth_value != harmonized_result_depth_value ~ mean(
            c(harmonized_activity_depth_value, harmonized_result_depth_value)),
        # Both agree
        harmonized_activity_depth_value == harmonized_result_depth_value ~ harmonized_activity_depth_value,
        # Defaults to NA otherwise
        .default = NA_real_
      ),
      # Indicate depth unit going along with this column
      harmonized_discrete_depth_unit = "m"
    ) %>%
    ungroup()
  
  # Create a flag system based on depth data presence/completion
  flagged_depth_chla <- harmonized_depth_chla %>%
    mutate(
      depth_flag = case_when(
        # No depths (including because of recoding above)
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value) ~ 0,
        # All columns present
        !is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value) ~ 3,
        # Integrated depths
        (!is.na(harmonized_top_depth_value) |
           !is.na(harmonized_bottom_depth_value)) &
          is.na(harmonized_discrete_depth_value) ~ 2,
        # Discrete depths
        !is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value) ~ 1,
        # Discrete and integrated present
        # Note that here using the non-combined discrete col since part of
        # the combination process above was to create NAs when the discrete
        # values disagree
        ((!is.na(harmonized_activity_depth_value) | !is.na(harmonized_result_depth_value)) &
           !is.na(harmonized_top_depth_value)) |
          ((!is.na(harmonized_activity_depth_value) | !is.na(harmonized_result_depth_value)) &
             !is.na(harmonized_bottom_depth_value)) ~ 3,
        .default = NA_real_
      )) %>%
    # These columns are no longer necessary since the harmonization is done
    select(-c(harmonized_activity_depth_value, harmonized_result_depth_value,
              depth_conversion))
  
  # Sanity check that flags are matching up with their intended qualities:
  depth_check_table <- flagged_depth_chla %>%
    mutate(
      # Everything present
      three_cols_present = if_else(
        !is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only discrete present
      only_discrete = if_else(
        !is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only top present
      only_top = if_else(
        is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Only bottom present
      only_bottom = if_else(
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Full integrated present
      fully_integrated = if_else(
        is.na(harmonized_discrete_depth_value) &
          !is.na(harmonized_top_depth_value) &
          !is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # No depths present
      no_depths = if_else(
        is.na(harmonized_discrete_depth_value) &
          is.na(harmonized_top_depth_value) &
          is.na(harmonized_bottom_depth_value),
        true = 1, false = 0),
      # Discrete and one of the integrated
      discrete_partial_integ = if_else(
        (!is.na(harmonized_discrete_depth_value) &
           !is.na(harmonized_top_depth_value) &
           is.na(harmonized_bottom_depth_value)) |
          (!is.na(harmonized_discrete_depth_value) &
             is.na(harmonized_top_depth_value) &
             !is.na(harmonized_bottom_depth_value)),
        true = 1, false = 0)
    ) %>%
    count(three_cols_present, only_discrete, discrete_partial_integ,
          only_top, only_bottom, fully_integrated, no_depths, depth_flag) %>%
    arrange(depth_flag)
  
  depth_check_out_path <- "3_harmonize/out/chla_depth_check_table.csv"
  
  write_csv(x = depth_check_table,
            file = depth_check_out_path)
  
  # Depth category counts:
  depth_counts <- flagged_depth_chla %>%
    # Using a temporary flag to aggregate depth values for count output
    mutate(depth_agg_flag = case_when(
      depth_flag == 1 &
        harmonized_discrete_depth_value <= 1 ~ "<=1m",
      depth_flag == 1 &
        harmonized_discrete_depth_value <= 5 ~ "<=5m",
      depth_flag == 1 &
        harmonized_discrete_depth_value > 5 ~ ">5m",
      depth_flag == 2 &
        harmonized_bottom_depth_value <= 1 ~ "<=1m",
      depth_flag == 2 &
        harmonized_bottom_depth_value <= 5 ~ "<=5m",
      depth_flag == 2 &
        harmonized_bottom_depth_value > 5 ~ ">5m",
      .default = "No or inconsistent depth"
    )) %>%
    count(depth_agg_flag)
  
  depth_counts_out_path <- "3_harmonize/out/chla_depth_counts.csv"
  
  write_csv(x = depth_counts, file = depth_counts_out_path)
  
  # Have any records been removed while processing depths?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_chla) - nrow(flagged_depth_chla)
    )
  )
  
  dropped_depths <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning depths",
    short_reason = "Clean depths",
    number_dropped = nrow(converted_units_chla) - nrow(flagged_depth_chla),
    n_rows = nrow(flagged_depth_chla),
    order = 8
  )
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla analytical methods present: ",
      length(unique(flagged_depth_chla$ResultAnalyticalMethod.MethodName))
    )
  )
  
  # Before creating analytical tiers remove any records that have unrelated
  # data based on their method:
  unrelated_text <- paste0(c("sulfate", "sediment", "5310", "counting",
                             "plasma", "turbidity", "coliform", "carbon",
                             "2540", "conductance", "nitrate", "nitrite",
                             "nitrogen", "alkalin", "zooplankton",
                             "phosphorus", "periphyton", "peri",
                             "biomass", "temperature", "elemental analyzer",
                             "2320"),
                           collapse = "|")
  
  chla_relevant <- flagged_depth_chla %>%
    filter(!grepl(pattern = unrelated_text,
                  x = ResultAnalyticalMethod.MethodName,
                  ignore.case = TRUE))
  
  # How many records removed due to irrelevant analytical method?
  print(
    paste0(
      "Rows removed due to unrelated analytical methods: ",
      nrow(flagged_depth_chla) - nrow(chla_relevant)
    )
  )
  
  
  # 1.1 HPLC detection
  
  # Create a new column indicating detection of HPLC-related methods text: T or F
  hplc_text <- paste0(c("447", "chromatography", "hplc"),
                      collapse = "|") 
  
  chla_tag_hplc <- chla_relevant %>%
    # TRUE if methods contain HPLC-related text or if the correct USGS p code
    mutate(hplc_tag = if_else(condition = grepl(pattern = hplc_text,
                                                x = ResultAnalyticalMethod.MethodName,
                                                ignore.case = TRUE) |
                                USGSPCode %in% c(70951, 70953),
                              true = TRUE,
                              false = FALSE,
                              missing = FALSE))
  
  # 1.2 Spec/Fluor detection - new column
  
  # Create a new column indicating detection of non-HPLC lab-analyzed methods: T or F
  spec_fluor_text <- paste0(c("445", "fluor", "Welshmeyer", "fld", "10200",
                              "446", "trichromatic", "spectrophoto", "monochrom",
                              "monchrom",
                              # spec not as part of a word
                              "(?<!\\w)spec"),
                            collapse = "|")
  
  chla_tag_spec_fluor <- chla_tag_hplc %>%
    mutate(spec_fluor_tag = if_else(
      condition = grepl(pattern = spec_fluor_text,
                        x = ResultAnalyticalMethod.MethodName,
                        ignore.case = TRUE,
                        perl = TRUE) |
        parameter_code %in% c(32211, 32230),
      true = TRUE,
      false = FALSE,
      missing = FALSE))
  
  # 1.3 Correction for pheo detection - new column
  
  # Create a new column indicating whether the lab-analyzed chla data for spec
  # and fluor have been corrected for pheophytin
  chla_tag_pheo <- chla_tag_spec_fluor %>%
    mutate(pheo_corr_tag = 
             case_when(
               # If correction or correction-related methods mentioned in
               # analytical method OR...
               grepl(pattern = "\\bcorrect|in presence",
                     x = ResultAnalyticalMethod.MethodName,
                     ignore.case = TRUE) ~ TRUE,
               # The characteristic name mentions correction
               grepl(pattern = "\\bcorrected for pheophytin|free of pheophytin",
                     x = CharacteristicName,
                     ignore.case = TRUE) ~ TRUE,
               .default = FALSE))
  
  # 1.4 Create tiers
  tiered_methods_chla <- chla_tag_pheo %>%
    mutate(analytical_tier = case_when(
      # Conflicting method info
      grepl(pattern = "445", x = ResultAnalyticalMethod.MethodName) & hplc_tag ~ 2,
      # Top tier is HPLC = TRUE
      hplc_tag ~ 0,
      # Narrowed tier is non-HPLC lab analyzed AND corrected
      spec_fluor_tag & pheo_corr_tag ~ 1,
      USGSPCode == 32209 ~ 1,
      # No specific method listed but correction mentioned
      pheo_corr_tag ~ 1,
      # Everything else is inclusive tier
      .default = 2
    ))
  
  # Export a record of how methods were tiered and their respective row counts
  tiering_record <- tiered_methods_chla %>%
    count(CharacteristicName, ResultAnalyticalMethod.MethodName, USGSPCode,
          hplc_tag, spec_fluor_tag, pheo_corr_tag, analytical_tier) %>%
    arrange(desc(n)) 
  
  tiering_record_out_path <- "3_harmonize/out/chla_analytical_tiering_record.csv"
  
  write_csv(x = tiering_record, file = tiering_record_out_path)
  
  # Slim the tiered product
  cleaned_tiered_methods_chla <- tiered_methods_chla %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))
  
  # Confirm that no rows were lost during tiering
  if(nrow(chla_relevant) != nrow(cleaned_tiered_methods_chla)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_methods <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(flagged_depth_chla) - nrow(cleaned_tiered_methods_chla),
    n_rows = nrow(chla_relevant),
    order = 9
  )
  
  
  # Flag field methods ------------------------------------------------------
  
  # Strings to use in determining sampling procedure
  in_vitro_pattern <- paste0(c("grab", "bottle", "vessel", "bucket", "jar", "composite",
                               "integrate", "UHL001", "surface", "filter", "filtrat",
                               "1060B", "kemmerer", "collect", "rosette", "equal width",
                               "vertical", "van dorn", "bail", "sample",
                               "sampling",
                               # "lab" not in the middle of another word
                               "\\blab", 
                               # G on its own for "grab"
                               "^g$"),
                             collapse = "|")
  
  in_situ_pattern <- paste0(c("in situ", "probe", "ctd"),
                            collapse = "|")
  
  # Create a temporary tag column for use when comparing to methods
  tagged_sampling_chla <- cleaned_tiered_methods_chla %>%
    mutate(field_tag = case_when(
      grepl(pattern = in_vitro_pattern,
            x = SampleCollectionMethod.MethodName,
            ignore.case = TRUE,
            perl = TRUE) ~ "in vitro",
      grepl(pattern = in_situ_pattern,
            x = SampleCollectionMethod.MethodName,
            ignore.case = TRUE,
            perl = TRUE) ~ "in situ",
      .default = "unknown"
    )) 
  
  # Now flag whether the sampling method makes sense with the analytical method
  field_flagged_chla <- tagged_sampling_chla %>% 
    mutate(field_flag = case_when(
      # HPLC or other lab analysis + in vitro = expected methods
      analytical_tier %in% c(0, 1) & field_tag == "in vitro" ~ 0,
      # HPLC or other lab analysis + in situ = UNexpected methods
      analytical_tier %in% c(0, 1) & field_tag %in% c("in situ", "unknown") ~ 1,
      # The inclusive analytical tier is unknown
      analytical_tier == 2 ~ 2
    )) %>%
    select(-field_tag)
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed while assigning field flags: ",
      nrow(cleaned_tiered_methods_chla) - nrow(field_flagged_chla)
    )
  )
  
  dropped_field <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while assigning field flags",
    short_reason = "Field flagging",
    number_dropped = nrow(cleaned_tiered_methods_chla) - nrow(field_flagged_chla),
    n_rows = nrow(field_flagged_chla),
    order = 10
  )
  
  
  # Generate plots with harmonized dataset ----------------------------------
  
  # We'll generate plots now before aggregating across simultaneous records
  # because it won't be possible to use CharacteristicName after that point.
  
  # Plot harmonized measurements by CharacteristicName
  
  plotting_subset <- field_flagged_chla %>%
    select(CharacteristicName, USGSPCode, analytical_tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001)
  
  char_dists <- plotting_subset %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(CharacteristicName), scales = "free_y") +
    xlab(expression("Harmonized chl a (ug/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized chl a values by CharacteristicName",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/chla_charname_dists.png",
         plot = char_dists,
         width = 8, height = 6, units = "in", device = "png")
  
  
  # Aggregate simultaneous records ------------------------------------------
  
  # There are full duplicates and also values occurring at the same time, location,
  # etc. We take medians across them here
  
  # First tag aggregate subgroups with group IDs
  grouped_chla <- field_flagged_chla %>%
    group_by(parameter, OrganizationIdentifier, MonitoringLocationIdentifier,
             MonitoringLocationTypeName, ActivityStartDateTime,
             harmonized_top_depth_value, harmonized_top_depth_unit,
             harmonized_bottom_depth_value, harmonized_bottom_depth_unit,
             harmonized_discrete_depth_value, harmonized_discrete_depth_unit,
             depth_flag, mdl_flag, approx_flag, greater_flag,
             analytical_tier, field_flag, harmonized_units) %>%
    mutate(subgroup_id = cur_group_id())
  
  # Export the dataset with subgroup IDs for joining future aggregated product
  # back to original raw data
  grouped_chla_out_path <- "3_harmonize/out/chla_harmonized_grouped.feather"
  
  grouped_chla %>%
    select(
      all_of(c(raw_names,
               "parameter_code", "group_name", "parameter_name_description",
               "subgroup_id")),
      group_cols(),
      harmonized_value
    ) %>%
    write_feather(path = grouped_chla_out_path)
  
  # Now aggregate at the subgroup level to take care of simultaneous observations
  no_simul_chla <- grouped_chla %>%
    # Make sure we don't drop subgroup ID
    group_by(subgroup_id, .add = TRUE) %>%
    summarize(
      harmonized_row_count = n(),
      harmonized_value_sd = sd(harmonized_value),
      harmonized_value = median(harmonized_value)
    ) %>%
    ungroup()
  
  rm(grouped_chla)
  gc()
  
  # Plot harmonized measurements by Tier
  
  tier_dists <- no_simul_chla %>%
    select(analytical_tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001,
           tier_label = case_when(
             analytical_tier == 0 ~ "Restrictive (Tier 0)",
             analytical_tier == 1 ~ "Narrowed (Tier 1)",
             analytical_tier == 2 ~ "Inclusive (Tier 2)"
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized chl a (ug/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized chl a values by analytical tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/chla_tier_dists_postagg.png",
         plot = tier_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  
  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Rows removed while aggregating simultaneous records: ",
      nrow(field_flagged_chla) - nrow(no_simul_chla)
    )
  )
  
  dropped_simul <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(field_flagged_chla) - nrow(no_simul_chla),
    n_rows = nrow(no_simul_chla),
    order = 11
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_media,
                                dropped_fails, dropped_mdls,
                                dropped_approximates, dropped_greater_than,
                                dropped_na, dropped_harmonization,
                                dropped_depths, dropped_methods,
                                dropped_field, dropped_simul)
  
  documented_drops_out_path <- "3_harmonize/out/chla_harmonize_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/chla_harmonized_final.csv"
  
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
    chla_tiering_record_path = tiering_record_out_path,
    chla_grouped_preagg_path = grouped_chla_out_path,
    chla_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))  
}
