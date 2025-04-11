
harmonize_tss <- function(raw_tss, p_codes){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "tss harmonization",
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(raw_tss),
    order = 0
  )
  
  # Minor data prep ---------------------------------------------------------
  
  # Grab the column names of the dataset coming in
  raw_names <- names(raw_tss)
  
  # First step is to read in the data and do basic formatting and filtering
  tss <- raw_tss %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    # Filter out non-target media types
    filter(ActivityMediaSubdivisionName %in% c("Surface Water", "Water", "Estuary") |
             is.na(ActivityMediaSubdivisionName)) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index") %>%
    # Create a persistent identifier of SSC vs TSS
    mutate(parameter = case_when(
      CharacteristicName == "Suspended Sediment Concentration (SSC)" ~ "ssc",
      CharacteristicName == "Total suspended solids" ~ "tss"
    ))
  
  if(any(is.na(tss$parameter))){
    stop("Unexpected values generated when classifying parameters by CharacteristicName.")
    }
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "tss harmonization",
    reason = "Filtered for only specific water media",
    short_reason = "Target water media",
    number_dropped = nrow(raw_tss) - nrow(tss),
    n_rows = nrow(tss),
    order = 1
  )
  
  rm(raw_tss)
  gc()
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", "improper",
    "interference", "invalid", "no result", "no test", "not accept",
    "outside of accept", "problem", "questionable", "suspect", "unable",
    "violation", "reject", "no data", "time exceed", "value extrapolated",
    "exceeds", "biased", "parameter not required", "not visited"
  )
  
  # Now get counts of fail-related string detections for each column: 
  fail_counts <- list("ActivityCommentText", "ResultLaboratoryCommentText",
                      "ResultCommentText", "ResultMeasureValue_original",
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
                 hit_count <- tss %>%
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
        .f = ~ ggsave(filename = paste0("3_harmonize/out/tss_",
                                        .y,
                                        "_fail_pie.png"),
                      plot = plot_fail_pie(dataset = .x, col_name = .y),
                      width = 6, height = 6, units = "in", device = "png"))
  
  
  # Now that the fails have been documented, remove them:
  tss_fails_removed <- tss %>%
    filter(
      if_all(.cols = c(ActivityCommentText, ResultLaboratoryCommentText,
                       ResultCommentText, ResultMeasureValue_original,
                       ResultDetectionConditionText),
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
      nrow(tss) - nrow(tss_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(tss) - nrow(tss_fails_removed),
    n_rows = nrow(tss_fails_removed),
    order = 2)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # non_detect_text <- "non-detect|not detect|non detect|undetect|below"
  non_detect_text <- "non-detect|not detect|non detect|undetect|below|Present <QL"
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- tss_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(ResultMeasureValue)) %>%
    # if the value is NA BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(
        test = (is.na(ResultMeasureValue_original) &
                  (grepl(non_detect_text, ResultLaboratoryCommentText, ignore.case = TRUE) | 
                     grepl(non_detect_text, ResultCommentText, ignore.case = TRUE) |
                     grepl(non_detect_text, ResultDetectionConditionText, ignore.case = TRUE))) |
          #.... OR, there is non-detect language in the value column itself /
          # it's labeled as ND
          (
            grepl(non_detect_text, ResultMeasureValue_original, ignore.case = TRUE) |
              ResultMeasureValue_original == "ND" |
              ResultMeasureValue_original == "nd"
          ),
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
      round((nrow(mdl_updates)) / nrow(tss_fails_removed) * 100, 1),
      "% of samples had values listed as being below a detection limit"
    )
  )
  
  # Replace "harmonized_value" field with these new values
  tss_mdls_added <- tss_fails_removed %>%
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
             .default = 0
           ))
  
  dropped_mdls <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(tss_fails_removed) - nrow(tss_mdls_added),
    n_rows = nrow(tss_mdls_added),
    order = 3
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  approx_text <- "result approx|RESULT IS APPROX|value approx"
  
  tss_approx <- tss_mdls_added %>%
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
  
  tss_approx$approx_value <- as.numeric(str_replace_all(tss_approx$ResultMeasureValue_original, c("\\*" = "")))
  tss_approx$approx_value[is.nan(tss_approx$approx_value)] <- NA
  
  # Keep important data
  tss_approx <- tss_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(tss_approx)) / nrow(tss_mdls_added) * 100, 3),
      "% of samples had values listed as approximated"
    )
  )
  
  # Replace harmonized_value field with these new values
  tss_approx_added <- tss_mdls_added %>%
    left_join(x = ., y = tss_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% tss_approx$index,
                                     approx_value,
                                     harmonized_value),
           # Flag: 1 = used approximate adjustment, 0 = value not adjusted
           approx_flag = ifelse(index %in% tss_approx$index, 1, 0))
  
  dropped_approximates <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(tss_mdls_added) - nrow(tss_approx_added),
    n_rows = nrow(tss_approx_added),
    order = 4
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  # Next step, incorporating and flagging "greater than" values. Using a similar
  # approach to the previous two flags, we can identify results that 
  # contain values greater than some amount
  
  greater_vals <- tss_approx_added %>%
    # First, remove the samples that we've already approximated:
    filter((!index %in% mdl_updates$index) & (!index %in% tss_approx$index)) %>%
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
      round((nrow(greater_vals)) / nrow(tss_approx_added) * 100, 9),
      "% of samples had values listed as being above a detection limit//greater than"
    )
  )
  
  # Replace harmonized_value field with these new values
  tss_harmonized_values <- tss_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           # Flag: 1 = used greater than adjustment, 0 = value not adjusted
           greater_flag = ifelse(index %in% greater_vals$index, 1, 0))
  
  dropped_greater_than <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(tss_approx_added) - nrow(tss_harmonized_values),
    n_rows = nrow(tss_harmonized_values),
    order = 5
  )
  
  # Free up memory
  rm(tss)
  gc()
  
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, and values containing
  # symbols like ">". If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.
  
  tss_no_na <- tss_harmonized_values %>%
    filter(
      !is.na(harmonized_value),
      # Some negative values can be introduced by the previous NA parsing steps:
      harmonized_value >= 0
    )
  
  dropped_na <- tibble(
    step = "tss harmonization",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(tss_harmonized_values) - nrow(tss_no_na),
    n_rows = nrow(tss_no_na),
    order = 6
  )
  
  # Free up memory
  rm(tss_harmonized_values, tss_approx_added, tss_mdls_added,
     tss_fails_removed)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Matchup table for expected tss units in the dataset
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c("mg/l", "mg/L", "ppm", "ug/l", "ug/L",
                                      "mg/m3", "ppb", "mg/cm3", "ug/ml",
                                      "mg/ml", "ppt", "ug/mL", "mg/mL",
                                      "g/L"),
    conversion = c(1, 1, 1, .001, .001, .001, .001, 1000, 1,
                   1000, 1000, 1, 1000, 1000))
  
  unit_table_out_path <- "3_harmonize/out/tss_unit_table.csv"
  
  write_csv(x = unit_conversion_table,
            file = unit_table_out_path)
  
  converted_units_tss <- tss_no_na %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = harmonized_value * conversion,
           harmonized_units = "mg/L")
  
  # Plot and export unit codes that didn't make through joining
  tss_no_na %>%
    anti_join(x = .,
              y = unit_conversion_table,
              by = "ResultMeasure.MeasureUnitCode")  %>%
    count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
    plot_unit_pie() %>%
    ggsave(filename = "3_harmonize/out/tss_unit_drop_pie.png",
           plot = .,
           width = 6, height = 6, units = "in", device = "png")
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(tss_no_na) - nrow(converted_units_tss)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(tss_no_na) - nrow(converted_units_tss),
    n_rows = nrow(converted_units_tss),
    order = 7
  )
  
  
  # Clean and flag depth data -----------------------------------------------
  
  # Recode any error-related character values to NAs
  recode_depth_na_tss <- converted_units_tss %>%
    mutate(
      across(.cols = c(ActivityDepthHeightMeasure.MeasureValue,
                       ResultDepthHeightMeasure.MeasureValue,
                       ActivityTopDepthHeightMeasure.MeasureValue,
                       ActivityBottomDepthHeightMeasure.MeasureValue),
             .fns = ~if_else(condition = .x %in% c("NA", "999", "-999",
                                                   "9999", "-9999", "-99",
                                                   "99", "NaN"),
                             true = NA_character_,
                             false = .x))
    )
  
  # Reference table for unit conversion
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  # There are four columns with potential depth data that we need to convert
  # into meters:
  converted_depth_units_tss <- recode_depth_na_tss %>%
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
  harmonized_depth_tss <- converted_depth_units_tss %>%
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
  flagged_depth_tss <- harmonized_depth_tss %>%
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
  depth_check_table <- flagged_depth_tss %>%
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
  
  depth_check_out_path <- "3_harmonize/out/tss_depth_check_table.csv"
  
  write_csv(x = depth_check_table,
            file = depth_check_out_path)
  
  # Depth category counts:
  depth_counts <- flagged_depth_tss %>%
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
  
  depth_counts_out_path <- "3_harmonize/out/tss_depth_counts.csv"
  
  write_csv(x = depth_counts, file = depth_counts_out_path)
  
  # Have any records been removed while processing depths?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_tss) - nrow(flagged_depth_tss)
    )
  )
  
  dropped_depths <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while cleaning depths",
    short_reason = "Clean depths",
    number_dropped = nrow(converted_units_tss) - nrow(flagged_depth_tss),
    n_rows = nrow(flagged_depth_tss),
    order = 8
  )
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of tss analytical methods present: ",
      length(unique(flagged_depth_tss$ResultAnalyticalMethod.MethodName))
    )
  )
  
  # Before creating tiers remove records that have clearly unrelated or unreliable
  # data based on their method:
  unrelated_text <- paste0(
    c("10200", "150.1", "2340", "2550", "4500", "9222", "9223", "Alkalinity", 
      "Chlorophyll", "DO NOT USE", "Mercury", "Nitrate", "Nitrogen", 
      "Oxygen", "Phosphorus", "Temperature", "Silica"),
    collapse = "|")
  
  tss_relevant <- flagged_depth_tss %>%
    filter(!grepl(pattern = unrelated_text,
                  x = ResultAnalyticalMethod.MethodName,
                  ignore.case = TRUE))
  
  # Also remove instances where there's no flow in a stream or river
  tss_flow <- tss_relevant %>%
    filter(
      !(
        # No flow
        grepl(pattern = "no flow|not flow|zero flow", x = ActivityCommentText, ignore.case = TRUE) &
          # River or stream
          grepl(pattern = "river|stream|canal", x = MonitoringLocationTypeName, ignore.case = TRUE)
      )
    )
  
  # How many records removed due to irrelevant analytical methods / no flow?
  print(
    paste0(
      "Rows removed due to unrelated analytical methods or no flow: ",
      nrow(flagged_depth_tss) - nrow(tss_flow)
    )
  )
  
  # Flag relevant conditions for tiering:
  
  # Comment mentions deep depth
  deep_text <- paste0(
    c("Activity Relative Depth: Bottom", "Activity Relative Depth: Near Bottom",
      "Activity Relative Depth: Midwater", "Relative Depth = Below Thermocline",
      "hypolimnion"),
    collapse = "|"
  )
  
  # Low flow flag
  low_flow_text <- paste0(
    c("low flow", "no flow", "no visible flow", "low stream flow", "Flow: Low",
      "Not flowing", "low visible flow", "slow flow", "BELOW NORMAL", "Gentle flow",
      "NO DISCERNIBLE FLOW", "Low base flow"),
    collapse = "|"
  )
  
  # ~NA methods or equipment flag
  na_text <- paste0(
    c("Not Available", "Unkn", "UNKOWN", "Historic",
      "not specified", "N/A Calculation", "LEGACY", "CALCULATION", "Unspecified",
      "Calculated", "Lab Method", "See Comments", "Field Office procedures"),
    collapse = "|"
  )
  
  # Methods expected for TSS
  correct_text <- paste0(
    c("2540", "160", "ASTM", "14B", "8006", "108"),
    collapse = "|")
  
  # USGS P codes expected for TSS/SSC
  correct_pcodes <- paste0(
    c("00530", "80154", "70299", "70293", "69613", "69586", "69587", 
      "69588", "69584", "69581", "69582", "69585", "69583", "69580", 
      "69579", "70292"),
    collapse = "|")
  
  # Less reliable methods
  lower_reliability <- paste0(
    c("GCLAS", "I-3765-85", "Nephelometry"),
    collapse = "|")
  
  # Add tags
  tss_flow_tagged <- tss_flow %>%
    mutate(
      # Taken with a pump/at depth/etc.?
      pump_tag = if_else(
        condition = grepl(x = SampleCollectionEquipmentName,
                          pattern = "pump|peristaltic|niskin|van dorn|Kemmerer",
                          ignore.case = T) |
          grepl(x = ActivityCommentText,
                pattern = deep_text,
                ignore.case = T) |
          # Sampling depth > 1m
          (!is.na(harmonized_top_depth_value) & (abs(harmonized_top_depth_value) > 1)) |
          (!is.na(harmonized_bottom_depth_value) & (abs(harmonized_bottom_depth_value) > 1)) |
          (!is.na(harmonized_discrete_depth_value) & (abs(harmonized_discrete_depth_value) > 1)),
        true = 1, false = 0),
      # Low flow indications?
      low_flow_tag = if_else(
        condition = grepl(x = ActivityCommentText,
                          pattern = low_flow_text,
                          ignore.case = TRUE) & 
          !grepl(x = ActivityCommentText,
                 pattern = "too deep|high flow",
                 ignore.case = TRUE),
        true = 1, false = 0
      ),
      # NA methods or equipment
      na_tag = if_else(
        condition = is.na(ResultAnalyticalMethod.MethodName) |
          is.na(SampleCollectionEquipmentName) |
          grepl(x = ResultAnalyticalMethod.MethodName,
                pattern = na_text,
                ignore.case = TRUE) |
          grepl(x = SampleCollectionEquipmentName,
                pattern = na_text,
                ignore.case = TRUE),
        true = 1, false = 0
      ),
      # Tag expected methods
      common_tag = if_else(
        condition = grepl(x = ResultAnalyticalMethod.MethodName,
                          pattern = correct_text,
                          ignore.case = TRUE) |
          grepl(x = USGSPCode,
                pattern = correct_pcodes),
        true = 1, false = 0
      ),
      # Methods with lower reliability
      lower_reliability_tag = if_else(
        condition = grepl(x = ResultAnalyticalMethod.MethodName,
                          pattern = lower_reliability,
                          ignore.case = TRUE),
        true = 1, false = 0
      )
    )
  
  # Tiering process
  tiered_methods_tss <- tss_flow_tagged %>%
    group_by(ResultAnalyticalMethod.MethodName) %>%
    add_count() %>%
    ungroup() %>%
    mutate(
      tier = if_else(
        # Has NA method/equip
        condition = na_tag == 1 |
          # Has low flow indication
          low_flow_tag == 1 |
          # Less reliable methods
          lower_reliability_tag == 1 |
          # Is non-USGS, labeled as SSC, and has pump/depth tag
          ( 
            (ProviderName == "STORET") &
              (CharacteristicName == "Suspended Sediment Concentration (SSC)") &
              (pump_tag == 0)
          ) |
          # TSS with pump tag
          (
            (CharacteristicName == "Total suspended solids") &
              (pump_tag == 1)
          ) |
          # Not part of a common method group and has few records
          (common_tag == 0) & (n <= 15),
        true = 2,
        false = 0
      ),
      tier = if_else(
        condition = (tier == 0) & !is.na(ResultCommentText),
        true = 1,
        false = tier
      )
    ) %>%
    select(-n)
  
  # Export a record of how methods were tiered and their respective row counts
  tiering_record <- tiered_methods_tss %>%
    count(CharacteristicName, ResultAnalyticalMethod.MethodName, USGSPCode,
          pump_tag, low_flow_tag, na_tag, common_tag, tier) %>%
    arrange(desc(n)) 
  
  tiering_record_out_path <- "3_harmonize/out/tss_tiering_record.csv"
  
  write_csv(x = tiering_record, file = tiering_record_out_path)
  
  # Confirm that no rows were lost during tiering
  if(nrow(tss_flow) != nrow(tiered_methods_tss)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_methods <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(flagged_depth_tss) - nrow(tiered_methods_tss),
    n_rows = nrow(tiered_methods_tss),
    order = 9
  )
  
  
  # Flag field methods ------------------------------------------------------
  
  # Field flag 1 = TSS was taken at depth, van dorn, etc. and SSC was NOT
  # taken at depth
  field_flagged_tss <- tiered_methods_tss %>%
    mutate(
      field_flag = if_else(
        # TSS with pump tag
        condition = (
          (CharacteristicName == "Total suspended solids") &
            (pump_tag == 1)
        ) |
          # Is non-USGS, labeled as SSC, and has pump/depth tag
          ( 
            (ProviderName == "STORET") &
              (CharacteristicName == "Suspended Sediment Concentration (SSC)") &
              (pump_tag == 0)
          ),
        true = 1,
        false = 0
      )
    ) %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed while assigning field flags: ",
      nrow(tiered_methods_tss) - nrow(field_flagged_tss)
    )
  )
  
  dropped_field <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while assigning field flags",
    short_reason = "Field flagging",
    number_dropped = nrow(tiered_methods_tss) - nrow(field_flagged_tss),
    n_rows = nrow(field_flagged_tss),
    order = 10
  )
  
  
  # Miscellaneous flag ------------------------------------------------------
  
  # This is a flag that isn't currently needed for tss, but other parameters
  # will use it.
  
  misc_flagged_tss <- field_flagged_tss %>%
    mutate(misc_flag = NA_character_)
  
  dropped_misc <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while assigning misc flags",
    short_reason = "Misc flagging",
    number_dropped = nrow(field_flagged_tss) - nrow(misc_flagged_tss),
    n_rows = nrow(misc_flagged_tss),
    order = 11
  )
  
  
  # Unrealistic values ------------------------------------------------------
  
  # We remove unrealistically high values prior to the final data export
  
  realistic_tss <- misc_flagged_tss %>%
    filter(
      harmonized_value <= 10000,
      harmonized_top_depth_value <= 592 | is.na(harmonized_top_depth_value),
      harmonized_bottom_depth_value <= 592 | is.na(harmonized_bottom_depth_value),
      harmonized_discrete_depth_value <= 592 | is.na(harmonized_discrete_depth_value)
    )
  
  dropped_unreal <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows with unrealistic values",
    short_reason = "Unrealistic values",
    number_dropped = nrow(misc_flagged_tss) - nrow(realistic_tss),
    n_rows = nrow(realistic_tss),
    order = 12
  )
  
  # Generate plots with harmonized dataset ----------------------------------
  
  # We'll generate plots now before aggregating across simultaneous records
  # because it won't be possible to use CharacteristicName after that point.
  
  # Plot harmonized measurements by CharacteristicName and ProviderName
  
  plotting_subset <- realistic_tss %>%
    select(CharacteristicName, ProviderName, tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001)
  
  char_dists <- plotting_subset %>%
    ggplot() +
    geom_histogram(aes(plot_value), color = "black", fill = "white") +
    facet_wrap(vars(CharacteristicName), scales = "free_y") +
    facet_grid(rows = vars(ProviderName), cols = vars(CharacteristicName)) +
    xlab(expression("Harmonized TSS (mg/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized TSS values by CharacteristicName & ProviderName",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = comma) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/tss_charname_dists.png",
         plot = char_dists,
         width = 8, height = 6, units = "in", device = "png")
  
  
  # Aggregate simultaneous records ------------------------------------------
  
  # There are true duplicate entries in the WQP or records with non-identical
  # values recorded at the same time and place and by the same organization
  # (field and/or lab replicates/duplicates). We take the mean of those values here
  
  # First tag aggregate subgroups with group IDs
  grouped_tss <- realistic_tss %>%
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
  grouped_tss_out_path <- "3_harmonize/out/tss_harmonized_grouped.feather"
  
  grouped_tss %>%
    select(
      all_of(c(raw_names,
               "parameter_code", "group_name", "parameter_name_description",
               "subgroup_id")),
      group_cols(),
      harmonized_value
    ) %>%
    write_feather(path = grouped_tss_out_path)
  
  # Now aggregate at the subgroup level to take care of simultaneous observations
  no_simul_tss <- grouped_tss %>%
    # Make sure we don't drop subgroup ID
    group_by(subgroup_id, .add = TRUE) %>%
    summarize(
      harmonized_row_count = n(),
      harmonized_value_sd = sd(harmonized_value),
      harmonized_value = mean(harmonized_value),
      lon = unique(lon),
      lat = unique(lat),
      datum = unique(datum)
    ) %>%
    # Calculate coefficient of variation as the standard deviation divided by
    # the mean value (harmonized_value in this case)
    mutate(
      harmonized_value_cv = harmonized_value_sd / harmonized_value
    ) %>%
    ungroup() %>%
    select(
      # No longer needed
      -harmonized_value_sd) %>%
    relocate(
      c(subgroup_id, harmonized_row_count, harmonized_units,
        harmonized_value, harmonized_value_cv, lat, lon, datum),
      .after = misc_flag
    )
  
  rm(grouped_tss)
  gc()
  
  # Plot harmonized measurements by Tier:
  
  # 1. Harmonized values
  tier_dists <- no_simul_tss %>%
    select(parameter, tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001,
           tier_label = case_when(
             tier == 0 ~ "Restrictive (Tier 0)",
             tier == 1 ~ "Narrowed (Tier 1)",
             tier == 2 ~ "Inclusive (Tier 2)"
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value), color = "black", fill = "white") +
    # facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
    facet_grid(cols = vars(parameter), rows = vars(tier_label), scales = "free_y") +
    xlab(expression("Harmonized values (mg/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized values by parameter and tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/tss_tier_dists_postagg.png",
         plot = tier_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  # 2: Harmonized CVs
  # Count NA CVs in each tier for plotting and make short count labels:
  na_labels <- no_simul_tss %>%
    filter(is.na(harmonized_value_cv)) %>%
    count(parameter, tier) %>%
    rowwise() %>%
    mutate(
      short = if_else(
        condition = n > 1000000,
        false = number(x = n, scale_cut = cut_short_scale()),
        true = number(x = n, accuracy = .1, scale_cut = cut_short_scale())
      ),
      removed_label = paste0("NAs removed: ", short)
    ) %>%
    ungroup()
  
  # Make the initial plot (will add labels with number of NAs removed below)
  tier_cv_dists_draft <- no_simul_tss %>%
    select(parameter, tier, harmonized_value_cv) %>%
    mutate(plot_value = harmonized_value_cv + 0.001,
           tier_label = case_when(
             tier == 0 ~ "Restrictive (Tier 0)",
             tier == 1 ~ "Narrowed (Tier 1)",
             tier == 2 ~ "Inclusive (Tier 2)"
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value), color = "black", fill = "white") +
    facet_grid(rows = vars(tier_label), cols = vars(parameter), scales = "free_y") +
    xlab(expression("Harmonized coefficient of variation, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized CVs by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  # Extract build info from initial plot
  gg_build <- ggplot_build(tier_cv_dists_draft)
  
  # Find out what the max y-axis break values are for each row's panels
  panel_break_y_values <- map(
    .x = 1:3,
    .f = ~{
      data.frame(
        row = .x,
        # Retrieve the max break value used on the y-axis
        max_y = max(gg_build$layout$panel_scales_y[[.x]]$get_breaks())
      )
    }
  ) %>%
    bind_rows()
  
  # Find out what the median x-axis break values are for the panels so we can
  # center the labels
  panel_break_x_values <- gg_build$layout$panel_scales_x[[1]]$get_labels() %>%
    as.numeric() %>%
    median()
  
  # Extract the param * tier combinations that correspond to panel numbers under the hood
  grid_info <- gg_build$layout$layout %>%
    # Extract the raw tier numbers for back joining
    mutate(tier = as.numeric(str_extract(string = tier_label, pattern = "[0-9]"))) %>%
    # Join corresponding NA removal labels by panel
    left_join(x = ., y = na_labels, by = c("parameter", "tier")) %>%
    # Join info on max breaks used in each panel
    left_join(x = ., y = panel_break_y_values, by = c("ROW" = "row")) %>%
    # Slap on the median x val. Only 1 b/c we use "free_y" in facet_grid
    mutate(median_x = panel_break_x_values)
  
  # Now update the plot using this info for geom_text placement
  tier_cv_dists <- tier_cv_dists_draft +
    geom_text(data = grid_info,
              aes(x = median_x, y = max_y, label = removed_label))
  
  ggsave(filename = "3_harmonize/out/tss_tier_cv_dists_postagg.png",
         plot = tier_cv_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  # Similarly, create maps of records counts by tier
  plot_tier_maps(dataset = no_simul_tss, parameter = "tss")
  
  # And year, month, day of week
  plot_time_charts(dataset = no_simul_tss)
  
  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Rows removed while aggregating simultaneous records: ",
      nrow(realistic_tss) - nrow(no_simul_tss)
    )
  )
  
  dropped_simul <- tibble(
    step = "tss harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(realistic_tss) - nrow(no_simul_tss),
    n_rows = nrow(no_simul_tss),
    order = 13
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_media,
                                dropped_fails, dropped_mdls,
                                dropped_approximates, dropped_greater_than,
                                dropped_na, dropped_harmonization,
                                dropped_depths, dropped_methods,
                                dropped_field, dropped_misc, dropped_unreal,
                                dropped_simul)
  
  documented_drops_out_path <- "3_harmonize/out/tss_harmonize_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/tss_harmonized_final.csv"
  
  write_csv(no_simul_tss,
            data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(no_simul_tss)
    )
  )
  
  return(list(
    tss_tiering_record_path = tiering_record_out_path,
    tss_grouped_preagg_path = grouped_tss_out_path,
    tss_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))  
}
