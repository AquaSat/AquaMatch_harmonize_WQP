
harmonize_doc <- function(raw_doc, p_codes){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "doc harmonization",
    reason = "Starting dataset",
    short_reason = "Start",
    number_dropped = 0,
    n_rows = nrow(raw_doc),
    order = 0
  )
  
  # Minor data prep ---------------------------------------------------------
  
  # Grab the column names of the dataset coming in
  raw_names <- names(raw_doc)
  
  # Count the number of rows in each fraction of the carbon pool:
  full_fraction_count <- raw_doc %>%
    count(ResultSampleFractionText, name = "record_count")
  
  doc_media <- raw_doc %>% 
    # Link up USGS p-codes. Their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    filter(
      # Filter out non-target media types
      ActivityMediaSubdivisionName %in% c("Surface Water", "Water", "Estuary") |
        is.na(ActivityMediaSubdivisionName)
    )
  
  doc_fraction <- doc_media %>%
    # Dissolved organic carbon is not a CharacteristicName, so we must identify
    # the fractions of the carbon pool reported that match our needs:
    semi_join(x = .,
              y = tribble(
                ~CharacteristicName, ~ResultSampleFractionText,
                "Organic carbon",                      "Dissolved",             
                "Organic carbon",                      "Filtered, lab",             
                "Organic carbon",                      "Filtered, field",           
                "Organic carbon",                      "Filterable",                 
                "Total carbon",                        "Filterable",
                "Organic carbon",                      "Total", 
                "Total carbon",                        NA,    
                "Total carbon",                        "Total",     
                "Total carbon",                        "Organic",      
                "Organic carbon",                      "Organic",
                "Non-purgeable Organic Carbon (NPOC)", "Dissolved",
                "Non-purgeable Organic Carbon (NPOC)", "Filtered, lab",
                "Non-purgeable Organic Carbon (NPOC)", "None",
                "Non-purgeable Organic Carbon (NPOC)", "Total",
                "Non-purgeable Organic Carbon (NPOC)", NA
              )
    ) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Count the number of rows in each fraction of the carbon pool now that we've
  # filtered some out:
  reduced_fraction_count <- doc_fraction %>%
    count(ResultSampleFractionText, name = "record_count")
  
  # Plot which fraction types got dropped, and how many records they had
  anti_join(x = doc_media,
            y = doc_fraction) %>%
    # Separate plot for each of the CharacteristicNames
    split(f = .$CharacteristicName) %>%
    walk(.f = ~ {
      # Grab CharacteristicName for plotting
      char_name <- unique(.x$CharacteristicName)
      
      drop_plot <- .x %>%
        count(ResultSampleFractionText, name = "record_count") %>%
        plot_fraction_pie() +
        ggtitle(paste0(char_name, ":\nFraction types dropped from dataset"))
      
      ggsave(filename = paste0("3_harmonize/out/doc_",
                               to_snake_case(char_name),
                               "_fraction_drop_pie.png"),
             plot = drop_plot, units = "in", device = "png",
             width = 6, height = 6)
    })
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "doc harmonization",
    reason = "Filtered for only water media & doc fraction",
    short_reason = "Target water media & doc",
    number_dropped = nrow(raw_doc) - nrow(doc_fraction),
    n_rows = nrow(doc_fraction),
    order = 1
  )
  
  rm(raw_doc)
  gc()
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", 
    "improper", "interference", "invalid", "no result", "no test",
    "not accept", "outside of accept", "problem", "questionable",
    "suspect", "unable", "violation", "reject", "no data", "time exceed",
    "value extrapolated", "exceeds", "biased", "parameter not required",
    "not visited"
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
                 hit_count <- doc_fraction %>%
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
    # drop them to avoid errors in the next step.
    keep(~nrow(.) > 0)
  
  
  # Plot and export the plots as png files
  walk2(.x = fail_counts,
        .y = names(fail_counts),
        .f = ~ ggsave(filename = paste0("3_harmonize/out/doc_",
                                        .y,
                                        "_fail_pie.png"),
                      plot = plot_fail_pie(dataset = .x, col_name = .y),
                      width = 6, height = 6, units = "in", device = "png"))
  
  
  # Now that the fails have been documented, remove them:
  doc_fails_removed <- doc_fraction %>%
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
      nrow(doc_fraction) - nrow(doc_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(doc_fraction) - nrow(doc_fails_removed),
    n_rows = nrow(doc_fails_removed),
    order = 2)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  non_detect_text <- "non-detect|not detect|non detect|undetect|below"
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- doc_fails_removed %>%
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
      half = as.numeric(mdl_vals) / 2)
  
  # Using the EPA standard for non-detects, select a random number between zero
  # and HALF the MDL:
  mdl_updates$std_value <- with(mdl_updates, runif(nrow(mdl_updates), 0, half))
  mdl_updates$std_value[is.nan(mdl_updates$std_value)] <- NA
  
  # Keep important data
  mdl_updates <- mdl_updates %>%
    select(index, std_value, mdl_vals, mdl_units) %>%
    filter(!is.na(std_value))
  
  
  print(
    paste(
      round((nrow(mdl_updates)) / nrow(doc_fails_removed) * 100, 1),
      "% of samples had values listed as being below a detection limit"
    )
  )
  
  # Replace "harmonized_value" field with these new values
  doc_mdls_added <- doc_fails_removed %>%
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
           ))
  
  dropped_mdls <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(doc_fails_removed) - nrow(doc_mdls_added),
    n_rows = nrow(doc_mdls_added),
    order = 3
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  approx_text <- "result approx|RESULT IS APPROX|value approx"
  
  doc_approx <- doc_mdls_added %>%
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
  
  doc_approx$approx_value <- as.numeric(str_replace_all(doc_approx$ResultMeasureValue_original, c("\\*" = "")))
  doc_approx$approx_value[is.nan(doc_approx$approx_value)] <- NA
  
  # Keep important data
  doc_approx <- doc_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(doc_approx)) / nrow(doc_mdls_added) * 100, 3),
      "% of samples had values listed as approximated"
    )
  )
  
  # Replace harmonized_value field with these new values
  doc_approx_added <- doc_mdls_added %>%
    left_join(x = ., y = doc_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% doc_approx$index,
                                     approx_value,
                                     harmonized_value),
           # Flag: 1 = used approximate adjustment, 0 = value not adjusted
           approx_flag = ifelse(index %in% doc_approx$index, 1, 0))
  
  dropped_approximates <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(doc_mdls_added) - nrow(doc_approx_added),
    n_rows = nrow(doc_approx_added),
    order = 4
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  # Next step, incorporating and flagging "greater than" values. Using a similar
  # approach to the previous two flags, we can identify results that 
  # contain values greater than some amount
  
  greater_vals <- doc_approx_added %>%
    # First, remove the samples that we've already approximated:
    filter((!index %in% mdl_updates$index) & (!index %in% doc_approx$index)) %>%
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
      round((nrow(greater_vals)) / nrow(doc_approx_added) * 100, 9),
      "% of samples had values listed as being above a detection limit//greater than"
    )
  )
  
  # Replace harmonized_value field with these new values
  doc_harmonized_values <- doc_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           # Flag: 1 = used greater than adjustment, 0 = value not adjusted
           greater_flag = ifelse(index %in% greater_vals$index, 1, 0))
  
  dropped_greater_than <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(doc_approx_added) - nrow(doc_harmonized_values),
    n_rows = nrow(doc_harmonized_values),
    order = 5
  )
  
  
  # Free up memory
  rm(doc_media, doc_fraction)
  gc()
  
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, and values containing
  # symbols like ">". If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.
  
  doc_no_na <- doc_harmonized_values %>%
    filter(
      !is.na(harmonized_value),
      # Some negative values can be introduced by the previous NA parsing steps:
      harmonized_value >= 0
    )
  
  dropped_na <- tibble(
    step = "doc harmonization",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(doc_harmonized_values) - nrow(doc_no_na),
    n_rows = nrow(doc_no_na),
    order = 6
  )
  
  # Free up memory
  rm(doc_harmonized_values, doc_approx_added, doc_mdls_added,
     doc_fails_removed)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Set up a lookup table so that final units are all in mg/L. 
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c("mg/L", "mg/l", "ppm", "ug/l", "ug/L",
                                      "mg/m3", "ppb", "mg/cm3", "ug/ml", "ug/mL",
                                      "mg/ml", "ppt", "umol/L"),
    conversion = c(1, 1, 1, 0.001, 0.001, 0.001, 0.001, 1000, 1, 1, 1000,
                   1000, 0.0120107))
  
  unit_table_out_path <- "3_harmonize/out/doc_unit_table.csv"
  
  write_csv(x = unit_conversion_table,
            file = unit_table_out_path)
  
  converted_units_doc <- doc_no_na %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = ResultMeasureValue * conversion,
           harmonized_units = "mg/L")
  
  # Plot and export unit codes that didn't make through joining
  doc_no_na %>%
    anti_join(x = .,
              y = unit_conversion_table,
              by = "ResultMeasure.MeasureUnitCode")  %>%
    count(ResultMeasure.MeasureUnitCode, name = "record_count") %>%
    plot_unit_pie() %>%
    ggsave(filename = "3_harmonize/out/doc_unit_drop_pie.png",
           plot = .,
           width = 6, height = 6, units = "in", device = "png")
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(doc_no_na) - nrow(converted_units_doc)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(doc_no_na) - nrow(converted_units_doc),
    n_rows = nrow(converted_units_doc),
    order = 7
  )
  
  
  # Clean and flag depth data -----------------------------------------------
  
  # Recode any error-related character values to NAs
  recode_depth_na_doc <- converted_units_doc %>%
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
  converted_depth_units_doc <- recode_depth_na_doc %>%
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
  harmonized_depth_doc <- converted_depth_units_doc %>%
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
  flagged_depth_doc <- harmonized_depth_doc %>%
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
  depth_check_table <- flagged_depth_doc %>%
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
  
  depth_check_out_path <- "3_harmonize/out/doc_depth_check_table.csv"
  
  write_csv(x = depth_check_table,
            file = depth_check_out_path)
  
  # Depth category counts:
  depth_counts <- flagged_depth_doc %>%
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
  
  depth_counts_out_path <- "3_harmonize/out/doc_depth_counts.csv"
  
  write_csv(x = depth_counts, file = depth_counts_out_path)
  
  # Have any records been removed while processing depths?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_doc) - nrow(flagged_depth_doc)
    )
  )
  
  dropped_depths <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while cleaning depths",
    short_reason = "Clean depths",
    number_dropped = nrow(converted_units_doc) - nrow(flagged_depth_doc),
    n_rows = nrow(flagged_depth_doc),
    order = 8
  )
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of doc analytical methods present: ",
      length(unique(flagged_depth_doc$ResultAnalyticalMethod.MethodName))
    )
  )
  
  # Before creating tiers remove any records that have unrelated
  # data based on their method:
  unrelated_text <- paste0(c("Oxygen", "Nitrogen", "Ammonia", "Metals",
                             "E. coli", "Anion", "Cation", "Phosphorus",
                             "Silica", "PH", "HARDNESS", "Nutrient", "Turbidity",
                             "Nitrate", "Conductance", "Alkalinity", "Chlorophyll",
                             "Solids", "Temperature", "Color in Water",
                             "Coliform", "PARTICULATE CARBON (inorg+org)",
                             "5210", "bed sed", "bs, calc", "5220",
                             "Suspended-Sediment in Water"),
                           collapse = "|")
  
  doc_relevant <- flagged_depth_doc %>%
    filter(!grepl(pattern = unrelated_text,
                  x = ResultAnalyticalMethod.MethodName,
                  ignore.case = TRUE))
  
  # 1.1 Combustion+IR detection
  
  # Create a new column indicating detection of Combustion+IR methods text: T or F
  combustion_ir_text <- paste0(c("5310 ?B", "415.1",
                                 # "combustion" but only when not in the context of
                                 # 5310 C / 5310C
                                 "^(?!.*5310 ?C).*combustion(?!.*5310 ?C)",
                                 "0.45u silver, persulfate IR", "0.7 um GFF, combust IR",
                                 "Laboratory Procedures for Water Quality Chemical Analysis",
                                 "CO2 formation", "Qian & Mopper 1996", "Shimadzu TOC Analyzer"),
                               collapse = "|")
  
  doc_tag_combustion_ir <- doc_relevant %>%
    # TRUE if methods contain Combustion+IR related text or if the correct USGS p code
    mutate(combustion_ir_tag = if_else(condition = grepl(pattern = combustion_ir_text,
                                                         x = ResultAnalyticalMethod.MethodName,
                                                         ignore.case = TRUE,
                                                         perl = TRUE),
                                       true = TRUE,
                                       false = FALSE,
                                       missing = FALSE))
  
  
  # 1.2 Persulfate-UV/Heated Persulfate Oxidation+IR detection
  
  # Create a new column indicating detection of Persulfate-UV methods text: T or F
  pers_uv_heated_text <- paste0(c("5310 ?C", "persulfate oxid", "415.2",
                                  "Ultraviolet", "Heated-Persulfate", "UV"),
                                collapse = "|")
  
  doc_tag_pers_uv_heated <- doc_tag_combustion_ir %>%
    # TRUE if methods contain Persulfate-UV/Heated Persulfate Oxidation+IR
    # related text or if the correct USGS p code
    mutate(pers_uv_heated_tag = if_else(condition = grepl(pattern = pers_uv_heated_text,
                                                          x = ResultAnalyticalMethod.MethodName,
                                                          ignore.case = TRUE,
                                                          perl = TRUE),
                                        true = TRUE,
                                        false = FALSE,
                                        missing = FALSE))
  
  # 1.3 Wet Oxidation+Persulfate+IR detection
  
  # Create a new column indicating detection of Wet Oxidation+Persulfate+IR
  # methods text: T or F
  wet_ox_pers_text <- paste0(c(
    "0.45 ?um cap", "wet oxidation", "5310 ?D", "415.3",
    "O-3100 ~ Total Organic Carbon in Water"),
    collapse = "|")
  
  doc_tag_wet_ox_pers <- doc_tag_pers_uv_heated %>%
    # TRUE if methods contain Wet Oxidation+Persulfate+IR related text or if
    # the correct USGS p code
    mutate(wet_ox_pers_tag = if_else(condition = grepl(pattern = wet_ox_pers_text,
                                                       x = ResultAnalyticalMethod.MethodName,
                                                       ignore.case = TRUE,
                                                       perl = TRUE),
                                     true = TRUE,
                                     false = FALSE,
                                     missing = FALSE))
  
  
  # 1.4 Elemental Analyzer detection
  
  # Create a new column indicating detection of elemental analyzer methods
  # text: T or F
  elemental_text <- 440
  
  doc_tag_elemental <- doc_tag_wet_ox_pers %>%
    # TRUE if methods contain elemental analyzer related text or if
    # the correct USGS p code
    mutate(elemental_tag = if_else(condition = grepl(pattern = elemental_text,
                                                     x = ResultAnalyticalMethod.MethodName,
                                                     ignore.case = TRUE,
                                                     perl = TRUE),
                                   true = TRUE,
                                   false = FALSE,
                                   missing = FALSE))
  
  # 1.5 Carbonaceous Analyzer detection
  
  # Create a new column indicating detection of carbonaceous analyzer methods
  # text: T or F
  carbonaceous_text <- "9060 A"
  
  doc_tag_carbonaceous <- doc_tag_elemental %>%
    # TRUE if methods contain carbonaceous analyzer related text or if
    # the correct USGS p code
    mutate(carbonaceous_tag = if_else(condition = grepl(pattern = carbonaceous_text,
                                                        x = ResultAnalyticalMethod.MethodName,
                                                        ignore.case = TRUE,
                                                        perl = TRUE),
                                      true = TRUE,
                                      false = FALSE,
                                      missing = FALSE))
  
  
  # 1.6 Create tiers
  tiered_methods_doc <- doc_tag_carbonaceous %>%
    mutate(tier = case_when(
      # Top tier is "Wet Oxidation+Persulfate+IR" and 
      # "Persulfate-UV/Heated Persulfate Oxidation+IR" - checking for TRUEs
      pers_uv_heated_tag | wet_ox_pers_tag ~ 0,
      # Narrowed tier is Combustion+IR or elemental analyzer
      combustion_ir_tag | elemental_tag ~ 1,
      # Carbonaceous is sorted into inclusive, along with the remaining methods
      carbonaceous_tag ~ 2,
      # All else inclusive
      .default = 2
    ))
  
  # Export a record of how methods were tiered and their respective row counts
  tiering_record <- tiered_methods_doc %>%
    count(CharacteristicName, ResultAnalyticalMethod.MethodName, USGSPCode,
          pers_uv_heated_tag, wet_ox_pers_tag, combustion_ir_tag, elemental_tag,
          carbonaceous_tag, tier) %>%
    arrange(desc(n)) 
  
  tiering_record_out_path <- "3_harmonize/out/doc_tiering_record.csv"
  
  write_csv(x = tiering_record, file = tiering_record_out_path)
  
  
  # Slim the tiered product
  cleaned_tiered_methods_doc <- tiered_methods_doc %>%
    # Drop tag columns - these are recorded and exported in tiering_record. We
    # keep only the final tier
    select(-contains("_tag"))
  
  # Confirm that no rows were lost during tiering
  if(nrow(doc_relevant) != nrow(cleaned_tiered_methods_doc)){
    stop("Rows were lost during analytical method tiering. This is not expected.")
  }  
  
  dropped_methods <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(flagged_depth_doc) - nrow(cleaned_tiered_methods_doc),
    n_rows = nrow(doc_relevant),
    order = 9
  )
  
  
  # Flag field methods ------------------------------------------------------
  
  # DOC doesn't have field sampling methods that lend themselves well to
  # comparing with analytical methods and assigning flags, unlike variables
  # like chlorophyll a. We fill this field_flag column with NA for "not applicable"
  # for DOC
  
  field_flagged_doc <- cleaned_tiered_methods_doc %>%
    mutate(field_flag = NA_character_)
  
  # How many records removed while assigning field flags?
  print(
    paste0(
      "Rows removed while assigning field flags: ",
      nrow(cleaned_tiered_methods_doc) - nrow(field_flagged_doc)
    )
  )
  
  dropped_field <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while assigning field flags",
    short_reason = "Field flagging",
    number_dropped = nrow(cleaned_tiered_methods_doc) - nrow(field_flagged_doc),
    n_rows = nrow(field_flagged_doc),
    order = 10
  )
  
  
  # Miscellaneous flag ------------------------------------------------------
  
  # This is a flag that isn't currently needed for DOC, but other parameters
  # will use it.
  
  misc_flagged_doc <- field_flagged_doc %>%
    mutate(misc_flag = NA_character_)
  
  dropped_misc <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while assigning misc flags",
    short_reason = "Misc flagging",
    number_dropped = nrow(field_flagged_doc) - nrow(misc_flagged_doc),
    n_rows = nrow(misc_flagged_doc),
    order = 11
  )
  
  
  # Unrealistic values ------------------------------------------------------
  
  # We remove unrealistically high values prior to the final data export
  # This value is based on Hotchkiss and DelSontro (2024) and Mulholland (2003).

  realistic_doc <- misc_flagged_doc %>%
    filter(harmonized_value <= 300)
  
  dropped_unreal <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows with unrealistic values",
    short_reason = "Unrealistic values",
    number_dropped = nrow(misc_flagged_doc) - nrow(realistic_doc),
    n_rows = nrow(realistic_doc),
    order = 12
  )
  
  # Generate plots with harmonized dataset ----------------------------------
  
  # We'll generate plots now before aggregating across simultaneous records
  # because it won't be possible to use CharacteristicName after that point.
  
  # Plot harmonized measurements by CharacteristicName
  
  plotting_subset <- realistic_doc %>%
    select(CharacteristicName, USGSPCode, tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001)
  
  char_dists <- plotting_subset %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(CharacteristicName), scales = "free_y") +
    xlab(expression("Harmonized DOC (mg/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized DOC values by CharacteristicName",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/doc_charname_dists.png",
         plot = char_dists,
         width = 8, height = 6, units = "in", device = "png")
  
  
  # Aggregate simultaneous records ------------------------------------------
  
  # There are true duplicate entries in the WQP or records with non-identical
  # values recorded at the same time and place and by the same organization
  # (field and/or lab replicates/duplicates)
  
  # We take the mean of those values here
  
  # First tag aggregate subgroups with group IDs
  grouped_doc <- realistic_doc %>%
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
  grouped_doc_out_path <- "3_harmonize/out/doc_harmonized_grouped.feather"
  
  grouped_doc %>%
    select(
      all_of(c(raw_names,
               "parameter_code", "group_name", "parameter_name_description",
               "subgroup_id")),
      group_cols(),
      harmonized_value
    ) %>%
    write_feather(path = grouped_doc_out_path)
  
  # Now aggregate at the subgroup level to take care of simultaneous observations
  no_simul_doc <- grouped_doc %>%
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
  
  
  rm(grouped_doc)
  gc()
  
  # Plot harmonized measurements by Tier
  
  # 1. Harmonized values
  tier_dists <- no_simul_doc %>%
    select(tier, harmonized_value) %>%
    mutate(plot_value = harmonized_value + 0.001,
           tier_label = case_when(
             tier == 0 ~ "Restrictive (Tier 0)",
             tier == 1 ~ "Narrowed (Tier 1)",
             tier == 2 ~ "Inclusive (Tier 2)"
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized DOC (mg/L, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized DOC values by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/doc_tier_dists_postagg.png",
         plot = tier_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  # 2: Harmonized CVs
  # Count NA CVs in each tier for plotting:
  na_labels <- no_simul_doc %>%
    filter(is.na(harmonized_value_cv)) %>%
    count(tier)
  
  tier_0 <- filter(na_labels, tier == 0)$n %>%
    number(scale_cut = cut_short_scale())
  
  tier_1 <- filter(na_labels, tier == 1)$n %>%
    number(scale_cut = cut_short_scale())
  
  tier_2 <- filter(na_labels, tier == 2)$n %>%
    number(scale_cut = cut_short_scale())
  
  tier_cv_dists <- no_simul_doc %>%
    select(tier, harmonized_value_cv) %>%
    mutate(plot_value = harmonized_value_cv + 0.001,
           tier_label = case_when(
             tier == 0 ~ paste0("Restrictive (Tier 0) NAs removed: ", tier_0),
             tier == 1 ~ paste0("Narrowed (Tier 1) NAs removed: ", tier_1),
             tier == 2 ~ paste0("Inclusive (Tier 2) NAs removed: ", tier_2)
           )) %>%
    ggplot() +
    geom_histogram(aes(plot_value)) +
    facet_wrap(vars(tier_label), scales = "free_y", ncol = 1) +
    xlab(expression("Harmonized coefficient of variation, " ~ log[10] ~ " transformed)")) +
    ylab("Count") +
    ggtitle(label = "Distribution of harmonized DOC CVs by tier",
            subtitle = "0.001 added to each value for the purposes of visualization only") +
    scale_x_log10(label = label_scientific()) +
    scale_y_continuous(label = label_scientific()) +
    theme_bw() +
    theme(strip.text = element_text(size = 7))
  
  ggsave(filename = "3_harmonize/out/doc_tier_cv_dists_postagg.png",
         plot = tier_cv_dists,
         width = 6, height = 4, units = "in", device = "png")
  
  # Similarly, create maps of records counts by tier
  plot_tier_maps(dataset = no_simul_doc, parameter = "doc")
  
  # And year, month, day of week
  plot_time_charts(dataset = no_simul_doc, parameter = "doc")
  
  # How many records removed in aggregating simultaneous records?
  print(
    paste0(
      "Rows removed while aggregating simultaneous records: ",
      nrow(realistic_doc) - nrow(no_simul_doc)
    )
  )
  
  dropped_simul <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while aggregating simultaneous records",
    short_reason = "Simultaneous records",
    number_dropped = nrow(realistic_doc) - nrow(no_simul_doc),
    n_rows = nrow(no_simul_doc),
    order = 12
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
  
  documented_drops_out_path <- "3_harmonize/out/doc_harmonize_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/doc_harmonized_final.csv"
  
  write_csv(no_simul_doc,
            data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(no_simul_doc)
    )
  )
  
  return(list(
    doc_tiering_record_path = tiering_record_out_path,
    doc_grouped_preagg_path = grouped_doc_out_path,
    doc_harmonized_path = data_out_path,
    compiled_drops_path = documented_drops_out_path
  ))   
  
}