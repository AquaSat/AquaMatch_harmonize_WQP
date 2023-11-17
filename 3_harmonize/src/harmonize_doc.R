
harmonize_doc <- function(raw_doc, p_codes){
  
  # Starting values for dataset
  starting_data <- tibble(
    step = "doc harmonization",
    reason = "Starting dataset",
    short_reason = "Starting",
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
  
  doc <- raw_doc %>% 
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    filter(
      ActivityMediaName %in% c("Water", "water"),
      # Dissolved organic carbon is not a CharacteristicName, so we must identify
      # the fractions of the carbon pool reported that match our needs:
      ResultSampleFractionText %in% c("Dissolved", "Filterable", "Filtered, lab",
                                      "Filtered, field")
    ) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Count the number of rows in each fraction of the carbon pool now that we've
  # filtered some out:
  reduced_fraction_count <- doc %>%
    count(ResultSampleFractionText, name = "record_count")
  
  # Which fraction types got dropped, and how many counts did they have? Plot it.
  anti_join(x = full_fraction_count,
            y = reduced_fraction_count) %>%
    plot_fraction_pie() %>%
    ggsave(filename = "3_harmonize/out/doc_fraction_drop_pie.png",
           plot = .,
           width = 6, height = 6, units = "in", device = "png")
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "doc harmonization",
    reason = "Filtered for only water media & doc fraction",
    short_reason = "Water media & doc",
    number_dropped = nrow(raw_doc) - nrow(doc),
    n_rows = nrow(doc),
    order = 1
  )
  
  rm(raw_doc)
  gc()
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", 
    "improper", "instrument down", "interference", "invalid", "no result", 
    "no test", "not accept", "outside of accept", "problem", "QC EXCEEDED", 
    "questionable", "suspect", "unable", "violation", "reject", "no data",
    "time exceed"
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
                 hit_count <- doc %>%
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
  doc_fails_removed <- doc %>%
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
      nrow(doc) - nrow(doc_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(doc) - nrow(doc_fails_removed),
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
      round((nrow(mdl_updates)) / nrow(doc_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
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
           )) %>%
    # Remove negative measurement values
    filter(harmonized_value >= 0)
  
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
      '% of samples had values listed as approximated'
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
  
  greater_vals <- doc_approx_added %>%
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
      '% of samples had values listed as being above a detection limit//greater than'
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
  rm(doc)
  gc()
  
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, and values containing
  # symbols like ">". If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.
  
  doc_no_na <- doc_harmonized_values %>%
    filter(!is.na(harmonized_value))
  
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
  
  # Set up a lookup table so that final units are all in ug/L. 
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c('mg/L', 'mg/l', 'ppm', 'ug/l', 'ug/L', 'mg/m3',
                                      'ppb', 'mg/cm3', 'ug/ml', 'mg/ml', 'ppt', 'umol/L'),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000,
                   1000000, 0.000001, 60.080000))
  
  unit_table_out_path <- "3_harmonize/out/doc_unit_table.csv"
  
  write_csv(x = unit_conversion_table,
            file = unit_table_out_path)
  
  
  converted_units_doc <- doc_no_na %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = (ResultMeasureValue * conversion) / 1000,
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
  
  doc_aggregated_methods <- doc_harmonized_units %>%
    # Includes more phrases that are unrelated
    filter(!grepl(
      paste0(
        c("Oxygen", "Nitrogen", "Ammonia", "Metals", "E. coli", "Anion", "Cation",
          "Phosphorus", "Silica", "PH", "HARDNESS", "Nutrient", "Turbidity",
          "Nitrate", "Conductance", "Alkalinity", "Chlorophyll", "Solids",
          "Temperature", "Color in Water", "Coliform", "PARTICULATE CARBON (inorg+org)",
          "5210", "bed sed", "bs, calc", "5220", "Suspended-Sediment in Water"),
        collapse = "|"),
      x = ResultAnalyticalMethod.MethodName,
      ignore.case = TRUE)) %>%
    mutate(method_status = case_when(
      
      # DOC-specific:
      grepl("5310 B ~ Total Organic Carbon by Combustion-Infrared Method|Total Organic Carbon by Combustion|
          5310 B ~ Total Organic Carbon by High-Temperature Combustion Method|SM5310B|
          Organic-C, combustion-IR method|EPA 415.1|SM 5310 B|EPA 415.1M|TOC, combustion (SM5310B,COWSC)|
          DOC, combustion, NDIR (SM5310B)|TOC, combustion & CO2 detection|415.1|TOC, wu, combustion (5310B;DDEC)|
          SM185310B|DOC, wf, combustion (5310B;DDEC)|EPA Method 415.1 for Total Organic Carbon in aqueous matrices|
          SM 5310 B v20|DOC, 0.45u silver, persulfate IR",
            ResultAnalyticalMethod.MethodName,ignore.case = T) ~ "5310B + EPA 415.1 - Combustion + IR",
      
      grepl("5310 C ~ Total Organic Carbon in Water- Ultraviolet Oxidation Method|UV OR HEATED PERSULFATE OXIDATION|
          DOC, UV/persulfate (NYWSC; ALSC)|415.2|DOC, persulfate oxidation & IR|
          SM5310C|SM 5310 C|TOC, persulfate oxid (5310C; PA)|TOC, wu, persulfate (SM5310C;CO)|DOC, persulfate oxid, IR (COWSC)|
          Dissolved Organic Carbon in Water by Persulfate Oxidation and Infrared Spectrometry|TOC, persulfate-UV oxid (NYSDEC)|
          TOTAL ORGANIC CARBON (TOC) PERSULFATE-ULTRAVIOLET|TOC - Persulfate-Ultraviolet or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;ND)|TOC, wu, persulfate (SM5310C;ND)|TOC, UV/persulfate/IR (USGS-NYL)|
          DOC, persulfate oxid (5310C; PA)|EPA 415.2|DOC, UV/persulfate (NYWSC; KECK)|
          5310 C ~ Total Organic Carbon by High-Temperature Combustion Method|415.2 M ~ Total Organic Carbon in Water|
          SM 5310 C, EPA 415.3|5310 C ~ Total organic carbon by Persulfate-UV or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;CO)", 
            ResultAnalyticalMethod.MethodName,ignore.case = T) ~ "5310C + USGS O-1122-92 + EPA 415.2 - Persulfate-UV/Heated Persulfate Oxidation + IR",
      
      grepl("TOC, wet oxidation|WET OXIDATION METHOD|DOC,0.45um cap,acid,persulfateIR|
    5310 D ~ Total Organic Carbon in Water- Wet-Oxidation Method|DOC, wf, 0.45 um cap, combust IR|415.3|
    Determination of Total Organic Carbon and Specific UV Absorbance at 254 nm in Source Water and Drinking Water|
    EPA 415.3|SM 5310 D|O-3100 ~ Total Organic Carbon in Water", 
            ResultAnalyticalMethod.MethodName,ignore.case = T) ~ "5310D + EPA 415.3 + USGS O-3100 - Wet Oxidation + Persulfate + IR",
      
      grepl("440 W  ~ Determination of Carbon and Nitrogen", 
            ResultAnalyticalMethod.MethodName,ignore.case = T) ~ "EPA 440.0",
      
      grepl("9060 A ~ Total Organic Carbon in water and wastes by Carbonaceous Analyzer|9060 AM ~ Total Volatile Organic Carbon|
          EPA 9060|EPA 9060A",
            ResultAnalyticalMethod.MethodName,ignore.case = T) ~ "EPA 9060A - Carbonaceous Analyzer",
      (!grepl("5310 B ~ Total Organic Carbon by Combustion-Infrared Method|Total Organic Carbon by Combustion|
          5310 B ~ Total Organic Carbon by High-Temperature Combustion Method|SM5310B|
          Organic-C, combustion-IR method|EPA 415.1|SM 5310 B|EPA 415.1M|TOC, combustion (SM5310B,COWSC)|
          DOC, combustion, NDIR (SM5310B)|TOC, combustion & CO2 detection|415.1|TOC, wu, combustion (5310B;DDEC)|
          SM185310B|DOC, wf, combustion (5310B;DDEC)|EPA Method 415.1 for Total Organic Carbon in aqueous matrices|
          SM 5310 B v20|DOC, 0.45u silver, persulfate IR|5310 C ~ Total Organic Carbon in Water- Ultraviolet Oxidation Method|
          UV OR HEATED PERSULFATE OXIDATION|
          DOC, UV/persulfate (NYWSC; ALSC)|415.2|DOC, persulfate oxidation & IR|
          SM5310C|SM 5310 C|TOC, persulfate oxid (5310C; PA)|TOC, wu, persulfate (SM5310C;CO)|DOC, persulfate oxid, IR (COWSC)|
          Dissolved Organic Carbon in Water by Persulfate Oxidation and Infrared Spectrometry|TOC, persulfate-UV oxid (NYSDEC)|
          TOTAL ORGANIC CARBON (TOC) PERSULFATE-ULTRAVIOLET|TOC - Persulfate-Ultraviolet or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;ND)|TOC, wu, persulfate (SM5310C;ND)|TOC, UV/persulfate/IR (USGS-NYL)|
          DOC, persulfate oxid (5310C; PA)|EPA 415.2|DOC, UV/persulfate (NYWSC; KECK)|
          5310 C ~ Total Organic Carbon by High-Temperature Combustion Method|415.2 M ~ Total Organic Carbon in Water|
          SM 5310 C, EPA 415.3|5310 C ~ Total organic carbon by Persulfate-UV or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;CO)|TOC, wet oxidation|WET OXIDATION METHOD|DOC,0.45um cap,acid,persulfateIR|
    5310 D ~ Total Organic Carbon in Water- Wet-Oxidation Method|DOC, wf, 0.45 um cap, combust IR|415.3|
    Determination of Total Organic Carbon and Specific UV Absorbance at 254 nm in Source Water and Drinking Water|
    EPA 415.3|SM 5310 D|O-3100 ~ Total Organic Carbon in Water|9060 A ~ Total Organic Carbon in water and wastes by Carbonaceous Analyzer|9060 AM ~ Total Volatile Organic Carbon|
          EPA 9060|EPA 9060A",
              # KW: include NA values in the "Ambiguous" method lump
              ResultAnalyticalMethod.MethodName,ignore.case = T) & !is.na(ResultAnalyticalMethod.MethodName)) | is.na(ResultAnalyticalMethod.MethodName) ~ "Ambiguous"))
  
  doc_grouped_more <- doc_aggregated_methods %>% 
    mutate(grouped = case_when(grepl(pattern = "5310B", 
                                     x = method_status) ~ "Combustion+IR",
                               grepl(pattern = "5310C", 
                                     x = method_status) ~ "Persulfate-UV/Heated Persulfate Oxidation+IR",
                               grepl(pattern = "5310D", 
                                     x = method_status) ~ "Wet Oxidation+Persulfate+IR",
                               grepl(pattern = "EPA 440.0", 
                                     x = method_status) ~ "Elemental Analyzer",
                               grepl(pattern = "EPA 9060A", 
                                     x = method_status) ~ "Carbonaceous Analyzer",
                               method_status == "Ambiguous" ~ "Ambiguous"))
  
  rm(doc_aggregated_methods)
  gc()
  
  doc_tiered <- doc_grouped_more %>%
    mutate(tiers = case_when(grouped %in% c("Wet Oxidation+Persulfate+IR",
                                            "Persulfate-UV/Heated Persulfate Oxidation+IR") ~ "Restrictive",
                             grouped == "Combustion+IR" ~ "Narrowed",
                             grouped %in% c("Ambiguous", "Carbonaceous Analyzer") ~ "Inclusive")) 
  
  doc_filter_tiers <- doc_tiered %>%
    filter(tiers != "Inclusive")
  
  # How many records removed due to methods?
  print(
    paste0(
      "Rows removed due to analytical method type: ",
      nrow(doc_harmonized_units) - nrow(doc_filter_tiers)
    )
  )
  
  dropped_methods <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(doc_harmonized_units) - nrow(doc_filter_tiers),
    n_rows = nrow(doc_filter_tiers),
    order = 6
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Final step in harmonization is to group and subset fractions of interest
  doc_harmonized <- doc_filter_tiers %>%
    mutate(
      harmonized_fraction = case_when(
        ResultSampleFractionText %in% c('Dissolved', 'Filtered, lab', 'Filterable', 
                                        'Filtered, field') ~ "Dissolved",
        ResultSampleFractionText %in% c('Total', 'Total Recovrble', 'Total Recoverable',
                                        'Recoverable', 'Unfiltered', "Acid Soluble", "Suspended",
                                        "Non-Filterable (Particle)") ~ "Total",
        ResultSampleFractionText %in% c('Fixed') ~ "Fixed",
        ResultSampleFractionText %in% c('Non-Filterable (Particle)') ~ 'Particle',
        is.na(ResultSampleFractionText) | ResultSampleFractionText %in% c(" ", "Field", "Bed Sediment",
                                                                          "Inorganic", "Organic") ~ "Ambiguous")
    ) %>%
    # Filter to dissolved fraction
    filter(harmonized_fraction == "Dissolved")
  
  # How many records removed due to methods? Similar number as AquaSat(1)
  print(
    paste0(
      "Rows removed due to fraction type: ",
      nrow(doc_filter_tiers) - nrow(doc_harmonized)
    )
  )
  
  dropped_fractions <- tibble(
    step = "doc harmonization",
    reason = "Dropped rows while filtering fraction types",
    short_reason = "Fraction types",
    number_dropped = nrow(doc_filter_tiers) - nrow(doc_harmonized),
    n_rows = nrow(doc_harmonized),
    order = 7
  )
  
  
  # Export ------------------------------------------------------------------
  
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_approximates, dropped_fails,
                                dropped_fractions, dropped_harmonization,
                                dropped_mdls, dropped_media, dropped_methods)
  
  documented_drops_out_path <- "3_harmonize/out/harmonize_doc_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_doc.feather"
  
  write_feather(doc_harmonized,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(doc_harmonized)
    )
  )
  
  return(list(
    harmonized_doc_path = data_out_path,
    compiled_drops_path = documented_drops_out_path))  
  
}