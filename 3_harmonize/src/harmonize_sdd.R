
harmonize_sdd <- function(raw_sdd, p_codes,
                          sdd_analytical_method_matchup,
                          sdd_sample_method_matchup,
                          sdd_equipment_matchup){
  
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
  
  # First step is to read in the data and do basic formatting and filtering
  sdd <- raw_sdd %>%
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    # Remove trailing white space in labels (Is this still necessary?)
    filter(
      ActivityMediaName %in% c("Water", "water")) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "sdd harmonization",
    reason = "Filtered for only water media",
    short_reason = "Water media",
    number_dropped = nrow(raw_sdd) - nrow(sdd),
    n_rows = nrow(sdd),
    order = 1
  )
  
  rm(raw_sdd)
  gc()
  
  
  # Remove fails ------------------------------------------------------------
  
  sdd_fails_removed <- sdd %>%
    filter(
      # REMOVE failure-related field comments, slightly different list of words
      # than lab and result list (not including things that could be used
      # to describe field conditions like "warm", "ice", etc.)
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "violation", "invalid", "unable", "no test", "cancelled",
            "instrument down", "no result", "time exceed", "not accept",
            "QC EXCEEDED"),
          collapse = "|"),
        x = ActivityCommentText,
        ignore.case = T
      ) |
        is.na(ActivityCommentText),
      # Remove failure-related lab (What about controls comments?):
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "beyond", "exceeded", "failed", "exceededs"),
          collapse = "|"),
        x = ResultLaboratoryCommentText,
        ignore.case = T
      ) |
        is.na(ResultLaboratoryCommentText),
      # Remove failure-related result comments
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
          collapse = "|"),
        x = ResultCommentText,
        ignore.case = T
      ) | is.na(ResultCommentText),
      # No failure-related values
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "not done", "no reading", "no secchi",
            "not reported", "no data"),
          collapse = "|"),
        x = ResultMeasureValue_original,
        ignore.case = T
      ) | is.na(ResultMeasureValue_original))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(sdd) - nrow(sdd_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows indicating fails, missing data, etc.",
    short_reason = "Fails, etc.",
    number_dropped = nrow(sdd) - nrow(sdd_fails_removed),
    n_rows = nrow(sdd_fails_removed),
    order = 2)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  non_detect_text <- "non-detect|not detect|non detect|undetect|below"
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- sdd_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(ResultMeasureValue)) %>%
    # if the value is na BUT there is non detect language in the comments...  
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
        no = ifelse(test = grepl("[0-9]", ResultMeasureValue_original) & grepl("<", ResultMeasureValue_original),
                    # ... use that number as the MDL
                    yes = str_replace_all(string = ResultMeasureValue_original,
                                          pattern = c("\\<"="", "\\*" = "", "\\=" = "" )),
                    no = NA)),
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
      round((nrow(mdl_updates)) / nrow(sdd_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
    )
  )
  
  # Replace "harmonized_value" field with these new values
  sdd_mdls_added <- sdd_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, ResultMeasureValue),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, ResultMeasure.MeasureUnitCode),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated using the EPA's MDL method.", NA))
  
  dropped_mdls <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(sdd_fails_removed) - nrow(sdd_mdls_added),
    n_rows = nrow(sdd_mdls_added),
    order = 3
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  approx_text <- "result approx|RESULT IS APPROX|value approx"
  
  sdd_approx <- sdd_mdls_added %>%
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
  
  sdd_approx$approx_value <- as.numeric(str_replace_all(sdd_approx$ResultMeasureValue_original, c("\\*" = "")))
  sdd_approx$approx_value[is.nan(sdd_approx$approx_value)] <- NA
  
  # Keep important data
  sdd_approx <- sdd_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(sdd_approx)) / nrow(sdd_mdls_added) * 100, 3),
      '% of samples had values listed as approximated'
    )
  )
  
  # Replace harmonized_value field with these new values
  sdd_approx_added <- sdd_mdls_added %>%
    left_join(x = ., y = sdd_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% sdd_approx$index,
                                     approx_value,
                                     harmonized_value),
           harmonized_comments = ifelse(index %in% sdd_approx$index,
                                        'Value identified as "approximated" by organization.',
                                        harmonized_comments))
  
  dropped_approximates <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(sdd_mdls_added) - nrow(sdd_approx_added),
    n_rows = nrow(sdd_approx_added),
    order = 4
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  greater_vals <- sdd_approx_added %>%
    filter((!index %in% mdl_updates$index) & (!index %in% sdd_approx$index)) %>%
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
      round((nrow(greater_vals)) / nrow(sdd_approx_added) * 100, 9),
      '% of samples had values listed as being above a detection limit//greater than'
    )
  )
  
  # Replace harmonized_value field with these new values
  sdd_harmonized_values <- sdd_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        'Value identified as being greater than listed value.',
                                        harmonized_comments))
  
  dropped_greater_than <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_harmonized_values),
    n_rows = nrow(sdd_harmonized_values),
    order = 5
  )
  
  # Free up memory
  rm(sdd)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Now count the units column: 
  unit_counts <- sdd_harmonized_values %>%
    count(ResultMeasure.MeasureUnitCode) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c("m", "ft", "cm", "in", "mm", "mi"),
    conversion = c(1, 0.3048, 0.01, 0.0254, 0.001, 1609.34)
  )
  
  converted_units_sdd <- sdd_harmonized_values %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = ResultMeasureValue * conversion,
           harmonized_units = "m") 
  
  # How many records removed due to unit harmonization?
  print(
    paste0(
      "Rows removed while converting to numeric values: ",
      nrow(sdd_harmonized_values) - nrow(converted_units_sdd)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(sdd_harmonized_values) - nrow(converted_units_sdd),
    n_rows = nrow(converted_units_sdd),
    order = 6
  )
  
  
  # Aggregate analytical methods --------------------------------------------
  
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
  
  # Add a new column aggregating the analytical methods groups
  grouped_analytical_methods_sdd <- converted_units_sdd %>%
    left_join(x = .,
              y = sdd_analytical_method_matchup,
              by = c("ResultAnalyticalMethod.MethodName" = "analytical_method")) %>%
    filter(analytical_method_grouping != "unlikely")
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_units_sdd) - nrow(grouped_analytical_methods_sdd)
    )
  )
  
  dropped_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_units_sdd) - nrow(grouped_analytical_methods_sdd),
    n_rows = nrow(grouped_analytical_methods_sdd),
    order = 7
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Now count the fraction column (the ungrouped version): 
  fraction_counts <- grouped_analytical_methods_sdd %>%
    count(ResultSampleFractionText) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the ResultSampleFractionText column
  grouped_fractions_sdd <- grouped_analytical_methods_sdd %>%
    mutate(aquasat_fraction = if_else(
      condition = ResultSampleFractionText %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
      true = "Makes sense",
      false = "Unlikely")) %>%
    filter(aquasat_fraction == "Makes sense")
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_sdd) - nrow(grouped_fractions_sdd)
    )
  )
  
  dropped_fractions <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while filtering fraction types",
    short_reason = "Fraction types",
    number_dropped = nrow(grouped_analytical_methods_sdd) - nrow(grouped_fractions_sdd),
    n_rows = nrow(grouped_fractions_sdd),
    order = 8
  )
  
  
  # Aggregate sample methods ------------------------------------------------
  
  # Now count the sample method column:
  sample_counts <- grouped_fractions_sdd %>%
    count(SampleCollectionMethod.MethodName) %>%
    arrange(desc(n))
  
  # Add a new column describing the sample method group:
  grouped_sample_methods_sdd <- grouped_fractions_sdd %>%
    left_join(x = .,
              y = sdd_sample_method_matchup,
              by = c("SampleCollectionMethod.MethodName" = "sample_method")) %>%
    filter(sample_method_grouping != "unlikely")
  
  # How many records removed due to unlikely sample methods?
  print(
    paste0(
      "Rows removed due to unlikely sample methods: ",
      nrow(grouped_fractions_sdd) - nrow(grouped_sample_methods_sdd)
    )
  )
  
  dropped_sample_methods <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating sample methods",
    short_reason = "Sample methods",
    number_dropped = nrow(grouped_fractions_sdd) - nrow(grouped_sample_methods_sdd),
    n_rows = nrow(grouped_sample_methods_sdd),
    order = 9
  )
  
  # Aggregate collection equipment ------------------------------------------
  
  # Now count the collection equipment column:
  equipment_counts <- grouped_sample_methods_sdd %>%
    count(SampleCollectionEquipmentName) %>%
    arrange(desc(n))
  
  grouped_equipment_sdd <- grouped_sample_methods_sdd %>%
    left_join(x = .,
              y = sdd_equipment_matchup,
              by = c("SampleCollectionEquipmentName" = "collection_equipment")) %>%
    filter(equipment_grouping != "unlikely")
  
  # How many records removed due to unlikely sample methods?
  print(
    paste0(
      "Rows removed due to unlikely sample methods: ",
      nrow(grouped_sample_methods_sdd) - nrow(grouped_equipment_sdd)
    )
  )
  
  dropped_equipment <- tibble(
    step = "sdd harmonization",
    reason = "Dropped rows while aggregating collection equipment",
    short_reason = "Collection equipment",
    number_dropped = nrow(grouped_sample_methods_sdd) - nrow(grouped_equipment_sdd),
    n_rows = nrow(grouped_equipment_sdd),
    order = 10
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_approximates, dropped_equipment, dropped_fails,
                                dropped_fractions, dropped_greater_than,
                                dropped_harmonization, dropped_mdls,
                                dropped_media, dropped_methods, dropped_sample_methods)
  
  documented_drops_out_path <- "3_harmonize/out/harmonize_sdd_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_sdd.feather"
  
  write_feather(grouped_equipment_sdd,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(grouped_equipment_sdd)
    )
  )
  
  return(list(
    harmonized_sdd_path = data_out_path,
    compiled_drops_path = documented_drops_out_path))  
  
}