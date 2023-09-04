
harmonize_chla <- function(raw_chla, p_codes, chla_analytical_method_matchup){
  
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
  
  # First step is to read in the data and do basic formatting and filtering
  chla <- raw_chla %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    filter(
      media %in% c("Water", "water")) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Record info on any dropped rows  
  dropped_media <- tibble(
    step = "chla harmonization",
    reason = "Filtered for only water media",
    short_reason = "Water media",
    number_dropped = nrow(raw_chla) - nrow(chla),
    n_rows = nrow(chla),
    order = 1
  )
  
  rm(raw_chla)
  gc()
  
  
  # Parameter name selection ------------------------------------------------
  
  chla_param_filter <- chla %>%
    filter(
      orig_parameter %in% c('Chlorophyll a', 'Chlorophyll a (probe relative fluorescence)',
                            'Chlorophyll a, corrected for pheophytin', 'Chlorophyll a (probe)',
                            'Chlorophyll a, free of pheophytin', 'Chlorophyll a - Phytoplankton (suspended)'))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to non-target parameter names: ",
      nrow(chla) - nrow(chla_param_filter)
    )
  )
  
  dropped_parameters <- tibble(
    step = "chla harmonization",
    reason = "Filtered for specific chlorophyll parameters",
    short_reason = "Chla params",
    number_dropped = nrow(chla) - nrow(chla_param_filter),
    n_rows = nrow(chla_param_filter),
    order = 2
  )
  
  
  # Remove fails ------------------------------------------------------------
  
  chla_fails_removed <- chla_param_filter %>%
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
        x = field_comments,
        ignore.case = T
      ) |
        is.na(field_comments),
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
        x = lab_comments,
        ignore.case = T
      ) |
        is.na(lab_comments),
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
        x = result_comments,
        ignore.case = T
      ) | is.na(result_comments),
      # No failure-related values
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "not done", "no reading", "not reported",
            "no data"),
          collapse = "|"),
        x = value,
        ignore.case = T
      ) | is.na(value))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(chla_param_filter) - nrow(chla_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows indicating fails, missing data, etc.",
    short_reason = "Fails, etc.",
    number_dropped = nrow(chla_param_filter) - nrow(chla_fails_removed),
    n_rows = nrow(chla_fails_removed),
    order = 3)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- chla_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(value_numeric)) %>%
    # if the value is na BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(test = (is.na(value) & 
                                  (grepl("non-detect|not detect|non detect|undetect|below", lab_comments, ignore.case = TRUE) | 
                                     grepl("non-detect|not detect|non detect|undetect|below", result_comments, ignore.case = TRUE) |
                                     grepl("non-detect|not detect|non detect|undetect|below", ResultDetectionConditionText, ignore.case = TRUE))) |
                          #.... OR, there is non-detect language in the value column itself....
                          grepl("non-detect|not detect|non detect|undetect|below", value, ignore.case = TRUE),
                        #... use the DetectionQuantitationLimitMeasure.MeasureValue value.
                        yes = DetectionQuantitationLimitMeasure.MeasureValue,
                        # if there is a `<` and a number in the values column...
                        no = ifelse(test = grepl("[0-9]", value) & grepl("<", value),
                                    # ... use that number as the MDL
                                    yes = str_replace_all(value, c("\\<"="", "\\*" = "", "\\=" = "" )),
                                    no = NA)),
      # preserve the units if they are provided:
      mdl_units = ifelse(!is.na(mdl_vals), DetectionQuantitationLimitMeasure.MeasureUnitCode, units),
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
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, value_numeric),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, units),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated using the EPA's MDL method.", NA))
  
  dropped_mdls <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning MDLs",
    short_reason = "Clean MDLs",
    number_dropped = nrow(chla_fails_removed) - nrow(chla_mdls_added),
    n_rows = nrow(chla_mdls_added),
    order = 4
  )
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  chla_approx <- chla_mdls_added %>%
    # First, remove the samples that we've already approximated using the EPA method:
    filter(!index %in% mdl_updates$index,
           # Then select fields where the numeric value column is NA....
           is.na(value_numeric) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", value) &
             # ...AND any of the comment fields have approximation language...
             (grepl("result approx|RESULT IS APPROX|value approx", lab_comments, ignore.case = T)|
                grepl("result approx|RESULT IS APPROX|value approx", result_comments, ignore.case = T )|
                grepl("result approx|RESULT IS APPROX|value approx", ResultDetectionConditionText, ignore.case = T)))
  
  chla_approx$approx_value <- as.numeric(str_replace_all(chla_approx$value, c("\\*" = "")))
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
           harmonized_comments = ifelse(index %in% chla_approx$index,
                                        'Value identified as "approximated" by organization.',
                                        harmonized_comments))
  
  dropped_approximates <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning approximate values",
    short_reason = "Clean approximates",
    number_dropped = nrow(chla_mdls_added) - nrow(chla_approx_added),
    n_rows = nrow(chla_approx_added),
    order = 5
  )
  
  
  # Clean up "greater than" values ------------------------------------------
  
  greater_vals <- chla_approx_added %>%
    filter((!index %in% mdl_updates$index) & (!index %in% chla_approx$index)) %>%
    # Then select fields where the NUMERIC value column is NA....
    filter(is.na(value_numeric) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", value) &
             #... AND a `>` symbol
             grepl(">", value))
  
  greater_vals$greater_value <- as.numeric(str_replace_all(greater_vals$value,
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
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        'Value identified as being greater than listed value.',
                                        harmonized_comments))
  
  dropped_greater_than <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning 'greater than' values",
    short_reason = "Greater thans",
    number_dropped = nrow(chla_approx_added) - nrow(chla_harmonized_values),
    n_rows = nrow(chla_harmonized_values),
    order = 6
  )
  
  
  # Free up memory
  rm(chla)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Now count the units column: 
  unit_counts <- chla_harmonized_values %>%
    count(units) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    units = c("mg/l", "mg/L", "ppm", "ug/l", "ug/L", "mg/m3", "ppb", "mg/cm3", "ug/ml", "mg/ml", "ppt"),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000, 1000000, 1000000)
  )
  
  converted_units_chla <- chla_harmonized_values %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "units") %>%
    mutate(harmonized_value = value_numeric * conversion,
           harmonized_unit = "ug/L") %>%
    # MR limit 
    filter(harmonized_value < 1000)
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(chla_harmonized_values) - nrow(converted_units_chla)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(chla_harmonized_values) - nrow(converted_units_chla),
    n_rows = nrow(converted_units_chla),
    order = 7
  )
  
  
  # Clean up depths ---------------------------------------------------------
  
  # As with value col, check for entries with potential salvageable data. But don't
  # create a flag column for this one
  salvage_depths <- converted_units_chla %>%
    filter(grepl(x = ActivityDepthHeightMeasure.MeasureValue, pattern = "-|>|<|=")) %>%
    count(ActivityDepthHeightMeasure.MeasureValue)
  
  salvage_depth_units <- converted_units_chla %>%
    count(ActivityDepthHeightMeasure.MeasureUnitCode)
  
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  converted_depth_units_chla <- converted_units_chla %>%
    left_join(x = .,
              y = depth_unit_conversion_table,
              by = c("ActivityDepthHeightMeasure.MeasureUnitCode" = "depth_units")) %>%
    mutate(harmonized_depth_value = as.numeric(ActivityDepthHeightMeasure.MeasureValue) * depth_conversion,
           harmonized_depth_unit = "m") %>%
    # Surface limits - for the time being using two columns for this. Make sure
    # numeric value is within +/-2m OR the raw character version indicates something
    # similar:
    filter(abs(harmonized_depth_value) <= 2 |
             ActivityDepthHeightMeasure.MeasureValue %in% c("0-2", "0-0.5")|
             # Don't want to lose all the NA depth records
             is.na(ActivityDepthHeightMeasure.MeasureValue))
  
  # How many records removed due to limits on depth?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_chla) - nrow(converted_depth_units_chla)
    )
  )
  
  dropped_depths <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while cleaning depths",
    short_reason = "Clean depths",
    number_dropped = nrow(converted_units_chla) - nrow(converted_depth_units_chla),
    n_rows = nrow(converted_depth_units_chla),
    order = 8
  )
  
  
  # Aggregate analytical methods --------------------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla analytical methods present: ",
      length(unique(converted_depth_units_chla$analytical_method))
    )
  )
  
  analytical_counts <- converted_depth_units_chla %>%
    count(analytical_method) %>%
    arrange(desc(n))
  
  # Add a new column aggregating the analytical methods groups, then filter out
  # unlikely ones
  grouped_analytical_methods_chla <- converted_depth_units_chla %>%
    left_join(x = .,
              y = chla_analytical_method_matchup,
              by = c("analytical_method")) %>%
    filter(!is.na(analytical_method_grouping),
           analytical_method_grouping != "unlikely")
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_depth_units_chla) - nrow(grouped_analytical_methods_chla)
    )
  )
  
  dropped_methods <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while aggregating analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_depth_units_chla) - nrow(grouped_analytical_methods_chla),
    n_rows = nrow(grouped_analytical_methods_chla),
    order = 9
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Now count the ResultSampleFractionText column
  fraction_counts <- grouped_analytical_methods_chla %>%
    count(ResultSampleFractionText) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the ResultSampleFractionText column
  grouped_fractions_chla <- grouped_analytical_methods_chla %>%
    mutate(aquasat_fraction = if_else(
      condition = ResultSampleFractionText %in% c("Non-Filterable (Particle)", "Suspended",
                                  "Non-filterable", "<Blank>", "Acid Soluble"),
      true = "Nonsensical",
      false = "Makes sense")) %>%
    filter(aquasat_fraction == "Makes sense")
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_chla) - nrow(grouped_fractions_chla)
    )
  )
  
  dropped_fractions <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while filtering fraction types",
    short_reason = "Fraction types",
    number_dropped = nrow(grouped_analytical_methods_chla) - nrow(grouped_fractions_chla),
    n_rows = nrow(grouped_fractions_chla),
    order = 10
  )
  
  
  # Aggregate sample methods ------------------------------------------------
  
  # Get an idea of how many sample methods exist:
  print(
    paste0(
      "Number of chla sample methods present in raw dataset: ",
      length(unique(grouped_fractions_chla$sample_method)),
      ". Skipping due to length."
    )
  )
  
  
  # Aggregate collection equipment ------------------------------------------
  
  # Get an idea of how many equipment types exist:
  print(
    paste0(
      "Number of chla equipment types present in raw dataset: ",
      length(unique(grouped_fractions_chla$collection_equipment)),
      ". Skipping due to length."
    )
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_approximates, dropped_depths, dropped_fails, 
                                dropped_fractions, dropped_greater_than, dropped_harmonization, 
                                dropped_mdls, dropped_media, dropped_methods, dropped_parameters)
  
  documented_drops_out_path <- "3_harmonize/out/harmonize_chla_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_chla.feather"
  
  write_feather(grouped_fractions_chla,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(grouped_fractions_chla)
    )
  )
  
  return(list(
    harmonized_chla_path = data_out_path,
    compiled_drops_path = documented_drops_out_path))  
}
