
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
  
  # First step is to read in the data and do basic formatting and filtering
  chla <- raw_chla %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = c("USGSPCode" = "parm_cd")) %>%
    filter(
      ActivityMediaName %in% c("Water", "water")) %>%
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
  
  
  # Character name selection ------------------------------------------------
  
  chla_char_filter <- chla %>%
    filter(
      CharacteristicName %in% c('Chlorophyll a',
                                'Chlorophyll a (probe relative fluorescence)',
                                'Chlorophyll a, corrected for pheophytin',
                                'Chlorophyll a (probe)',
                                'Chlorophyll a, free of pheophytin',
                                'Chlorophyll a - Phytoplankton (suspended)'))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to non-target CharacteristicNames: ",
      nrow(chla) - nrow(chla_char_filter)
    )
  )
  
  dropped_chars <- tibble(
    step = "chla harmonization",
    reason = "Filtered for specific chlorophyll CharacteristicNames",
    short_reason = "Chla characteristics",
    number_dropped = nrow(chla) - nrow(chla_char_filter),
    n_rows = nrow(chla_char_filter),
    order = 2
  )
  
  
  # Document and remove fail language ---------------------------------------
  
  # The values that will be considered fails for each column:
  fail_text <- c(
    "beyond accept", "cancelled", "contaminat", "error", "fail", 
    "improper", "instrument down", "interference", "invalid", "no result", 
    "no test", "not accept", "outside of accept", "problem", "QC EXCEEDED", 
    "questionable", "suspect", "unable", "violation", "reject", "no data"
  )
  
  # Now get counts of string detections for each column:
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
                 hit_count <- chla_char_filter %>%
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
  chla_fails_removed <- chla_char_filter %>%
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
      nrow(chla_char_filter) - nrow(chla_fails_removed)
    )
  )
  
  dropped_fails <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows containing fail-related language",
    short_reason = "Fails, etc.",
    number_dropped = nrow(chla_char_filter) - nrow(chla_fails_removed),
    n_rows = nrow(chla_fails_removed),
    order = 3)
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- chla_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(ResultMeasureValue)) %>%
    # if the value is na BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(test = (is.na(ResultMeasureValue_original) & 
                                  (grepl("non-detect|not detect|non detect|undetect|below", ResultLaboratoryCommentText, ignore.case = TRUE) | 
                                     grepl("non-detect|not detect|non detect|undetect|below", ResultCommentText, ignore.case = TRUE) |
                                     grepl("non-detect|not detect|non detect|undetect|below", ResultDetectionConditionText, ignore.case = TRUE))) |
                          #.... OR, there is non-detect language in the value column itself....
                          grepl("non-detect|not detect|non detect|undetect|below", ResultMeasureValue_original, ignore.case = TRUE),
                        #... use the DetectionQuantitationLimitMeasure.MeasureValue value.
                        yes = DetectionQuantitationLimitMeasure.MeasureValue,
                        # if there is a `<` and a number in the values column...
                        no = ifelse(test = grepl("[0-9]", ResultMeasureValue_original) & grepl("<", ResultMeasureValue_original),
                                    # ... use that number as the MDL
                                    yes = str_replace_all(ResultMeasureValue_original, c("\\<"="", "\\*" = "", "\\=" = "" )),
                                    no = NA)),
      # preserve the units if they are provided:
      mdl_units = ifelse(!is.na(mdl_vals), DetectionQuantitationLimitMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode),
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
           is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             # ...AND any of the comment fields have approximation language...
             (grepl("result approx|RESULT IS APPROX|value approx", ResultLaboratoryCommentText, ignore.case = T)|
                grepl("result approx|RESULT IS APPROX|value approx", ResultCommentText, ignore.case = T )|
                grepl("result approx|RESULT IS APPROX|value approx", ResultDetectionConditionText, ignore.case = T)))
  
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
    filter(is.na(ResultMeasureValue) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", ResultMeasureValue_original) &
             #... AND a `>` symbol
             grepl(">", ResultMeasureValue_original))
  
  greater_vals$greater_value <- as.numeric(str_replace_all(greater_vals$ResultMeasureValue_original,
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
    count(ResultMeasure.MeasureUnitCode) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    ResultMeasure.MeasureUnitCode = c("mg/l", "mg/L", "ppm", "ug/l", "ug/L", "mg/m3", "ppb",
                                      "mg/cm3", "ug/ml", "mg/ml", "ppt"),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000,
                   1000000, 1000000))
  
  converted_units_chla <- chla_harmonized_values %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "ResultMeasure.MeasureUnitCode") %>%
    mutate(harmonized_value = ResultMeasureValue * conversion,
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
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla analytical methods present: ",
      length(unique(converted_depth_units_chla$ResultAnalyticalMethod.MethodName))
    )
  )
  
  # Below we'll implement a tiering system in two stages.
  # The first stage tags each record with an identifier based on the contents
  # of the analytical method column:
  #   - ⁠HPLC
  #   - ⁠fluorometer
  #   - ⁠spectrophotometer
  #   - ⁠other: another method than the previous three ones (e.g., an in situ method)
  #   - undefined: no information provided or not enough to determine the method
  #     from the content of this column alone
  #   - unrelated: a method seemingly unrelated to chlorophyll a
  
  # The second stage assigns coarser tiers based on the tags:
  #   - Restrictive (HPLC)
  #   - Narrowed (fluorometer, Spectrophotometer)
  #   - Inclusive (undefined, other)
  #   - Remove (unrelated) - these will be filtered out of the dataset
  
  # Define patterns to search when tagging chlorophyll a analytical methods
  fluorometer_text <- paste0(c("445", "fluor", "Welshmeyer", "fld"),
                             collapse = "|")
  hplc_text <- paste0(c("447", "chromatography", "hplc"),
                      collapse = "|") 
  spectrophotometer_text <- paste0(c("10200", "446", "trichromatic",
                                     "spectrophoto", "monochrom", "monchrom",
                                     # spec not as part of a word
                                     "(?<!\\w)spec"),
                                   collapse = "|")
  other_text <- paste0(c("Seabird CTD", "probe", "in situ", "oxygen demand",
                         # Find the text "ysi" but not part of, e.g., "analysis"
                         "\\bysi\\b", "\\bysi\\d+", "\\d+ysi\\b",
                         "WETStar"),
                       collapse = "|")
  unrelated_text <- paste0(c("sulfate", "sediment", "5310", "counting",
                             "plasma", "turbidity", "coliform", "carbon",
                             "2540", "conductance", "nitrate", "nitrite",
                             "nitrogen", "alkalin", "zooplankton",
                             "phosphorus", "periphyton", "peri",
                             "10300", "biomass", "temperature",
                             # Methods 10200 F & G
                             "10200[ -]?[fg]",
                             "elemental analyzer", "2320"),
                           collapse = "|")
  undefined_text <- paste0(c("unkn", "not specified", "unspecified",
                             "not available", "n/a"),
                           collapse = "|")
  
  # Create a separate column for each method tag
  tagged_methods_chla <- converted_depth_units_chla %>%
    rowwise() %>%
    mutate(
      hplc_tag = grepl(pattern = hplc_text,
                       x = ResultAnalyticalMethod.MethodName,
                       ignore.case = TRUE),
      fluoro_tag = grepl(pattern = fluorometer_text,
                         x = ResultAnalyticalMethod.MethodName,
                         ignore.case = TRUE),
      spectro_tag = grepl(pattern = spectrophotometer_text,
                          x = ResultAnalyticalMethod.MethodName,
                          ignore.case = TRUE,
                          perl = TRUE),
      other_tag = grepl(pattern = other_text,
                        x = ResultAnalyticalMethod.MethodName,
                        ignore.case = TRUE,
                        perl = TRUE),
      unrelated_tag = grepl(pattern = unrelated_text,
                            x = ResultAnalyticalMethod.MethodName,
                            ignore.case = TRUE),
      # Undefined is more complicated
      undefined_tag = case_when(
        grepl(pattern = undefined_text,
              x = ResultAnalyticalMethod.MethodName,
              ignore.case = TRUE) ~ TRUE,
        # NAs = undefined
        is.na(ResultAnalyticalMethod.MethodName) ~ TRUE,
        # If there's a non-undefined tag then it's not undefined
        any(c(hplc_tag, fluoro_tag, spectro_tag,
              other_tag, unrelated_tag)) ~ FALSE,
        # If there's more than one tag in the other columns then it's undefined
        sum(c(hplc_tag, fluoro_tag, spectro_tag,
              other_tag, unrelated_tag)) > 1 ~ TRUE,
        # Defaults everything else to undefined
        .default = TRUE)) 
  
  # Now tier based on the tags above
  tiered_methods_chla <- tagged_methods_chla %>%
    rowwise() %>%
    mutate(
      # If a lower tiered method is also mentioned in the same string then don't
      # assign it to restrictive tier. There are some with multiple mentioned
      restrictive_tier = if_else(condition = (hplc_tag == TRUE) &
                                   (fluoro_tag != TRUE) &
                                   (spectro_tag != TRUE) &
                                   (other_tag != TRUE) &
                                   (unrelated_tag != TRUE) &
                                   (undefined_tag != TRUE),
                                 true = TRUE,
                                 false = FALSE),
      narrowed_tier = if_else(condition = any(hplc_tag, fluoro_tag, spectro_tag),
                              true = TRUE,
                              false = FALSE),
      inclusive_tier = if_else(condition = any(hplc_tag, fluoro_tag, spectro_tag,
                                               other_tag, undefined_tag),
                               true = TRUE,
                               false = FALSE),
      # If undef/unrelat/other, then cannot be restrictive or narrowed. Fixes any
      # with, e.g., multiple tags
      across(c(restrictive_tier, narrowed_tier),
             .fns = ~if_else(any(undefined_tag, unrelated_tag, other_tag), FALSE, .x)),
      # If unrelated, then cannot be inclusive either
      inclusive_tier = if_else(condition = unrelated_tag,
                               true = FALSE,
                               false = inclusive_tier)
    ) %>%
    ungroup()
  
  # Export a record of how methods were tiered and their respective row counts
  tiering_record <- tiered_methods_chla %>%
    add_count(ResultAnalyticalMethod.MethodName) %>%
    select(ResultAnalyticalMethod.MethodName, n, contains("_tag"), contains("_tier")) %>%
    arrange(desc(n)) %>%
    distinct() 
  
  tiering_record_out_path <- "3_harmonize/out/chla_tiering_record.csv"
  
  write_csv(x = tiering_record, file = tiering_record_out_path)
  
  # Filter and slim the tiered product
  cleaned_tiered_methods_chla <- tiered_methods_chla %>%
    # No unrelated methods
    filter(unrelated_tag == FALSE) %>%
    # Drop tag columns - these are recorded and exported in tiering_record
    select(-contains("_tag"))
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unrelated analytical methods: ",
      nrow(converted_depth_units_chla) - nrow(cleaned_tiered_methods_chla)
    )
  )
  
  dropped_methods <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while tiering analytical methods",
    short_reason = "Analytical methods",
    number_dropped = nrow(converted_depth_units_chla) - nrow(cleaned_tiered_methods_chla),
    n_rows = nrow(cleaned_tiered_methods_chla),
    order = 9
  )
  
  
  # Tier field methods ------------------------------------------------------
  
  # Function to build tiers based on the fraction and depth data
  field_tiers_chla <- assign_field_tier(dataset = cleaned_tiered_methods_chla)
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed while assigning field tiers: ",
      nrow(cleaned_tiered_methods_chla) - nrow(field_tiers_chla)
    )
  )
  
  dropped_field <- tibble(
    step = "chla harmonization",
    reason = "Dropped rows while assigning field tiers",
    short_reason = "Field tiers",
    number_dropped = nrow(cleaned_tiered_methods_chla) - nrow(field_tiers_chla),
    n_rows = nrow(field_tiers_chla),
    order = 10
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Record of all steps where rows were dropped, why, and how many
  compiled_dropped <- bind_rows(starting_data, dropped_approximates, dropped_depths, dropped_fails, 
                                dropped_field, dropped_greater_than, dropped_harmonization, 
                                dropped_mdls, dropped_media, dropped_methods, dropped_chars)
  
  documented_drops_out_path <- "3_harmonize/out/harmonize_chla_dropped_metadata.csv"
  
  write_csv(x = compiled_dropped,
            file = documented_drops_out_path)
  
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_chla.feather"
  
  write_feather(field_tiers_chla,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(field_tiers_chla)
    )
  )
  
  return(list(
    harmonized_chla_path = data_out_path,
    compiled_drops_path = documented_drops_out_path,
    chla_tiering_record_path = tiering_record_out_path))  
}
