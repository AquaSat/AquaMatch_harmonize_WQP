
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
    mutate(ResultMeasureValue = abs(ResultMeasureValue)) %>% # this cannot be here
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
  
  # fail language from chl-a, and is not super applicable to secchi, but will be
  # used for now to move on to the next issue
  
  fail_text <- c(
    "error", "fail", "invalid", "no result",  "questionable", "suspect", 
    "unable", "reject", "no data", "Not Reported"
  )
  
  # update this to make sure that we aren't dropping bottom ***
  # fail (and check what the bottom text is saying)
  # activity comment text column ***
  
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
  
  # Now that the fails have been documents, remove them:
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
    # only want NAs and where there is a `<` and a number in the values column... 
    filter(is.na(ResultMeasureValue) &
           grepl("<", ResultMeasureValue_original) &
           grepl("[0-9]", ResultMeasureValue_original)) %>%
    # replace "harmonize_value" field with the values sans '<'
    mutate(harmonized_value = str_replace_all(string = ResultMeasureValue_original, pattern = c("\\<"="")),
           # Preserve the units if they are provided 
           harmonized_units = ResultMeasure.MeasureUnitCode,
           harmonized_comments = "Approximated during MDL step") %>% 
    # keep important data
    select(index, harmonized_value, harmonized_units, harmonized_comments)
  
  print(
    paste(
      round(nrow(mdl_updates) / nrow(sdd_fails_removed) * 100, 3),
      "% of samples had values listed as being below a detection limit"
      )
    )
  
  # Update sdd_mdls added with mdl_updates
    # Flag after harmonizing units
  sdd_mdls_added <- sdd_fails_removed %>% 
    left_join(x = ., y = mdl_updates, by = "index") %>% # flag by index
    mutate(harmonized_value = ifelse(!(index %in% mdl_updates$index), ResultMeasureValue, harmonized_value),
           harmonized_units = ifelse(!(index %in% mdl_updates$index), ResultMeasure.MeasureUnitCode, harmonized_units))
    
  
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
  
  # The approximate flag is different here because:
    # There is no approximated text in the values column and the approx language
    # in the comments are not related to secchi. There are two instances of approx text
    # actually being related to secchi.
    
    # ignoring "*" because there is 5 counts of it being stored with numeric data
  
  # assuming that ResultMeasureValue has the most correct value and that
  # ResultMeasureValue.UnitCode has the most correct units
  
  # if there is no unit or if there is no value it will get dropped during the harmonization and NA removal step
  
  # flag 0 = value not adjusted using activity depth measure column
  # flag 1 = value filled using activity depth measure column 
  # flag 2 = unit filled using activity depth measure unit column 
  # flag 3 = value and unit filled using activity depth measure/measure unit column 
  
  # ignore this text because it will get resolved in the following steps
  # flag 4 = bottom depth flag
  ignore_value_text <- paste(
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
    ">", "*", "=",
    sep = "|"
  )
  
  ignore_comment_text <- paste(
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
  
  sdd_approx <- sdd_mdls_added %>%
    # ignore samples that have indications of ignore text 
    filter(!grepl(ignore_comment_text, ResultCommentText, ignore.case = T)) %>% 
    filter(!grepl(ignore_value_text, ResultMeasureValue_original, ignore.case = T)) %>% 
    # save samples where the NUMERIC value column or UNITS are NA:
    filter(is.na(ResultMeasureValue)|is.na(ResultMeasure.MeasureUnitCode)) %>% 
    # save samples that have data in depth height measure value or depth height measure unit column
    filter(!is.na(ActivityDepthHeightMeasure.MeasureValue)|!is.na(ActivityDepthHeightMeasure.MeasureUnitCode))
  
  # Keep important data
  sdd_approx <- sdd_approx %>%
    select(index, 
           ResultMeasureValue_original, ResultMeasureValue, ResultMeasure.MeasureUnitCode, 
           ResultCommentText, harmonized_value, harmonized_units,
           ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode)
  
  print(
    paste(
      round((nrow(sdd_approx)) / nrow(sdd_mdls_added) * 100, 3),
      "% of samples have missing numeric values or missing units that can be approximated from depth measure columns."
    )
  )
  
  # Replace harmonized_value field with these new values
  sdd_approx_added <- sdd_mdls_added %>%
    mutate(
      # flag 0 = value and units not adjusted
      # flag 1 = value filled using activity depth measure column 
      # flag 2 = unit filled using activity depth measure unit column 
      # flag 3 = value and unit filled using activity depth measure/measure unit column 
      approx_flag = case_when(
        !index %in% sdd_approx$index ~ 0,
        index %in% sdd_approx$index & is.na(harmonized_value) & !is.na(ActivityDepthHeightMeasure.MeasureValue) & 
          is.na(harmonized_units) & !is.na(ActivityDepthHeightMeasure.MeasureUnitCode) ~ 3,
        index %in% sdd_approx$index & is.na(harmonized_units) & !is.na(ActivityDepthHeightMeasure.MeasureUnitCode) ~ 2,
        index %in% sdd_approx$index & is.na(harmonized_value) & !is.na(ActivityDepthHeightMeasure.MeasureValue) ~ 1,
        .default = NA_integer_
      ),
      harmonized_value = if_else(
        index %in% sdd_approx$index & is.na(harmonized_value),
        ActivityDepthHeightMeasure.MeasureValue,
        harmonized_value
      ),
      harmonized_units = if_else(
        index %in% sdd_approx$index & is.na(harmonized_units),
        ActivityDepthHeightMeasure.MeasureUnitCode,
        harmonized_units
      ),
      harmonized_comments = case_when(
        approx_flag == 3 ~ "Value and units 'approximated' from `ActivityDepthHeightMeasure.MeasureValue` 
        and `ActivityDepthHeightMeasure.MeasureUnitCode` columns.",
        approx_flag == 2 ~ "Units 'approximated' from `ActivityDepthHeightMeasure.MeasureUnitCode` column.",
        approx_flag == 1 ~ "Value 'approximated' from `ActivityDepthHeightMeasure.MeasureValue` column.",
        .default = NA_character_
      )
    )
  
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
             #... AND a `>`, `*`, or `=` symbol (included this because they are replaced by "" in the next step)
             grepl(">|\\*|=", ResultMeasureValue_original))
  
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
      round((nrow(greater_vals)) / nrow(sdd_approx_added) * 100, 3),
      "% of samples had values listed have symbols `>`,`*`,`=` in the original value column."
    )
  )
  
  sdd_harmonized_values <- sdd_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        "Special character (greater than) flag.",
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
    number_dropped = nrow(sdd_approx_added) - nrow(sdd_harmonized_values),
    n_rows = nrow(sdd_harmonized_values),
    order = 5
  )
  
  # Free up memory
  rm(sdd)
  gc()
  
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
  
  # Remove remaining NAs ----------------------------------------------------
  
  # At this point we've processed MDLs, approximate values, values containing
  # symbols like ">", and have updated NA values due to bottom outs. 
  # If there are still remaining NAs in the numeric measurement
  # column then it's time to drop them.
  
  # maintain harmonized values that are not NA 
  sdd_no_na <- sdd_bottom_depth_added %>%
    # Keep rows if non-NA in values and units OR...
    filter((!is.na(harmonized_value) & !is.na(harmonized_units)) |
      # keep rows if there are indications of bottom in the comments
        grepl(bottom_text, ResultMeasureValue_original, ignore.case=T) |
        grepl(bottom_text, ResultCommentText, ignore.case=T) |
        grepl(bottom_text, ActivityCommentText, ignore.case=T)
    )

  print(
    paste(
      round((nrow(sdd_no_na)) / nrow(sdd_bottom_depth_added) * 100, 2),
      "% of samples remain after removing NA values."
    )
  )
  
  dropped_na <- tibble(
    step = "sdd drop NAs",
    reason = "Dropped unresolved NAs",
    short_reason = "Unresolved NAs",
    number_dropped = nrow(sdd_bottom_depth_added) - nrow(sdd_no_na),
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
  
  converted_units_sdd <- sdd_no_na %>%
    left_join(x = .,
               y = unit_conversion_table,
               by = "harmonized_units") %>%
    mutate(harmonized_value = as.numeric(harmonized_value) * conversion,
           harmonized_units = "m") %>% 
    # filter for successful converted units and bottoms
    filter(
      (!is.na(harmonized_value)) |
        # keep rows if there are indications of bottom in the comments
        grepl(bottom_text, ResultMeasureValue_original, ignore.case=T) |
        grepl(bottom_text, ResultCommentText, ignore.case=T) |
        grepl(bottom_text, ActivityCommentText, ignore.case=T)
      )
  
  # How many records removed due to unit harmonization?
  print(
    paste0(
      "Rows removed while converting to numeric values: ",
      nrow(sdd_no_na) - nrow(converted_units_sdd)
    )
  )
  
  dropped_harmonization <- tibble(
    step = "sdd unit conversion",
    reason = "Dropped rows while harmonizing units",
    short_reason = "Harmonize units",
    number_dropped = nrow(sdd_no_na) - nrow(converted_units_sdd),
    n_rows = nrow(converted_units_sdd),
    order = 6
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
  