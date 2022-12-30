# A function to repair a data frame that maps state names to the FIPS codes
# used by WQP
get_wqp_state_codes <- function() {
  
  states_df <- stateCd %>%
    filter(STATE < 60) %>%
    mutate(
      value = paste0("US:", STATE),
      name = STATE_NAME) %>%
    select(value, name)
  
  return(states_df)
  
}

# A function to pull the parameter codes from the USGS website and save them
# as a table for use in the cleaning process
get_p_codes <- function(){
  
  # Scrape URL
  site_url <- "https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
  
  # Pull table from website
  code_table <- read_html(site_url) %>%
    html_node("table") %>%
    html_table()
  
  # Get parameter codes from table
  p_codes <- code_table %>%
    clean_names() %>%
    mutate(parm_cd = str_pad(string = as.character(parameter_code), 
                             width = 5,
                             pad = "0"))
  
  return(p_codes)
}

# A function that creates a table with shortened names for WQP columns alongside
# their longer original counterparts
create_match_table <- function(){
  
  match_table <- tribble(
    ~short_name,          ~wqp_name, 
    "date", "ActivityStartDate",
    "orig_parameter", "CharacteristicName",
    "parm_cd", "USGSPCode",
    "units", "ResultMeasure.MeasureUnitCode",
    "SiteID", "MonitoringLocationIdentifier",
    "org", "OrganizationFormalName",
    "org_id", "OrganizationIdentifier",
    "time", "ActivityStartTime.Time",
    "value", "ResultMeasureValue",
    "sample_method", "SampleCollectionMethod.MethodName",
    "analytical_method", "ResultAnalyticalMethod.MethodName",
    "particle_size", "ResultParticleSizeBasisText",
    "date_time", "ActivityStartDateTime",
    "media", "ActivityMediaName",
    "type", "ActivityMediaSubdivisionName",
    "sample_depth", "ActivityDepthHeightMeasure.MeasureValue",
    "sample_depth_unit", "ActivityDepthHeightMeasure.MeasureUnitCode",
    "fraction", "ResultSampleFractionText",
    "status", "ResultStatusIdentifier",
    "field_comments", "ActivityCommentText",
    "lab_comments", "ResultLaboratoryCommentText",
    "result_comments", "ResultCommentText"
  )
  
  return(match_table)
}

# Function for making a nice table that gets a summary of units and the number 
# of observations with that unit code. (Adapted from AquaSat)
unit_kable <- function(data){
  
  data %>%
    group_by(units) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    kable(., "html", caption = "All  parameter and unit combinations") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}

# Function for making a nice table that gets a summary of units and the number 
# of observations with that analytical method. (Adapted from AquaSat)
analytical_kable <- function(data){
  
  data %>%
    group_by(analytical_method) %>%
    summarize(count = n()) %>% 
    arrange(desc(count)) %>%
    kable(., "html", caption = "All analytical methods and their count") %>%
    kable_styling() %>%
    scroll_box(width = "600px", height = "400px")
  
}

# Function for making a nice table that gets a summary of nonsensical units
# and the number of observations with that analytical method.
# (Adapted from AquaSat)
unit_disharmony <- function(data, lookup){
  
  data %>%
    anti_join(x = ., y = lookup, by = "units") %>%
    group_by(units) %>%
    summarize(count = n())  %>%
    kable(., "html", caption = "The following measurements
          were dropped because the units do not make sense") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}


# From USGS WQP pipeline:

#' @title Flag missing results
#' 
#' @description 
#' Function to flag true missing results, i.e. when the result measure value 
#' and detection limit value are both NA, when "not reported" is found in the
#' column "ResultDetectionConditionText", or when any of the strings from
#' `commenttext_missing` are found in the column "ResultCommentText".
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. Must contain the columns
#' "DetectionQuantitationLimitMeasure.MeasureValue", "ResultMeasureValue", 
#' "ResultDetectionConditionText", and "ResultCommentText".
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values.
#' 
#' @returns
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for missing results. 
#' 
flag_missing_results <- function(wqp_data, commenttext_missing){
  
  wqp_data_out <- wqp_data %>%
    mutate(flag_missing_result = 
             ( is.na(value) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ) |
             grepl("not reported", ResultDetectionConditionText, ignore.case = TRUE) |
             grepl(paste(commenttext_missing, collapse = "|"), result_comments, ignore.case = TRUE)
    )
  
  return(wqp_data_out)
  
}

# From USGS WQP pipeline:

#' @title Flag duplicated records
#' 
#' @description 
#' Function to flag duplicated rows based on a user-supplied definition
#' of a duplicate record. 
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#'
#' @returns 
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for duplicated records. 
#' 
flag_duplicates <- function(wqp_data, duplicate_definition){
  
  # Flag duplicate records using the `duplicate_definition`
  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    mutate(n_duplicated = n(),
           flag_duplicated_row = n_duplicated > 1) %>% 
    ungroup() %>%
    select(-n_duplicated)
  
  return(wqp_data_out)
  
}

# From USGS WQP pipeline:

#' @title Remove duplicated records
#' 
#' @description
#' Function to append additional flags to sets of duplicate rows that are then 
#' used to drop duplicates from the dataset. Currently, we randomly retain the 
#' first record in a set of duplicated rows and drop all others.
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record.
#' @param duplicate_definition character string(s) indicating which columns are
#' used to identify a duplicate record. Duplicate records are defined as those 
#' that share the same value for each column within `duplicate_definition`.
#' 
#' @returns 
#' Returns a data frame containing data downloaded from the Water Portal in which
#' duplicated rows have been removed. 
#' 
remove_duplicates <- function(wqp_data, duplicate_definition){
  
  wqp_data_out <- wqp_data %>%
    group_by(across(all_of(duplicate_definition))) %>% 
    # arrange all rows to maintain consistency in row order across users/machines;
    # the rows should be ordered the same way across machines so that when we 
    # "randomly" select the first duplicated row below, the output is consistent
    # for all users.
    arrange(across(c(all_of(duplicate_definition), everything()))) %>%
    # To help resolve duplicates, randomly select the first record
    # from each duplicated set and flag all others for exclusion.
    mutate(n_duplicated = n(),
           dup_number = seq(n_duplicated),
           flag_duplicate_drop_random = n_duplicated > 1 & dup_number != 1) %>%
    filter(flag_duplicate_drop_random == FALSE) %>%
    ungroup() %>%
    select(-c(n_duplicated, dup_number, flag_duplicate_drop_random))
  
  return(wqp_data_out)
  
}


# Function to harmonize the silica data drawn from WQP, prepare it for use further
# down the pipeline, and provide summarized outputs for use in an Rmd/Qmd to
# illustrate the process
harmonize_silica <- function(raw_silica,
                             p_codes,
                             commenttext_missing,
                             duplicate_definition,
                             match_table){
  
  # Aggregating -------------------------------------------------------------
  # From https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names
  
  raw_silica <- raw_silica %>%
    # Keep and rename columns of interest
    rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
                .cols = match_table$wqp_name) %>%
    # Add in codes from WQP  
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    mutate(year = year(date),
           units = trimws(units)) %>%
    filter(year >= 1984,
           media %in% c("Water", "water"),
           type %in% c("Surface Water", "Water", "Estuary", "Ocean Water", 
                       "Mixing Zone") |
             is.na(type)) %>%
    rowid_to_column(., var = "index")
  
  
  # Data lumping ------------------------------------------------------------
  
  # Flagging by noted analytical method
  classified_methods <- raw_silica %>%
    mutate(method_status = case_when(
      # Silica-specific:
      grepl(pattern = "4500-SiO2 C|4500 SiO2-C|4500 Si-C|4500 Si C|
          4500C|4500 C|4500-SiC|4500-SI02 C|4500 SIO2 C|
          4500-SIO2C", 
            x = analytical_method,
            ignore.case = T) ~ "Molybdosilicate Method",
      
      grepl("4500-SiO2 D|4500 SiO2-D|4500 Si-D|4500 Si D|
          4500D|4500 D|4500-SiD", 
            analytical_method, 
            ignore.case = T) ~ "Heteropoly Blue Method",
      
      grepl("370.1", 
            analytical_method,
            ignore.case = T) ~ "EPA Method 370.1",
      
      grepl("4500-SiO2 E|4500 SiO2-E|4500 Si-E|4500 Si E|4500E|
          4500 E|4500-SiE|Technicon Industrial Method 105-71|
          4500 SIO2 E", 
            analytical_method,
            ignore.case = T) ~ "Automated Method for Molybdate-Reactive Silica",
      
      grepl("4500-SiO2 F|4500 SiO2-F|4500 Si-F|4500 Si F|4500F|
          4500 F|4500-SiF|4500 SIO2 F", 
            analytical_method,
            ignore.case = T) ~ "Flow Injection Analysis for Molybdate-Reactive Silicate",
      
      grepl("366",
            analytical_method,
            ignore.case = T) ~ "EPA Method 366.0",
      
      # Inorganic/Metals general (many of these  do not list Si specifically, 
      # though SM section 4500 lists them as appropriate methods):
      grepl("3111 D|3111-D|Nitrous Oxide|FLAA",
            analytical_method,
            ignore.case = T) ~ "Direct Nitrous Oxide-Acetylene Flame Method",
      
      grepl("3120|200.7|6010|Plasma Emission Spectrosc|ICP-AES|ICP AES|
          ICP-OES|ICP-OES|ICP OES|ICP/OES|Inductively Coupled Plasma AES|
          Atomic Emission Spec|Optical Emission Spec|Atomic Emission Spectrometry", # is this real though 
            analytical_method,
            ignore.case = T) ~ "ICP-AES",
      
      grepl("3113|Atomic Absorption|GFAA|graphite furnace", 
            analytical_method,
            ignore.case = T) ~ "Electrothermal Atomic Absorption Spectrometric Method",
      
      grepl("3125|200.8|ICP/MS|ICP MS|ICP-MS|plasma/mass spec",
            analytical_method,
            ignore.case = T) ~ "ICP/MS",
      
      # General ICP
      grepl("Silica in Water by ICP| ICP, EPA method|ICP Package",
            analytical_method,
            ignore.case = T) ~ "Just ICP"))
  
  # Further narrowing methods classifications into coarser groups
  narrowed_methods <- classified_methods %>% 
    mutate(
      # A candidate for case_when:
      grouped = ifelse(test = method_status %in% 
                         c("Molybdosilicate Method",
                           "Heteropoly Blue Method",
                           "EPA Method 370.1",
                           "EPA Method 366.0",
                           "Automated Method for Molybdate-Reactive Silica",
                           "Flow Injection Analysis for Molybdate-Reactive Silicate") |
                         grepl("colorimetr|molybd|colorimeter",
                               analytical_method,
                               ignore.case = T),
                       yes = "Colorimetry",
                       no = ifelse(test = is.na(method_status) |
                                     method_status == "Ambiguous",
                                   yes = "Ambiguous",
                                   no = method_status)),
      grouped = ifelse(method_status %in% c("ICP-AES",
                                            "Just ICP",
                                            "ICP/MS"),
                       "ICP",
                       grouped),
      aquasat_fraction = case_when(
        fraction %in% c("Dissolved", "Filtered, lab", "Filterable") ~ "Dissolved",
        fraction %in% c("Total", "Total Recovrble", "Total Recoverable",
                        "Recoverable", "Unfiltered") ~ "Total",
        fraction %in% c("Fixed") ~ "Fixed",
        fraction %in% c("Non-Filterable (Particle)") ~ "Particle",
        is.na(fraction) | fraction %in% c(" ", "Field") ~ "Ambiguous"))
  
  rm(classified_methods, p_codes)
  gc()
  
  
  # Methods -----------------------------------------------------------------
  
  # Silica can be analyzed in myriad ways. However, many of these analytic
  # methods can be grouped together into just a few actual methodologies
  # that are realistic for silica:
  # -   Colorimetry: this represents samples that reference the molybdosilicate
  #       method (SM 4500 C), the heteropoly blue method (SM 4500 D), the automated
  #       method for molybdate-reactive silica (SM 4500 E), flow injection analysis for
  #       molybdate-reactive silica (SM 4500 F), gas segmented continuous flow
  #       colorimetric analysis (EPA 366.0), spectrophotometric detection of dissolved 
  #       silica (EPA 370.1), "colorimetry", or "molybdate".
  # -   ICP: this represents samples that reference ICP/MS (SM 3125,
  #       EPA Method 200.8, or "ICP MS"), ICP-AES (SM 3120 , EPA Methods 200.7
  #       and 6010, "ICP AES", or "ICP OES"), or just "ICP".
  # -   Direct Nitrous Oxide-Acetylene Flame Method: this represents samples
  #       that reference SM 3111 D , "Nitrous Oxide", or "FLAA"
  # -   Electrothermal Atomic Absorption Spectrometric Method: this represents
  #       samples that reference SM 3113, "GFAA", or "graphite furnace".
  # -   All others, which I'm calling **ambiguous**
  
  # Plot of record count by method type
  horiz_bar_rec_by_methods <- narrowed_methods %>%
    group_by(grouped) %>%
    summarize(count = n()) %>%
    arrange(count) %>%
    ggplot()+
    geom_col(aes(x = fct_reorder(grouped,count), y = count, fill = grouped)) +
    geom_text(aes(label = count, y = count / 2, x = fct_reorder(grouped, count), 
                  color = grouped),
              position = position_dodge(0.5),
              vjust = 0) +
    ylab("Count") +
    xlab("") +
    coord_flip() +
    theme_bw() +
    scale_fill_manual(values = c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#0072B2")) + 
    scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) + 
    scale_y_continuous(labels = comma)+
    theme(legend.position = "none",
          text = element_text(size = 20))
  
  
  # It is clear that colorimetric methods are the most common across grouped
  # methods. However, colorimetry encompasses several different types of analytic 
  # methods that are defined by either the EPA or SM; there are also a large
  # amount that do not clearly define which colorimetric method was used 
  # (i.e., ambiguous but some form of colorimetry):
  
  # Summary dataset for piechart
  col_pie_plot_data <- narrowed_methods %>%
    filter(grouped == "Colorimetry") %>%
    group_by(method_status) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(method_status = ifelse(is.na(method_status),
                                  "Ambiguous Colorimetry",
                                  method_status),
           method_status = factor(x = method_status,
                                  levels = method_status),
           prop = count / sum(count),
           ypos = cumsum(prop) - 0.5 * prop,
           legend = paste0(method_status, " (", percent(prop), ")"))
  
  colorimetry_pie_plot <- ggplot(data = col_pie_plot_data,
                                 aes(x = "", y = count, fill = legend)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9", "#CC79A7",
                                          "#0072B2", "#F0E442", "#D55E00")) +
                                            guides(fill = guide_legend(title = "Colorimetry Techniques")) +
    theme_void() + # remove background, grid, numeric label
    theme(text = element_text(size = 20))
  
  # The ICP method is the second most common method for silica analysis
  icp_pie_plot_data <- narrowed_methods %>%
    filter(grouped == "ICP") %>%
    group_by(method_status) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(method_status = factor(x = method_status, levels = method_status),
           prop = count / sum(count),
           ypos = cumsum(prop) - 0.5 * prop,
           legend = paste0(method_status, " (", percent(prop), ")"))
  
  icp_pie_plot <- ggplot(data = icp_pie_plot_data,
                         aes(x = "", y = count, fill = legend)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9", "#CC79A7",
                                          "#0072B2", "#F0E442", "#D55E00")) +
                                            guides(fill = guide_legend(title = "ICP Techniques")) +
    theme_void() + # remove background, grid, numeric label
    theme(text = element_text(size = 20))
  
  # The third most prevalent aggregated method encompasses samples whose 
  # methodologies were too vague to determine how they were analyzed. Below
  # is a table of all methodologies that we considered ambiguous:
  ambiguous_summary <- narrowed_methods %>%
    filter(grouped == "Ambiguous") %>%
    group_by(analytical_method) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) 
  
  
  # Sample Fractions --------------------------------------------------------
  
  # When looking at these different grouped methodologies coupled with their 
  # fractionation we find that most samples were analyzed for the dissolved
  # fraction of silica. In fact, only the heteropoly blue method (a colorimetric
  # technique) had more samples that analyzed the total fraction of silica.
  
  horiz_bar_rec_grouped_frac <- narrowed_methods %>%
    filter(grouped %in% c("ICP", "Colorimetry", "Ambiguous")) %>%
    mutate(method_status = ifelse(is.na(method_status),
                                  "Ambiguous", method_status)) %>%
    group_by(grouped, method_status, aquasat_fraction) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = method_status, y = count, fill = aquasat_fraction)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    theme_bw() +
    theme(text = element_text(size = 15)) +
    coord_flip() +
    ylab("Grouped Methodologies") +
    xlab("") +
    scale_fill_manual(values = c("#CC79A7", "#0072B2", "#F0E442", "#D55E00", "#009E73")) +
    guides(fill = guide_legend(title = "Sample Fraction")) +
    facet_wrap(~grouped) +
    scale_y_continuous(labels = comma)
  
  
  # How to tier silica ------------------------------------------------------
  
  # There is no clear way of tiering silica based on fraction if we want all
  # tiers to be looking at the exact same thing. Instead, I suggest we only look
  # at samples that are analyzing the dissolved fraction. With this in 
  # mind, I suggest tiering silica as follows:
  #   1.  **Restrictive.** ICP (all versions). Dissolved fraction. Only water
  # samples with an accepted value that is reasonable with logical units.
  #   2.  **Narrowed.** ICP and colorimetry (all versions). Dissolved fraction.
  #      Only water samples with an accepted value that is reasonable with logical units.
  #   3.  **Inclusive.** ICP and colorimetry (all versions), and all other
  #     *non-ambiguous* methods. Dissolved fraction. Only water samples with an
  #      accepted value that is reasonable with logical units. Currently no samples
  #      fall within this tier.
  #   4.  **Ambiguous (and therefore removed)**. Ambiguous methods. Dissolved 
  #      fraction. Only water samples with an accepted value that is reasonable with 
  #      logical units.
  
  # Clean data and assign flags
  
  # Flag and remove duplicates:
  no_duplicate_samples <- narrowed_methods %>%
    flag_duplicates(., duplicate_definition) %>%
    remove_duplicates(., duplicate_definition)
  
  # Remove samples that have duplicates and no values + no lab/result metadata
  silica_empties_removed <- no_duplicate_samples %>%
    flag_missing_results(., commenttext_missing) %>%
    filter(!flag_missing_result,
           status %in% c("Accepted", "Final", "Historical", "Validated"),
           !(is.na(value) &
               is.na(units) &
               is.na(lab_comments) &
               is.na(result_comments))) 
  
  silica_numeric_added <- silica_empties_removed %>%
    mutate(numeric_value = as.numeric(value)) #%>%
  # This would drop NDs:
  # filter(!is.na(numeric_value))
  
  # Make sure no numeric data is being lost (Necessary? I'm not sure if
  # there's an actual risk of this situation based on how R handles data types,
  # but I wanted to check for myself and am leaving it in for now):
  check_conversion <- silica_numeric_added %>%
    select(numeric_value, value) %>%
    mutate(numeric_check = if_else(
      # value entry containing numbers where numbers weren't kept?:
      condition = is.na(numeric_value) & grepl(x = value, pattern = "[0-9]"),
      true = "May contain numeric data",
      false = NA_character_)) %>%
    filter(!is.na(numeric_check))
  
  if(nrow(check_conversion) > 0){
    warning("Some numeric data may have been lost during numeric conversion of values!")
  }
  
  rm(check_conversion)
  gc()
  
  
  # Set up a lookup table so that final units are all in ug/L. 
  silica_lookup <- tibble(units = c("mg/L", "mg/l", "ppm", "ug/l", "ug/L",
                                    "mg/m3", "ppb", "mg/cm3", "ug/ml", "mg/ml",
                                    "ppt", "umol/L"),
                          conversion = c(1000, 1000, 1000, 1, 1, 1, 1,
                                         1000000, 1000, 1000000, 0.000001,
                                         60.080000))
  
  # Add unit conversion
  silica_harmonized <- silica_numeric_added %>%
    inner_join(x = .,
               y = silica_lookup,
               by = "units") %>%
    mutate(harmonized_value = (numeric_value * conversion) / 1000,
           harmonized_unit = "mg/L")
  
  # Tier based on methods group
  silica_tiered <- silica_harmonized %>%
    filter(aquasat_fraction == "Dissolved") %>%
    mutate(tiers = case_when(grouped == "ICP" ~ "Restrictive",
                             #grouped=="ICP/MS" ~ "Restrictive",
                             grouped == "Colorimetry" ~ "Narrowed",
                             grouped == "Direct Nitrous Oxide-Acetylene Flame Method" ~ "Inclusive",
                             grouped == "Ambiguous" ~ "Dropped from Aquasat"))
  
  # Histogram of records by silica tier
  silica_tier_hist <- ggplot(data = silica_tiered) +
    geom_histogram(aes(x = harmonized_value, fill = tiers), bins = 100) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    facet_wrap(~tiers, scales = "fixed") +
    xlab("Silica mg/L") +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 20))
  
  # MRB Notes:
  # - Any mdl cleaning needs to be re-added to this function once refined
  # - Need to incorporate fail checking, e.g. the grepl chains in the secchi script
  # - Check for and clean up redundancy between USGS filtering out of missing data
  #   and that which we've implemented
  
  return(
    list(
      horiz_bar_rec_by_methods = horiz_bar_rec_by_methods,
      colorimetry_pie_plot = colorimetry_pie_plot,
      icp_pie_plot = icp_pie_plot,
      ambiguous_summary = ambiguous_summary,
      horiz_bar_rec_grouped_frac = horiz_bar_rec_grouped_frac,
      silica_tiered = silica_tiered,
      silica_tier_hist = silica_tier_hist
    )
  )
  
}


harmonize_true_color <- function(raw_true_color, p_codes, match_table){
  
  # First step is to read in the data and make it workable, we'll then filter
  # the data to 1984 and beyond
  raw_true_color <- raw_true_color %>% 
    rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
                .cols = match_table$wqp_name) %>%
    left_join(p_codes, by = 'parm_cd') %>%
    mutate(year = year(date),
           units = trimws(units)) %>%
    filter(year >= 1984) %>%
    rowid_to_column(., "index")
  
  
  # Initial data cleaning Steps ---------------------------------------------
  
  # First off, let's focus our data set to "water" samples (as done in original
  # AquaSat). Then we can remove any samples from our data set that failed, or 
  # don't have enough lab metadata to make assumptions about the values presented.
  # (In the future, these `grepl` functions can be based on the word matrices we
  # just produced upstream!)
  
  true_color_no_data_samples <- raw_true_color %>%
    filter(is.na(value)&is.na(units)&is.na(lab_comments)&is.na(result_comments)) #identify samples that have no meaningful data related to an NA value
  
  true_color_fails_removed <- raw_true_color %>%
    filter(media == "Water",
           status %in% c('Accepted','Final','Historical','Validated'),
           # No failure-related field comments, slightly different list of words than
           # lab and result list (not including things that could be used to describe
           # field conditions like "warm", "ice", etc.)
           !grepl(pattern = paste0(c("fail", "suspect", "error", "beyond accept", "interference", 
                                     "questionable", "outside of accept", "problem", "contaminat", 
                                     "improper", "violation", "invalid", "unable", "no test", "cancelled", 
                                     "instrument down", "no result", "time exceed", "not accept", "QC EXCEEDED"),
                                   collapse = "|"),
                  x = field_comments,
                  ignore.case = T) | is.na(field_comments), 
           # No failure-related lab, should we remove controls comments
           !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                           "questionable", "outside of accept", "problem", "contaminat",
                           "improper", "warm", "violation", "invalid", "unable", "no test",
                           "cancelled", "instrument down", "no result", "time exceed",
                           "not accept", "QC EXCEEDED", "not ice", "ice melt", "PAST HOLDING TIME",
                           "beyond", "exceeded", "failed", "exceededs"),
                         collapse = "|"),
                  lab_comments,
                  ignore.case = T) | is.na(lab_comments),
           # No failure-related result comments
           !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                           "questionable", "outside of accept", "problem", "contaminat",
                           "improper", "warm", "violation", "invalid", "unable", "no test",
                           "cancelled", "instrument down", "no result", "time exceed",
                           "not accept", "QC EXCEEDED", "not ice", "ice melt",
                           "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
                         collapse = "|"),
                  result_comments,
                  ignore.case = T) | is.na(result_comments),
           # No failure-related values
           !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                           "questionable", "outside of accept", "problem", "contaminat",
                           "improper", "warm", "violation", "invalid", "unable", "no test",
                           "cancelled", "instrument down", "no result", "time exceed",
                           "not accept", "QC EXCEEDED", "not ice", "ice melt", "PAST HOLDING TIME"),
                         collapse = "|"),
                  value,
                  ignore.case = T) | is.na(value),
           !index %in% true_color_no_data_samples$index)
  
  # Message about types of data kept:
  print(paste('We kept',
              round(x = nrow(true_color_fails_removed) /
                      nrow(raw_true_color) * 100,
                    digits = 2),
              '% of samples, because the method used did not make sense. These methods are:'))
  
  rm(p_codes, true_color_no_data_samples) 
  gc()
  
  # Next we begin filtering the results to exclude qualitative results 
  # this is because there are various color specifications (such as green,
  # yellow, brown, etc.) that although give an estimation of apparent color do
  # not provide a relevant scale which is comparable to the other
  # apparent/ true color scales. 
  
  # Remove the qualitative results from the dataset
  true_color_fails_removed$value <- gsub(pattern = "[a-zA-Z]",
                                         replacement = "",
                                         x = true_color_fails_removed$value)
  
  # Create a dataframe from values to identify special characters and see if
  # they're worth keeping
  testsamples <- true_color_fails_removed %>% 
    count(value) 
  
  # No failure-related values
  true_color_vals_filtered <- true_color_fails_removed %>%
    filter(!grepl("*<|#|>| |-|,|/",
                  value,
                  ignore.case = F) | is.na(value))
  
  # Rerun to make sure special characters were removed, still having a problem
  # removing asterisk without removing valied values
  testsamples <- true_color_vals_filtered %>% 
    count(value)
  
  # The result of dropping all values that are qualitative and not quantitative is
  print(paste('We kept',
              round(nrow(true_color_vals_filtered) /
                      nrow(true_color_fails_removed) * 100,
                    2),
              '% of samples, because the values were qualitative and not quantitative.'))
  
  # Since we kept the majority of the data, exclusion of qualitative data should
  # not be impactful for the rest of the dataset.
  
  # There are three parameter classifications when there should be two
  # Apparent/True Color, the third "Color" classification needs to be further
  # explore so for now we'll split the data into three dataframes. 
  
  true_color_apparent <- true_color_vals_filtered %>% 
    filter(parameter == "Apparent color") 
  
  true_color_color <- true_color_vals_filtered %>% 
    filter(parameter == "Color") 
  
  true_color_true <- true_color_vals_filtered %>% 
    filter(parameter == "True color")
  
  parameter_count <- true_color_vals_filtered %>% 
    count(parameter) %>% 
    mutate(paramer_percentage = n / sum(n) * 100)
  
  # The good news is that the true color measurments are the significant portion
  # of the data, with apparent color also being the other significant variable
  # that we know how to deal with, while color is only a small fraction of the data
  parameter_barplot <- ggplot(true_color_vals_filtered) +
    geom_bar(aes(parameter, fill = parameter)) +
    theme_classic() +
    labs(x = "Parameter Variable",
         y = "Parameter Count")
  
  # Next we will analyze the analytical and sample methods to see how the samples
  # are being analyzed and processed
  apparent_methods <- true_color_apparent %>% 
    count(analytical_method)
  
  color_methods <- true_color_color %>% 
    count(analytical_method)
  
  true_color_methods <- true_color_true %>% 
    count(analytical_method)
  
  # First we will see how the unique analytical methods are being analyzed.
  # Unfortunately it seems like a little over a third of the data is being lost
  # due to no analytical method being present.
  true_color_unique_methods <- true_color_vals_filtered %>% 
    count(analytical_method)%>% 
    mutate(samp_perc = n / sum(n) * 100)
  
  true_color_sample_methods <- true_color_vals_filtered %>% 
    count(sample_method) %>% 
    mutate(samp_perc = n / sum(n) * 100)
  
  # Now we create a classification groups based on analytical methods to sort
  # samples into (visual, spectrophotogrametry, photogrametry)
  
  visual_methods <- tibble(group = c("visual"),
                           analytical_method = c("2120 B ~ Color in Water by Visual Comparison",
                                                 "Color - Visual Comparison Method",
                                                 "Color in Water by Visual Comparison",
                                                 "STANDARD METHODS 2120B COLOR BY VISUAL",
                                                 "2320 B ~ Alkalinity by Gran Titration"))
  
  specphoto_methods <- tibble(group = c("spectrophotometry"),
                              analytical_method = c("2120 C ~ Color in Water by Spectrophotometry",
                                                    "Color - Spectrophotometric Single Wavelength Method",
                                                    "Color by Spectrophotometric Analysis",
                                                    "Color in Water by Spectrophotometry Modified",
                                                    "Color in Water by Spectrophotometry"))
  
  photo_methods <- tibble(group = c("photometry"),
                          analytical_method = c("2120 E ~ Color in Water Using the ADMI Method",
                                                "Color by Calculating ADMI Values",
                                                "Color, wf, visual comparison",
                                                "Color by Calculating ADMI Values"))
  
  ambiguous_methods <- tibble(group = c("ambiguous"),
                              analytical_method = c("10-308-00-1 A ~ Determination of Color in Water",
                                                    "Analytical procedure not specified",
                                                    "Color",
                                                    "COLOR",
                                                    "Color - Pt/Co units",
                                                    "COLOR (PLATINUM-COBALT UNITS)",
                                                    "COLOR 345 C",
                                                    "COLOR 345 F .45",
                                                    "COLOR 440 F .22",
                                                    "Color Analysis Using Platinum/Cobalt",
                                                    "COLOR IN WATERS",
                                                    "Color, APHA Platinum-Cobalt",
                                                    "Color, True",
                                                    "DEP-SOP-NU-094",
                                                    "EPA110.2",
                                                    "Estero Bay Aquatic Preserve tributary sampling",
                                                    "Field measurement/observation, generic method",
                                                    "Field Office procedures",
                                                    "FIIR - NPHL",
                                                    "Laboratory Procedures for Water Quality Chemical Analysis",
                                                    "LAKE COUNTY QUALITY SYSTEMS MANUAL",
                                                    "Lake Trafford",
                                                    "LEGACY",
                                                    "Legacy STORET migration; analytical procedure not specified",
                                                    "Measurement of Water Color",
                                                    "N/A Calculation",
                                                    "Other of Unknown Procedure",
                                                    "Other or Unknown Procedure",
                                                    "SM182120B",
                                                    "SM2120C",
                                                    "Standard Methods for the Examination of Water and Wastewater",
                                                    "To be updated",
                                                    "USEPA Methods for Chemical Analysis or Water and Wastewater; EPA 600/4-79-020",
                                                    NA,
                                                    "Color (EPA)",
                                                    "Field - Color",
                                                    "ANALYSIS OF COLOR IN WATERS - MODIFIED LACHAT METHOD 10-308-00-1-A",
                                                    "Apparent color (EPA110.2 /DODEC)",
                                                    "Apparent color (StdMeth /DODEC)",
                                                    "Apparent Color, Hach Color Wheel Method",
                                                    "Color in the Field by Unknown Limnology Color Chart",
                                                    "Color, APHA Platinum-Cobalt",
                                                    "COLOR,APPARENT(UNFILTERED SAMPLE) PLAT-COB UNITS",
                                                    "DEP Field Analytical Procedures",
                                                    "General Listing of Field and Lab Analytical Procedures for Manatee County",
                                                    "HACH KIT",
                                                    "I1250",
                                                    "LAKE COUNTY QUALITY SYSTEMS MANUAL",
                                                    "LaMotte Smart 2 Colorimeter",
                                                    "LEGACY",
                                                    "Legacy STORET migration; analytical procedure not specified",
                                                    "Other of Unknown Procedure",
                                                    "QA Plan #900456"))
  
  # Combine methods lists
  methods_stacked <- rbind(visual_methods, specphoto_methods, photo_methods, ambiguous_methods)
  
  # Now join the new method classifications and we will drop the datat with no
  # analytical methods since we can not be sure how the data is being analyzed
  # to sort into the classifcations for inslusion in the final aquasat dataset.
  
  true_color_method_groups <- left_join(x = true_color_vals_filtered,
                                        y = methods_stacked,
                                        by = "analytical_method")
  
  # Check to make sure observations all have group vals
  true_color_goup_NA <- true_color_method_groups %>% 
    filter(is.na(group))
  
  # Drop the ambiguous methods since we want to keep high quality data, will have
  # to check to make sure that we do want to drop these vals
  true_color_ambiguous_methods_dropped <- true_color_method_groups %>% 
    filter(!group == "ambiguous")
  
  # Message about ambiguous records
  print(paste('We kept',
              round(nrow(true_color_ambiguous_methods_dropped) /
                      nrow(true_color_vals_filtered) * 100,
                    2),
              '% of samples, because the method could not be classified into an apparent/true color standard:'))
  
  rm(true_color_apparent, true_color_color, true_color_true)
  gc()
  
  # Now we rerun and filter out nonsensical methods like filtered methods for
  # apparent color that can't exist because apparent color is nonfiltered while
  # checking to make sure we dropped everything else
  apparent_methods_check <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "Apparent color") %>% 
    count(analytical_method) %>% 
    mutate(percent = n / sum(n) * 100)
  
  color_methods_check <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "Color") %>% 
    count(analytical_method)%>% 
    mutate(percent = n / sum(n) * 100)
  
  true_color_methods_check <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "True color") %>% 
    count(analytical_method) %>% 
    mutate(percent = n / sum(n) * 100)
  
  apparent_methods_plot <- ggplot(apparent_methods_check) +
    geom_col(aes(x = analytical_method, y = percent)) +
    theme_classic() +
    labs(x = "Analytical method",
         y = "Percent of data",
         title = "Apparent Color") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  color_methods_plot <- ggplot(color_methods_check) +
    geom_col(aes(x = analytical_method, y = percent)) +
    theme_classic() +
    labs(x = "Analytical method",
         y = "Percent of data",
         title = "Color") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  true_color_methods_plot <- ggplot(true_color_methods_check) +
    geom_col(aes(x = analytical_method, y = percent)) +
    theme_classic() +
    labs(x = "Analytical method",
         y = "Percent of data",
         title = "True Color") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  # The next step is to drop methods that can not occur with specific parameter
  # classification i.e apparent color can only be measured using visual methods
  # because all other standard require the sample to be filtered before measuring
  # and therefore making it apparent color. Luckily the Color parameter
  # classification comes with the USGS P-code so we can be sure that these are
  # actually True color measurements (00080,	Physical	Color, water, filtered,
  # platinum cobalt units	Agree, Dissolved, Color, PCU)
  
  # Should we change these Apparent Color to True Color or not since we cant be
  # 100% sure what they logged here?
  apparent_nonsensical_methods_dropped <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "Apparent color",
           !grepl(paste0(c("2120 C ~ Color in Water by Spectrophotometry",
                           "2120 E ~ Color in Water Using the ADMI Method",
                           "2320 B ~ Alkalinity by Gran Titration",
                           "Color - Spectrophotometric Single Wavelength Method",
                           "Color by Calculating ADMI Values",
                           "Color by Spectrophotometric Analysis", 
                           "Color in Water by Spectrophotometry", 
                           "Color in Water by Spectrophotometry Modified"), 
                         collapse = "|"),
                  analytical_method, ignore.case = T))
  
  apparent_nonsensical_methods <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "Apparent color") 
  
  color_nonsensical_methods_dropped <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "Color")
  
  truecolor_nonsensical_methods_dropped <- true_color_ambiguous_methods_dropped %>% 
    filter(parameter == "True color") 
  
  water_color_final_filter <- rbind(apparent_nonsensical_methods_dropped,
                                    color_nonsensical_methods_dropped,
                                    truecolor_nonsensical_methods_dropped)
  
  print(paste('We kept',
              round(nrow(apparent_nonsensical_methods_dropped) / 
                      nrow(apparent_nonsensical_methods) * 100, 
                    2),
              '% of samples, because the method could be classified into an apparent color standard that makes sense:'))
  
  # Lastly we filter by non-sensical units only keeping those that make sense,
  # which are Platinum Cobalt Units and ADMI for the 2120E standard
  
  water_color_units <- water_color_final_filter %>% 
    count(units)  
  # Looks like most of the units at this point are in PCU, so now we can drop
  # the rest of the units that dont make sense, we have to make another set of
  # tibbles because using grepl it will drop PCU units when we filter CU
  
  water_color_units_filetered <- water_color_final_filter %>% 
    filter(!grepl("AU|CU|mg/l|None|pt|units/cm|uS/cm", units, ignore.case = T)) 
  
  unlikely_unit_vals <- tibble(unit_type = c("drop"),
                               units = c("AU", "CU", "mg/l", "None", "pt",
                                         "units/cm", "uS/cm"))
  
  sense_unit_vals <- tibble(unit_type = c("keep"),
                            units = c("ADMI", "PCU"))
  
  
  units <- rbind(unlikely_unit_vals, sense_unit_vals)
  
  water_color_unit_drop <- left_join(x = water_color_final_filter,
                                     y = units,
                                     by = "units") %>% 
    filter(!unit_type == "drop")
  
  water_color_unit_check <- water_color_unit_drop %>% 
    count(units)
  
  print(paste(
    'We kept',
    round(nrow(water_color_unit_drop) /
            nrow(water_color_final_filter) * 100, 2),
    '% of samples, because the units made sense for Apparent/True Color measurements:'))
  
  # Last step check for numerical values
  vals_cleaned <- water_color_unit_drop %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))
  
  vals_cleaned_classification <- vals_cleaned %>% 
    group_by(parameter,group) %>% 
    count() %>% 
    mutate(percent = n / sum(n) * 100)
  
  print(paste('We kept',
              round(nrow(vals_cleaned) /
                      nrow(raw_true_color) * 100,
                    2),
              '% of samples after all the cleaning steps'))
  
  class_param_plot <- ggplot(vals_cleaned_classification)+
    geom_col(aes(x = group, y = n, fill = parameter)) +
    theme_classic() +
    labs(x = "Group Classification",
         y = "Observation Count") +
    scale_fill_discrete("Parameter")
  
  return(
    list(
      true_color_vals_filtered = true_color_vals_filtered,
      parameter_count = parameter_count,
      parameter_barplot = parameter_barplot,
      true_color_unique_methods = true_color_unique_methods,
      true_color_sample_methods = true_color_sample_methods,
      apparent_methods_plot = apparent_methods_plot,
      color_methods_plot = color_methods_plot,
      true_color_methods_plot = true_color_methods_plot,
      class_param_plot = class_param_plot
    )
  )
  
}


harmonize_tss <- function(raw_tss, p_codes, match_table){
  
  # Aggregating
  raw_tss <- raw_tss %>%
    rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
                .cols = match_table$wqp_name) %>%
    # Remove trailing white space in labels (Is this still necessary?)
    mutate(units = trimws(units)) %>%
    #Keep only samples that are water samples
    filter(media == "Water")
  
  # There are a lot of parameter codes so we are just going to use a grepl command
  # with key words that definitely disqualify the sample
  nonsensical_tss_methods <- raw_tss %>%
    filter(grepl(pattern = paste0(c("Oxygen", "Nitrogen", "Ammonia", "Metals", "E. coli", "Carbon",
                                    "Anion", "Cation", "Phosphorus", "Silica", "PH", "HARDNESS",
                                    "Nutrient", "Turbidity", "Temperature", "Nitrate", "Conductance",
                                    "Alkalinity", "Chlorophyll"),
                                  collapse = "|"),
                 x = analytical_method,
                 ignore.case = T))
  
  tss_filtered <- raw_tss %>%
    filter(!analytical_method %in% nonsensical_tss_methods$analytical_method)
  
  print(
    paste("We dropped",
          round(nrow(nonsensical_tss_methods) / nrow(raw_tss) * 100,2),
          '% of samples, because the method used did not make sense. These methods are:')
  )
  
  # Nice function for printing long vectors horizontally separated by dash
  p(unique(nonsensical_tss_methods$analytical_method),
    wrap = "",
    sep = " - ")
  
  # Define a depth lookup table to convert all depth data to meters. 
  depth_lookup <- tibble(sample_depth_unit = c("cm", "feet", "ft", "in", "m",
                                               "meters", "None"),
                         depth_conversion = c(1 / 100, .3048, .3048, 0.0254,
                                              1, 1, NA)) 
  
  # Join depth lookup table to tss data
  tss_depth <- inner_join(x = raw_tss,
                          y = depth_lookup,
                          by = c("sample_depth_unit")) %>%
    # Some depth measurements have negative values (assume that is just preference)
    # I also added .01 meters because many samples have depth of zero assuming
    # they were taken directly at the surface
    mutate(sample_depth = as.numeric(sample_depth),
           harmonized_depth = abs(sample_depth * depth_conversion) + .01)
  
  # We lose lots of data by keeping only data with depth measurements
  print(paste("If we only kept samples that had depth information we would lose",
              round((nrow(raw_tss) - nrow(tss_depth)) / nrow(raw_tss) * 100, 1),
              "% of samples"))
  
  tss_depth_hist <- ggplot(tss_depth, aes(x = harmonized_depth)) + 
    geom_histogram(bins = 100) + 
    scale_x_log10(limits = c(0.01, 10^3), breaks = c(.1, 1, 10, 100)) 
  
  
  # TSS Unit Harmonization --------------------------------------------------
  
  # TSS disharmony
  
  # TSS particle size fractionation
  
  # Select only units for %
  tss_perc <- raw_tss %>%
    filter(units == "%") 
  
  # Look at the breakdown of particle sizes
  tss_perc_summary <- tss_perc %>%
    group_by(particle_size) %>%
    summarize(count = n())
  
  # Keep only the sand fraction data (~50% of the data)
  sand.harmonized  <- tss_perc %>%
    filter(particle_size %in%  c("< 0.0625 mm", "sands")) %>%
    mutate(conversion = NA,
           harmonized_parameter = "p.sand",
           harmonized_value = value,
           harmonized_unit = "%")
  
  # TSS dropping bad units
  
  # Make a TSS lookup table
  tss_lookup <- tibble(units = c("mg/l", "g/l", "ug/l", "ppm"),
                       conversion = c(1, 1000, 1 / 1000, 1))
  
  # TSS harmony in mg/l
  # Join to the lookup table and harmonize units
  tss_tis_harmonized <- raw_tss %>%
    inner_join(tss_lookup, by = "units") %>%
    mutate(harmonized_parameter = "tss",
           harmonized_value = value * conversion,
           harmonized_unit = "mg/l") %>%
    # Change harmonized parameter to tis for parameter "fixed suspended solids"
    mutate(harmonized_parameter = ifelse(orig_parameter == "Fixed suspended solids", "tis", harmonized_parameter))
  
  # rm(tss, tss_depth, tss_filtered, tss_lookup, tss_p, nonsensical_tss_methods, depth_lookup)
  # gc()
  
  # TSS SSC empirical check
  ssc_tss <- tss_tis_harmonized %>%
    filter(orig_parameter %in% c("Total suspended solids",
                            "Suspended Sediment Concentration (SSC)")) %>%
    select(date, date_time, SiteID, parameter, orig_parameter, harmonized_value) %>%
    distinct(date_time, SiteID, .keep_all = T) %>%
    pivot_wider(names_from = "orig_parameter", values_from = "harmonized_value") %>% 
    rename(tss = `Total suspended solids`,
           ssc = `Suspended Sediment Concentration (SSC)`) 
  
  ssc_tss_summary <- ssc_tss %>%
    filter(!is.na(ssc)) %>%
    summary(.)
  
  return(
    list(
      tss_depth_hist = tss_depth_hist,
      tss_perc_summary = tss_perc_summary,
      raw_tss = raw_tss,
      tss_lookup = tss_lookup,
      ssc_tss_summary = ssc_tss_summary
    )
  )
  
}


harmonize_sdd <- function(raw_sdd, p_codes, match_table, sdd_method_matchup){
  
  # First step is to read in the data and make it workable, we'll then filter
  # the data to 1984 and beyond
  
  raw_sdd <- raw_sdd %>%
    rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
                .cols = match_table$wqp_name) %>%
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    # Remove trailing white space in labels (Is this still necessary?)
    mutate(year = year(date),
           units = trimws(units)) %>%
    filter(year >= 1984,
           media %in% c("Water", "water"),
           type %in% c("Surface Water", "Water", "Estuary", "Ocean Water",
                       "Mixing Zone") | is.na(type)) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of secchi analytical methods present: ",
      length(unique(raw_sdd$analytical_method))
    )
  )
  
  # Add a new column describing the methods groups
  grouped_sdd <- raw_sdd %>%
    left_join(x = .,
              y = sdd_method_matchup,
              by = c("analytical_method"))
  
  sdd_method_groups_plot <- grouped_sdd %>%
    count(method_grouping) %>%
    ggplot() +
    geom_bar(aes(x = method_grouping, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Method group") +
    ggtitle("SDD aggregation counts") +
    theme_bw()
  
  # Create a column to lump things that do/don't make sense for the fraction column
  grouped_sdd <- grouped_sdd %>%
    mutate(aquasat_fraction = if_else(
      condition = fraction %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
      true = "Makes sense",
      false = "Nonsensical"))
  
  sdd_fraction_groups_plot <- grouped_sdd %>%
    count(aquasat_fraction) %>%
    ggplot() +
    geom_bar(aes(x = aquasat_fraction, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Fraction group") +
    # ggtitle("SDD aggregation counts") +
    theme_bw()

  
  return(
    list(
      harmonized_sdd = grouped_sdd,
      sdd_method_groups_plot = sdd_method_groups_plot,
      sdd_fraction_groups_plot = sdd_fraction_groups_plot
    )
  )
}







