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