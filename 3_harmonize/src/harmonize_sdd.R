
harmonize_sdd <- function(raw_sdd, p_codes, match_table,
                          sdd_analytical_method_matchup,
                          sdd_sample_method_matchup,
                          sdd_equipment_matchup){
  
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
  
  # Identify samples that have no meaningful data
  sdd_no_data_samples <- raw_sdd %>%
    filter(is.na(value) & is.na(units) & is.na(lab_comments) & is.na(result_comments))
  
  # Remove fails and missing data
  sdd_fails_removed <- raw_sdd %>%
    filter(media == "Water",
           # Finalized data
           status %in% c('Accepted', 'Final', 'Historical', 'Validated'),
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
                 "PAST HOLDING TIME", "not done", "no reading", "no secchi",
                 "not reported", "no data"),
               collapse = "|"),
             x = value,
             ignore.case = T
           ) | is.na(value),
           # Remove samples that have no values and no lab/result metadata
           !index %in% sdd_no_data_samples$index)
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(raw_sdd) - nrow(sdd_fails_removed)
    )
  )
  
  # Now label rows that may have data in them still (i.e., some numeric and
  # some character data)
  sdd_values_flagged <- sdd_fails_removed %>%
    mutate(value_text_flag = if_else(
      condition = grepl(x = value,
                        # Adapted from https://stackoverflow.com/a/31761609
                        pattern = "^(?=.*?\\d)(?=.*?[a-zA-Z'\"<>])[a-zA-Z\\d \\.'\"<>]+$",
                        perl = TRUE),
      true = "May contain data",
      false = NA_character_
    )) 
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of secchi analytical methods present: ",
      length(unique(sdd_values_flagged$analytical_method))
    )
  )
  
  analytical_counts <- sdd_values_flagged %>%
    count(analytical_method) %>%
    arrange(desc(n))
  
  # Add a new column aggregating the analytical methods groups
  grouped_analytical_methods_sdd <- sdd_values_flagged %>%
    left_join(x = .,
              y = sdd_analytical_method_matchup,
              by = c("analytical_method"))
  
  # Plot methods grouping counts
  sdd_analytical_method_groups_plot <- grouped_analytical_methods_sdd %>%
    count(analytical_method_grouping) %>%
    ggplot() +
    geom_bar(aes(x = analytical_method_grouping, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Analytical method group") +
    ggtitle("SDD aggregation counts") +
    theme_bw()
  
  # Export as file instead of RDS to save memory
  ggsave(filename = "figs/sdd_analytical_method_groups_plot.png",
         plot = sdd_analytical_method_groups_plot,
         device = "png", width = 7.5, height = 4.5, units = "in")
  
  # Now count the fraction column (the ungrouped version): 
  fraction_counts <- raw_sdd %>%
    count(fraction) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the fraction column
  grouped_fractions_sdd <- grouped_analytical_methods_sdd %>%
    mutate(aquasat_fraction = if_else(
      condition = fraction %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
      true = "Makes sense",
      false = "Nonsensical"))
  
  sdd_fraction_groups_plot <- grouped_fractions_sdd %>%
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
  
  ggsave(filename = "figs/sdd_fraction_groups_plot.png",
         plot = sdd_fraction_groups_plot,
         device = "png", width = 8, height = 4, units = "in")
  
  # Now count the sample_method column: 
  sample_counts <- raw_sdd %>%
    count(sample_method) %>%
    arrange(desc(n))
  
  # Add a new column describing the sample_method group:
  grouped_sample_methods_sdd <- grouped_fractions_sdd %>%
    left_join(x = .,
              y = sdd_sample_method_matchup,
              by = c("sample_method"))
  
  sdd_sample_methods_groups_plot <- grouped_sample_methods_sdd %>%
    count(sample_method_grouping) %>%
    ggplot() +
    geom_bar(aes(x = sample_method_grouping, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Sample methods group") +
    theme_bw()
  
  ggsave(filename = "figs/sdd_sample_methods_groups_plot.png",
         plot = sdd_sample_methods_groups_plot,
         device = "png", width = 7.5, height = 4.5, units = "in")
  
  # Now count the collection_equipment column:
  equipment_counts <- raw_sdd %>%
    count(collection_equipment) %>%
    arrange(desc(n))
  
  group_equipment_sdd <- grouped_sample_methods_sdd %>%
    left_join(x = .,
              y = sdd_equipment_matchup,
              by = c("collection_equipment"))
  
  sdd_equipment_groups_plot <- group_equipment_sdd %>%
    count(equipment_grouping) %>%
    ggplot() +
    geom_bar(aes(x = equipment_grouping, y = n),
             fill = "white",
             color = "black",
             stat = "identity") +
    ylab("Record count") +
    xlab("Equipment group") +
    theme_bw()
  
  ggsave(filename = "figs/sdd_equipment_groups_plot.png",
         plot = sdd_equipment_groups_plot,
         device = "png", width = 8, height = 4, units = "in")
  
  # Now count the units column: 
  unit_counts <- raw_sdd %>%
    count(units) %>%
    arrange(desc(n))
  
  # Add a new column describing the units group:
  group_units_sdd <- group_equipment_sdd %>%
    mutate(unit_grouping = case_when(
      units %in% c("m", "ft", "cm", "in", "mm") ~ "standard",
      units %in% c("deg F", "NTU", "ft/sec", "mg") ~ "unlikely",
      is.na(units) | units %in% c("mi", "None") ~ "unclear"
    ))
  
  # Distribution of numeric "unclear" records vs "standard" and "unlikely"
  sdd_unit_dist_plot <- group_units_sdd %>%
    ggplot() +
    geom_histogram(aes(as.numeric(value),
                       fill = unit_grouping),
                   color = "black") +
    scale_y_log10() +
    theme_bw()
  
  ggsave(filename = "figs/sdd_unit_dist_plot.png",
         plot = sdd_unit_dist_plot,
         device = "png", width = 9.5, height = 4, units = "in")
  
  return(
    list(
      sdd_analytical_method_groups_plot = "figs/sdd_analytical_method_groups_plot.png",
      sdd_fraction_groups_plot = "figs/sdd_fraction_groups_plot.png",
      # sdd_equipment_groups_plot = sdd_equipment_groups_plot,
      # sdd_sample_methods_groups_plot = sdd_sample_methods_groups_plot,
      # sdd_unit_dist_plot = sdd_unit_dist_plot,
      analytical_counts = analytical_counts,
      fraction_counts = fraction_counts,
      sample_counts = sample_counts,
      unit_counts = unit_counts,
      equipment_counts = equipment_counts
    )
  )
}