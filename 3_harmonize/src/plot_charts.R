# A function that plots bar charts of record counts by year, month, and day of week

#' @title Create bar charts of record counts by data tier
#' 
#' @description
#' A function that creates bar charts of record counts for years, months, and days 
#' of each tier of a selected parameter's harmonized dataset. The charts are
#' created as a single paneled ggplot object and then exported as a single PNG file.
#' 
#' @param dataset A data frame of the harmonized parameter's dataset containing
#' the columns `tier`, `MonitoringLocationIdentifier`, `lat`, `lon`, and `datum`.
#' Intended to be the version of the dataset with simultaneous observations removed. 
#' @param custom_width The desired output PNG width in inches.
#' @param custom_height The desired output PNG height in inches.
#' @param scale_type String for scales argument of facet_wrap, if applicable.:

plot_time_charts <- function(dataset, #parameter,
                             custom_width = 6, custom_height = 4,
                             scale_type = "fixed"){
  
  unique_params <- unique(dataset$parameter)
  
  tier_levels <- sort(unique(dataset$tier))
  
  # Grab year data and plot record counts
  year_plot <- dataset %>%
    select(parameter, harmonized_local_time, tier) %>%
    mutate(year = year(harmonized_local_time),
           tier = factor(x = tier, levels = tier_levels, ordered = TRUE)) %>%
    count(parameter, year, tier) %>%
    ggplot() +
    geom_bar(aes(x = year, y = n, fill = tier),
             color = "gray20", stat = "identity") +
    ylab("Record count") +
    xlab("Activity year") +
    ggtitle(
      paste0("Record distribution by year & tier: ",
             paste0(unique_params, collapse = ", "))
      ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_fill_viridis_d("Tier", direction = -1) +
    theme_bw()
  
  # If only a single value for parameter column...
  if(length(unique_params) > 1){
    year_plot <- year_plot + facet_wrap(vars(parameter), scales = scale_type)
  }
  
  # Export with autogenerated filename
  ggsave(filename = paste0("3_harmonize/out/",
                           paste0(unique_params, collapse = "_"), "_",
                           "year_tier_chart.png"),
         plot = year_plot, units = "in", device = "png",
         width = custom_width, height = custom_height)
  
  # Day of week
  day_plot <- dataset %>%
    select(parameter, harmonized_local_time, tier) %>%
    mutate(weekday = wday(harmonized_local_time, label = TRUE),
           tier = factor(x = tier, levels = tier_levels, ordered = TRUE)) %>%
    count(parameter, weekday, tier) %>%
    ggplot() +
    geom_bar(aes(x = weekday, y = n, fill = tier),
             color = "gray20", stat = "identity") +
    ylab("Record count") +
    xlab("Activity day") +
    ggtitle(
      paste0("Record distribution by day of week & tier: ",
             paste0(unique_params, collapse = ", "))
      ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_fill_viridis_d("Tier", direction = -1) +
    theme_bw()
  
  # If only a single value for parameter column...
  if(length(unique_params) > 1){
    day_plot <- day_plot + facet_wrap(vars(parameter), scales = scale_type)
  }
  
  ggsave(filename = paste0("3_harmonize/out/",
                           paste0(unique_params, collapse = "_"), "_",
                           "wday_tier_chart.png"),
         plot = day_plot, units = "in", device = "png",
         width = custom_width, height = custom_height)
  
  # Month
  month_plot <- dataset %>%
    select(parameter, harmonized_local_time, tier) %>%
    mutate(month = month(harmonized_local_time, label = TRUE),
           tier = factor(x = tier, levels = tier_levels, ordered = TRUE)) %>%
    count(parameter, month, tier) %>%
    ggplot() +
    geom_bar(aes(x = month, y = n, fill = tier),
             color = "gray20", stat = "identity") +
    ylab("Record count") +
    xlab("Activity month") +
    ggtitle(
      paste0("Record distribution by month & tier: ",
             paste0(unique_params, collapse = ", "))
      ) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_fill_viridis_d("Tier", direction = -1) +
    theme_bw()
  
  # If only a single value for parameter column...
  if(length(unique_params) > 1){
    month_plot <- month_plot + facet_wrap(vars(parameter), scales = scale_type)
  }
  
  ggsave(filename = paste0("3_harmonize/out/",
                           paste0(unique_params, collapse = "_"), "_",
                           "month_tier_chart.png"),
         plot = month_plot, units = "in", device = "png",
         width = custom_width, height = custom_height)
  
  
  
  
}
