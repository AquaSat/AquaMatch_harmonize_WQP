#' @title Create pie charts for grepl string detections
#' 
#' @description
#' A function to streamline creation of pie charts for showing failure-related
#' grepl string detection counts
#' 
#' @param dataset A data frame containing the columns `record_count` (numeric
#'  count of detections) and `word` (string containing the pattern used in grepl)
#' @param col_name A string containing the name of the WQP column being checked
#' for fail-related language 
#' 
#' @returns A ggplot object containing a pie chart
#' 
#' @note Adapted from https://stackoverflow.com/questions/69715282/how-to-adjust-ggrepel-label-on-pie-chart
plot_fail_pie <- function(dataset, col_name, text_size = 3){
  
  # Prepare the position info needed for the pie chart
  pie_prep <- dataset %>% 
    # Don't clutter with absent searches
    filter(record_count != 0) %>%
    mutate(perc = record_count / sum(record_count),
           labels = scales::percent(perc)) %>% 
    # Descending order of frequency
    arrange(desc(perc)) %>%
    mutate(word = fct_rev(fct_inorder(word)),
           # Text label locations
           text_y = cumsum(record_count) - record_count / 2)
  
  pie_prep %>%
    ggplot(aes(x = "", y = record_count, fill = word)) + 
    geom_col(color = "black", linewidth = 0.35) +
    # Pie chart format
    coord_polar(theta = "y") +
    # Label with grepl text and record count
    geom_label_repel(
      aes(x = 1.4,
          label = paste0(word, "\n n = ", record_count),
          y = text_y), 
      nudge_x = 0.3,
      nudge_y = 0.6,
      size = text_size,
      max.overlaps = 25,
      show.legend = F) +
    # Avoid dark colors that would prevent legibility
    scale_fill_viridis_d(begin = 0.2) +
    ggtitle(paste0("Fail-related language in ", col_name, " column"),
            subtitle = "Detections are not mutually exclusive") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
}
