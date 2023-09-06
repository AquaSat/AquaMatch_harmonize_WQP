# A function to assign field tiering to samples from the Water Quality Portal based
# on the sample filtering practices and the inclusion of depth data in the
# dataset.

# dataset: data frame of Water Quality Portal data containing the fields
# ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode,
# ResultSampleFractionText.

# Returns: a data frame with the additional columns `filter_flag`, `depth_flag`,
# and `field_tier`.

assign_field_tier <- function(dataset){
  
  dataset %>%
    mutate(
      # Check if it's been filtered
      # Q: Should this include Filterable or Supernate?
      filter_flag = if_else(
        ResultSampleFractionText %in% c("Filtered, lab", "Filtered, field"),
        true = "filtered",
        false = "unfiltered or unknown"),
      # Check if it has complete depth data
      depth_flag = if_else(
        !is.na(ActivityDepthHeightMeasure.MeasureValue) &
          !is.na(ActivityDepthHeightMeasure.MeasureUnitCode),
        true = "complete depth",
        false = "incomplete or missing depth"),
      # Assign tier based on both flags
      field_tier = case_when(
        # Filtered + complete depth = restrictive
        filter_flag == "filtered" & depth_flag == "complete depth" ~ "restrictive",
        # Partial in some way = narrowed
        ( filter_flag == "filtered" & depth_flag == "incomplete or missing depth" ) |
          ( filter_flag == "unfiltered or unknown" & depth_flag == "complete depth" ) ~ "narrowed",
        # Neither = inclusive
        filter_flag == "unfiltered or unknown" & depth_flag == "incomplete or missing depth" ~ "inclusive",
        TRUE ~ "other"
      )
    )
  
}
