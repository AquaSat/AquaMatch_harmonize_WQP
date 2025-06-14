#' @title Create hex maps of record counts by data tier
#' 
#' @description
#' A function that creates hex maps of record counts for each tier of a selected
#' parameter's harmonized dataset. The maps are created as a single paneled ggplot
#' object and then exported as a single PNG file.
#' 
#' @param dataset A data frame of the harmonized parameter's dataset containing
#' the columns `tier`, `MonitoringLocationIdentifier`, `lat`, `lon`, and `datum`.
#' Intended to be the version of the dataset with simultaneous observations removed. 
#' @param map_crs The epsg code that should be used when creating the maps.
#' @param custom_width The desired output PNG width in inches.
#' @param custom_height The desired output PNG height in inches.
plot_tier_maps <- function(dataset, map_crs = 9311,
                           custom_width = 6.5, custom_height = 8){
  
  # Check for multiple parameter entries. Will facet if so
  if(length(unique(dataset$parameter)) > 1) {
    multiple <- TRUE
  } else {
    multiple <- FALSE
  }
  
  # Conterminous US sf object
  conterminous_us <- tigris::states(progress_bar = FALSE) %>%
    st_transform(crs = 9311) %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "American Samoa",
                         "Guam", "Puerto Rico",
                         "United States Virgin Islands",
                         "Commonwealth of the Northern Mariana Islands")))
  
  # Other US territories sf object
  non_conterminous_us <- tigris::states(progress_bar = FALSE) %>%
    st_transform(crs = 9311) %>%
    filter((NAME %in% c("Alaska", "Hawaii", "American Samoa",
                        "Guam", "Puerto Rico",
                        "United States Virgin Islands",
                        "Commonwealth of the Northern Mariana Islands")))
  
  
  # Datum varies throughout the dataset; build a conversion table.
  epsg_codes <- tribble(
    ~datum, ~epsg,
    # American Samoa Datum
    "AMSMA", 4169,
    # Midway Astro 1961
    "ASTRO", 37224,
    # Guam 1963
    "GUAM", 4675,
    # High Accuracy Reference Network for NAD83
    "HARN", 4957,
    # Johnston Island 1961 (Spelled Johnson in WQX)
    "JHNSN", 6725,
    # North American Datum 1927
    "NAD27", 4267,
    # North American Datum 1983
    "NAD83", 4269,
    # Old Hawaiian Datum
    "OLDHI", 4135,
    # Assume WGS84
    "OTHER", 4326,
    # Puerto Rico Datum
    "PR", 4139,
    # St. George Island Datum
    "SGEOR", 4138,
    # St. Lawrence Island Datum
    "SLAWR", 4136,
    # St. Paul Island Datum
    "SPAUL", 4137,
    # Assume WGS84
    "UNKWN", 4326,
    # Wake-Eniwetok 1960
    "WAKE", 37229,
    # World Geodetic System 1972
    "WGS72", 4322,
    # World Geodetic System 1984
    "WGS84", 4326
  )
  
  # Create a simplified sf object from the provided dataset
  recs_sf <- dataset %>%
    select(parameter, tier, MonitoringLocationIdentifier, lat, lon, datum) %>%
    left_join(
      x = .,
      y = epsg_codes,
      by = "datum"
    ) %>%
    # Group by CRS 
    split(f = .$epsg) %>%
    # Transform and re-stack
    map_df(.x = .,
           .f = ~ .x %>%
             st_as_sf(coords = c("lon", "lat"),
                      crs = unique(.x$epsg)) %>%
             st_make_valid() %>%
             st_transform(crs = map_crs)) %>%
    # More informative facet panel labels
    mutate(tier_label = case_when(
      tier == 0 ~ "Tier 0: Restrictive",
      tier == 1 ~ "Tier 1: Narrowed",
      tier == 2 ~ "Tier 2: Inclusive",
      tier == 3 ~ "Tier 3: Inclusive"
    ))
  
  # Focal records for the map
  trim_recs_sf <- recs_sf[conterminous_us, ]
  
  if(multiple){
    
    # Make the map
    map_plot <- sf_to_df(trim_recs_sf, fill = TRUE) %>%
      mutate(parameter = toupper(parameter)) %>%
      ggplot() +
      geom_hex(aes(x = x, y = y),
               bins = 60) +
      geom_sf(data = conterminous_us,
              color = "black",
              fill = NA) +
      scale_fill_viridis_c("Record count",
                           trans = "log",
                           breaks = breaks_log(n = 6),
                           labels = label_number(big.mark = ",")) +
      xlab(NULL) +
      ylab(NULL) +
      facet_grid(rows = vars(tier_label), cols = vars(parameter)) +
      guides(x = guide_axis(check.overlap = TRUE),
             y = guide_axis(check.overlap = TRUE)) +
      ggtitle(
        label = "Record counts across the US by tier and parameter",
        subtitle = paste0(
          "Not shown: ",
          comma(nrow(recs_sf[non_conterminous_us,])),
          " records from outside the conterminous US"
        )) +
      theme_bw()
    
  } else {
    
    # Make the map
    map_plot <- sf_to_df(trim_recs_sf, fill = TRUE) %>%
      ggplot() +
      geom_hex(aes(x = x, y = y),
               bins = 60) +
      geom_sf(data = conterminous_us,
              color = "black",
              fill = NA) +
      scale_fill_viridis_c("Record count",
                           trans = "log",
                           breaks = breaks_log(n = 6),
                           labels = label_number(big.mark = ",")) +
      xlab(NULL) +
      ylab(NULL) +
      facet_wrap(vars(tier_label), ncol = 1) +
      guides(x = guide_axis(check.overlap = TRUE),
             y = guide_axis(check.overlap = TRUE)) +
      ggtitle(
        label = paste0(toupper(unique(dataset$parameter)),
                       " record counts across the US by tier"),
        subtitle = paste0(
          "Not shown: ",
          comma(nrow(recs_sf[non_conterminous_us,])),
          " records from outside the conterminous US"
        )) +
      theme_bw()
    
  }
  
  
  # Export with autogenerated filename
  ggsave(filename = paste0("3_harmonize/out/",
                           paste0(unique(dataset$parameter), collapse = "_"),
                           "_tier_hex_map.png"),
         plot = map_plot, units = "in", device = "png",
         width = custom_width, height = custom_height)
}
