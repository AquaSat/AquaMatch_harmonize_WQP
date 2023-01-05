library(targets)
library(tidyverse)
library(pander)

raw_tss <- tar_read(wqp_data_aoi_formatted_filtered) %>%
  filter(parameter == "tss")
p_codes <- tar_load(p_codes)
match_table <- tar_read(wqp_col_match)


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
         value_numeric = as.numeric(value),
         harmonized_value = value_numeric * conversion,
         harmonized_unit = "mg/l") %>%
  # Change harmonized parameter to tis for parameter "fixed suspended solids"
  mutate(harmonized_parameter = ifelse(orig_parameter == "Fixed suspended solids",
                                       "tis",
                                       harmonized_parameter))

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


