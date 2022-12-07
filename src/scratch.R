library(targets)
library(tidyverse)
# library(lubridate)

# tar_make(c(silica_update_usgs, true_color_update_usgs, tss_update_usgs, sdd_update_usgs))

# tar_load(p3_wqp_data_aoi_formatted)
# tar_load(p1_char_names_crosswalk)
# tar_load(p_codes)
# 
# raw_silica <- p3_wqp_data_aoi_formatted %>%
  # left_join(x = .,
  #           y = p1_char_names_crosswalk,
  #           by = c("CharacteristicName" = "char_name")) %>%
#   filter(parameter == "silica")
# 
# rm(p3_wqp_data_aoi_formatted)
# gc()



# 
# raw_tss <- tar_read(wqp_data_aoi_formatted_filtered) %>%
#   filter(parameter == "tss")
# 
# tar_load(p_codes)
targets::tar_read(p1_wqp_inventory) %>%
  left_join(x = .,
            y = tar_read(p1_char_names_crosswalk),
            by = c("CharacteristicName" = "char_name")) %>% 
  group_by(parameter) %>%
  summarize(n_records = sum(resultCount))


tar_read(p1_global_grid_aoi) %>%
  ggplot() +
  geom_sf(data = tar_read(us_shp)) +
  geom_sf(alpha = 0.35) +
  coord_sf(xlim = c(-180, -60)) +
  theme_bw()

tar_read(p1_global_grid_aoi) %>%
  ggplot() +
  geom_sf(data = tar_read(us_shp)) +
  geom_sf(alpha = 0.35) +
  coord_sf(xlim = c(150, 180),
           ylim = c(40, 60)) +
  theme_bw()

