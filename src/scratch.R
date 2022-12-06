library(targets)
library(tidyverse)
# library(lubridate)

tar_make(c(silica_update_usgs, true_color_update_usgs, tss_update_usgs, sdd_update_usgs))

# tar_load(p3_wqp_data_aoi_formatted)
# tar_load(p1_char_names_crosswalk)
# tar_load(p_codes)
# 
# raw_silica <- p3_wqp_data_aoi_formatted %>%
#   left_join(x = .,
#             y = p1_char_names_crosswalk,
#             by = c("CharacteristicName" = "char_name")) %>%
#   filter(parameter == "silica")
# 
# rm(p3_wqp_data_aoi_formatted)
# gc()



# 
# raw_tss <- tar_read(wqp_data_aoi_formatted_filtered) %>%
#   filter(parameter == "tss")
# 
# tar_load(p_codes)
