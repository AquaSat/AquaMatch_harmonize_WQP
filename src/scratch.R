library(targets)
library(tidyverse)

library(lubridate)
library(scales)

# tar_load(raw_silica)
raw_silica <- tar_read(wqp_data_aoi_formatted_filtered) %>%
  filter(parameter == "silica")

tar_load(p_codes)
commenttext_missing = c('analysis lost', 'not analyzed', 
                        'not recorded', 'not collected', 
                        'no measurement taken')

duplicate_definition = c('org_id',
                         'SiteID',
                         'date', 
                         'time',
                         'orig_parameter', 
                         'fraction')


# harmonize_silica function contents --------------------------------------


harmonize_silica(raw_silica = raw_silica,
                 p_codes = p_codes,
                 commenttext_missing = commenttext_missing,
                 duplicate_definition = duplicate_definition)








