# True color specific targets list for the harmonization step

p3_tc_targets_list <- list(
  
  # Pre-harmonization data prep ---------------------------------------------
  
  # Cleaning steps: 
  # Remove duplicates, ensure meaningful results present, check data status,
  # check media, remove white spaces
  
  # All columns in p3_wqp_data_aoi_* are of class character. Coerce select 
  # columns back to numeric, but first retain original entries in new columns
  # ending in "_original". The default option is to format "ResultMeasureValue"
  # and "DetectionQuantitationLimitMeasure.MeasureValue" to numeric, but 
  # additional variables can be added using the `vars_to_numeric` argument in 
  # format_columns(). By default, format_columns() will retain all columns, but
  # undesired variables can also be dropped from the WQP dataset using the 
  # optional `drop_vars` argument. 
  tar_target(
    name = p3_wqp_data_aoi_formatted_tc,
    command = format_columns(p2_wqp_data_aoi_tc),
    format = "feather"
  ),
  
  # Join in column containing site type info
  tar_target(
    name = p3_wqp_data_aoi_sitetype_tc,
    command = left_join(
      x = p3_wqp_data_aoi_formatted_tc,
      y = p1_wqp_inventory_aoi_tc %>%
        select(OrganizationIdentifier, MonitoringLocationIdentifier,
               ResolvedMonitoringLocationTypeName, CharacteristicName,
               OrganizationFormalName, ProviderName, MonitoringLocationTypeName)
    )
  ),
  
  
  # Pre-harmonization -------------------------------------------------------
  
  # Time and time zone fills
  tar_target(
    name = p3_wqp_data_aoi_date_time_tc,
    command = fill_date_time(dataset = p3_wqp_data_aoi_sitetype_tc,
                             site_data = p2_site_counts_tc),
    packages = c("tidyverse", "lutz", "sf", "sfheaders")
  ),
  
  tar_target(
    name = p3_wqp_data_aoi_ready_tc,
    command = clean_wqp_data(wqp_data = p3_wqp_data_aoi_date_time_tc,
                             char_names_crosswalk = p1_char_names_crosswalk_tc,
                             site_data = p2_site_counts_tc,
                             wqp_metadata = p1_wqp_inventory_aoi_tc),
    packages = c("tidyverse", "feather")
  ),
  
  # Connect cleaned data output to the pipeline
  tar_file_read(
    name = p3_cleaned_wqp_data_tc,
    command = p3_wqp_data_aoi_ready_tc$wqp_data_clean_path,
    read = read_feather(path = !!.x),
    cue = tar_cue("always"),
    packages = "feather")#,
  
  
  # Harmonization -----------------------------------------------------------
  
)