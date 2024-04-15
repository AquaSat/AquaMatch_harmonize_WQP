# Chlorophyll-a specific targets list for the harmonization step

p3_chla_targets_list <- list(
  
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
    name = p3_wqp_data_aoi_formatted_chl,
    command = format_columns(p2_wqp_data_aoi_chl),
    format = "feather"
  ),
  
  # Join in column containing site type info
  tar_target(
    name = p3_wqp_data_aoi_sitetype_chl,
    command = left_join(
      x = p3_wqp_data_aoi_formatted_chl,
      y = p1_wqp_inventory_aoi_chl %>%
        select(OrganizationIdentifier, MonitoringLocationIdentifier,
               ResolvedMonitoringLocationTypeName, CharacteristicName,
               OrganizationFormalName, ProviderName, MonitoringLocationTypeName)
    )
  ),
  
  
  # Pre-harmonization -------------------------------------------------------
  
  # Time and time zone fills
  tar_target(
    name = p3_wqp_data_aoi_date_time_chl,
    command = fill_date_time(dataset = p3_wqp_data_aoi_sitetype_chl,
                             site_data = p2_site_counts_chl),
    packages = c("tidyverse", "lutz", "sf", "sfheaders")
  ),
  
  tar_target(
    name = p3_wqp_data_aoi_ready_chl,
    command = clean_wqp_data(wqp_data = p3_wqp_data_aoi_date_time_chl,
                             char_names_crosswalk = p1_char_names_crosswalk_chl,
                             site_data = p2_site_counts_chl,
                             wqp_metadata = p1_wqp_inventory_aoi_chl),
    packages = c("tidyverse", "feather")
  ),
  
  # Connect cleaned data output to the pipeline
  tar_target(
    name = p3_cleaned_wqp_data_chl,
    command = read_feather(p3_wqp_data_aoi_ready_chl$wqp_data_clean_path),
    packages = "feather",
    format = "feather"
  ),
  
  
  # Harmonization -----------------------------------------------------------
  
  tar_target(
    name = p3_chla_harmonized,
    command = harmonize_chla(raw_chla = p3_cleaned_wqp_data_chl,
                             p_codes = p3_p_codes),
    packages = c("tidyverse", "feather", "ggrepel", "scales")
  ),
  
  tar_file_read(
    name = p3_chla_tiering_record,
    command = p3_chla_harmonized$chla_tiering_record_path,
    read = read_csv(file = !!.x)
  ),
  
  # Harmonized chlorophyll data containing grouping IDs for simultaneous
  # records, but not aggregated
  tar_file_read(
    name = p3_chla_preagg_grouped,
    command = p3_chla_harmonized$chla_grouped_preagg_path,
    read = read_feather(path = !!.x),
    cue = tar_cue("always"),
    packages = "feather"),
  
  # Harmonized chlorophyll data after simultaneous record aggregation (i.e.,
  # final product)
  tar_file_read(
    name = p3_chla_agg_harmonized,
    command = p3_chla_harmonized$chla_harmonized_path,
    read = read_csv(file = !!.x)),
  
  # Create a copy of the csv in feather format
  tar_file_read(
    name = p3_chla_agg_harmonized_feather,
    command = {
      out_path <- gsub(x = p3_chla_harmonized$chla_harmonized_path,
                       pattern = ".csv",
                       replacement = ".feather")
      
      write_feather(x = p3_chla_agg_harmonized,
                    path = out_path)
      
      out_path
    },
    packages = c("targets", "feather"),
    read = read_feather(path = !!.x)
  ),
  
  # Export
  tar_target(
    name = p3_chla_agg_harmonized_feather_drive_file,
    command = export_single_file(target = p3_chla_agg_harmonized_feather,
                                 drive_path = p0_chl_output_path,
                                 stable = p0_harmonization_config$chl_use_stable,
                                 google_email = p0_harmonization_config$google_email,
                                 date_stamp = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  
  # Site info ---------------------------------------------------------------
  
  # Generate site metadata after harmonization is complete
  tar_file_read(
    name = p3_chla_harmonized_site_info,
    command = {
      # Pull and clean data
      chla_sites <- get_site_info(dataset = p3_chla_preagg_grouped)
      
      out_path <- "3_harmonize/out/chla_harmonized_site_info.feather"
      
      chla_sites %>%
        write_feather(path = out_path)
      
      out_path
    },
    read = read_feather(path = !!.x),
    packages = c("tidyverse", "dataRetrieval", "feather")
  ),
  
  # Export
  tar_target(
    name = p3_chla_site_info_drive_file,
    command = export_single_file(target = p3_chla_harmonized_site_info,
                                 drive_path = p0_chl_output_path,
                                 stable = p0_harmonization_config$chl_use_stable,
                                 google_email = p0_harmonization_config$google_email,
                                 date_stamp = p0_harmonization_config$chl_stable_date),
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  )
  
)

