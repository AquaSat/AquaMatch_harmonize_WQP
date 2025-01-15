# TSS specific targets list for the harmonization step

p3_tss_targets_list <- list(
  
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
    name = p3_wqp_data_aoi_formatted_tss,
    command = format_columns(p2_wqp_data_aoi_tss),
    format = "feather"
  ),
  
  # Join in column containing site type info
  tar_target(
    name = p3_wqp_data_aoi_sitetype_tss,
    command = left_join(
      x = p3_wqp_data_aoi_formatted_tss,
      y = p1_wqp_inventory_aoi_tss %>%
        select(OrganizationIdentifier, MonitoringLocationIdentifier,
               ResolvedMonitoringLocationTypeName, CharacteristicName,
               OrganizationFormalName, ProviderName, MonitoringLocationTypeName)
    )
  ),
  
  
  # Pre-harmonization -------------------------------------------------------
  
  # Time and time zone fills
  tar_target(
    name = p3_wqp_data_aoi_date_time_tss,
    command = fill_date_time(dataset = p3_wqp_data_aoi_sitetype_tss,
                             site_data = p2_site_counts_tss),
    packages = c("tidyverse", "lutz", "sf", "sfheaders")
  ),
  
  tar_target(
    name = p3_wqp_data_aoi_ready_tss,
    command = clean_wqp_data(wqp_data = p3_wqp_data_aoi_date_time_tss,
                             char_names_crosswalk = p1_char_names_crosswalk_tss,
                             site_data = p2_site_counts_tss,
                             wqp_metadata = p1_wqp_inventory_aoi_tss),
    packages = c("tidyverse", "feather")
  ),
  
  # Connect cleaned data output to the pipeline
  tar_file_read(
    name = p3_cleaned_wqp_data_tss,
    command = p3_wqp_data_aoi_ready_tss$wqp_data_clean_path,
    read = read_feather(path = !!.x),
    packages = "feather"),
  
  
  # Harmonization -----------------------------------------------------------
  
  tar_target(
    name = p3_tss_harmonized,
    command = harmonize_tss(raw_tss = p3_cleaned_wqp_data_tss,
                             p_codes = p3_p_codes),
    packages = c("tidyverse", "feather", "ggrepel", "scales", "snakecase",
                 "sf", "sfheaders")
  ),
  
  tar_file_read(
    name = p3_tss_tiering_record,
    command = p3_tss_harmonized$tss_tiering_record_path,
    read = read_csv(file = !!.x)
  ),
  
  # Harmonized TSS data containing grouping IDs for simultaneous
  # records, but not aggregated
  tar_file_read(
    name = p3_tss_preagg_grouped,
    command = p3_tss_harmonized$tss_grouped_preagg_path,
    read = read_feather(path = !!.x),
    packages = "feather"),
  
  # Harmonized TSS data after simultaneous record aggregation (i.e.,
  # final product)
  tar_file_read(
    name = p3_tss_agg_harmonized,
    command = p3_tss_harmonized$tss_harmonized_path,
    read = read_csv(file = !!.x)),
  
  # Create a copy of the csv in feather format
  tar_file_read(
    name = p3_tss_agg_harmonized_feather,
    command = {
      out_path <- gsub(x = p3_tss_harmonized$tss_harmonized_path,
                       pattern = ".csv",
                       replacement = ".feather")
      
      write_feather(x = p3_tss_agg_harmonized,
                    path = out_path)
      
      out_path
    },
    packages = c("targets", "feather"),
    read = read_feather(path = !!.x)
  ),
  
  # Export
  tar_target(
    name = p3_tss_agg_harmonized_feather_drive_file,
    command = {
      p0_check_tss_drive
      export_single_file(target = p3_tss_agg_harmonized_feather,
                         drive_path = p0_tss_output_path,
                         stable = p0_harmonization_config$tss_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$tss_stable_date)
    },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  
  # Site info ---------------------------------------------------------------
  
  # Generate site metadata after harmonization is complete
  tar_file_read(
    name = p3_tss_harmonized_site_info,
    command = {
      # Pull and clean data
      tss_sites <- get_site_info(dataset = p3_tss_preagg_grouped)
      
      out_path <- "3_harmonize/out/tss_harmonized_site_info.feather"
      
      tss_sites %>%
        write_feather(path = out_path)
      
      out_path
    },
    read = read_feather(path = !!.x),
    packages = c("tidyverse", "dataRetrieval", "feather")
  ),
  
  # Export
  tar_target(
    name = p3_tss_site_info_drive_file,
    command = {
      p0_check_tss_drive
      export_single_file(target = p3_tss_harmonized_site_info,
                         drive_path = p0_tss_output_path,
                         stable = p0_harmonization_config$tss_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$tss_stable_date)
    },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  # Get file IDs ------------------------------------------------------------
  
  # In order to access "stable" versions of the dataset created by the pipeline,
  # we get their Google Drive file IDs and store those in the repo so that
  # the harmonization pipeline can retrieve them more easily. The targets below
  # will include all file IDs in the Drive location, not just stable ones
  
  tar_file_read(
    name = p3_tss_drive_ids,
    command = get_file_ids(google_email = p0_harmonization_config$google_email,
                           drive_folder = p0_tss_output_path,
                           file_path = "3_harmonize/out/tss_drive_ids.csv",
                           depend = p3_tss_site_info_drive_file
    ),
    read = read_csv(file = !!.x),
    packages = c("tidyverse", "googledrive")
  )
  
)

