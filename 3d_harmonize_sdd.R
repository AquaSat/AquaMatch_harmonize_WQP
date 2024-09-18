# SDD specific targets list for the harmonization step

p3_sdd_targets_list <- list(
  
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
    name = p3_wqp_data_aoi_formatted_sdd,
    command = format_columns(p2_wqp_data_aoi_sdd),
    format = "feather"
  ),
  
  # Join in column containing site type info
  tar_target(
    name = p3_wqp_data_aoi_sitetype_sdd,
    command = left_join(
      x = p3_wqp_data_aoi_formatted_sdd,
      y = p1_wqp_inventory_aoi_sdd %>%
        select(OrganizationIdentifier, MonitoringLocationIdentifier,
               ResolvedMonitoringLocationTypeName, CharacteristicName,
               OrganizationFormalName, ProviderName, MonitoringLocationTypeName)
    )
  ),
  
  
  # Pre-harmonization -------------------------------------------------------
  
  # Time and time zone fills
  tar_target(
    name = p3_wqp_data_aoi_date_time_sdd,
    command = fill_date_time(dataset = p3_wqp_data_aoi_sitetype_sdd,
                             site_data = p2_site_counts_sdd),
    packages = c("tidyverse", "lutz", "sf", "sfheaders")
  ),
  
  tar_target(
    name = p3_wqp_data_aoi_ready_sdd,
    command = clean_wqp_data_sdd(wqp_data = p3_wqp_data_aoi_date_time_sdd,
                                 char_names_crosswalk = p1_char_names_crosswalk_sdd,
                                 site_data = p2_site_counts_sdd,
                                 wqp_metadata = p1_wqp_inventory_aoi_sdd),
    packages = c("tidyverse", "feather")
  ),
  
  # Connect cleaned data output to the pipeline
  tar_file_read(
    name = p3_cleaned_wqp_data_sdd,
    command = p3_wqp_data_aoi_ready_sdd$wqp_data_clean_path,
    read = read_feather(path = !!.x),
    packages = "feather"),
  
  
  # Harmonization -----------------------------------------------------------
  
  tar_target(
    name = p3_sdd_harmonized,
    command = harmonize_sdd(raw_sdd = p3_cleaned_wqp_data_sdd,
                            p_codes = p3_p_codes),
    packages = c("tidyverse", "feather", "ggrepel", "scales")
  ),
  
  tar_file_read(
    name = p3_sdd_tiering_record,
    command = p3_sdd_harmonized$sdd_tiering_record_path,
    read = read_csv(file = !!.x)
  ),
  
  # Harmonized SDD data containing grouping IDs for simultaneous
  # records, but not aggregated
  tar_file_read(
    name = p3_sdd_preagg_grouped,
    command = p3_sdd_harmonized$sdd_grouped_preagg_path,
    read = read_feather(path = !!.x),
    packages = "feather"),
  
  # Harmonized SDD data after simultaneous record aggregation (i.e.,
  # final product)
  tar_file_read(
    name = p3_sdd_agg_harmonized,
    command = p3_sdd_harmonized$sdd_harmonized_path,
    read = read_csv(file = !!.x)),
  
  # Create a copy of the csv in feather format
  tar_file_read(
    name = p3_sdd_agg_harmonized_feather,
    command = {
      out_path <- gsub(x = p3_sdd_harmonized$sdd_harmonized_path,
                       pattern = ".csv",
                       replacement = ".feather")
      
      write_feather(x = p3_sdd_agg_harmonized,
                    path = out_path)
      
      out_path
    },
    packages = c("targets", "feather"),
    read = read_feather(path = !!.x)
  ),
  
  # Export
  tar_target(
    name = p3_sdd_agg_harmonized_feather_drive_file,
    command = {
      p0_check_sdd_drive
      export_single_file(target = p3_sdd_agg_harmonized_feather,
                         drive_path = p0_sdd_output_path,
                         stable = p0_harmonization_config$sdd_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$sdd_stable_date)
    },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  
  # Site info ---------------------------------------------------------------
  
  # Generate site metadata after harmonization is complete
  tar_file_read(
    name = p3_sdd_harmonized_site_info,
    command = {
      # Pull and clean data
      sdd_sites <- get_site_info(dataset = p3_sdd_preagg_grouped)
      
      out_path <- "3_harmonize/out/sdd_harmonized_site_info.feather"
      
      sdd_sites %>%
        write_feather(path = out_path)
      
      out_path
    },
    read = read_feather(path = !!.x),
    packages = c("tidyverse", "dataRetrieval", "feather")
  ),
  
  # Export
  tar_target(
    name = p3_sdd_site_info_drive_file,
    command = {
      p0_check_sdd_drive
      export_single_file(target = p3_sdd_harmonized_site_info,
                         drive_path = p0_sdd_output_path,
                         stable = p0_harmonization_config$sdd_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$sdd_stable_date)
    },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  # Get file IDs ------------------------------------------------------------
  
  # In order to access "stable" versions of the dataset created by the pipeline,
  # we get their Google Drive file IDs and store those in the repo so that
  # the harmonization pipeline can retrieve them more easily. The targets below
  # will include all file IDs in the Drive location, not just stable ones
  
  # SDD
  tar_file_read(
    name = p3_sdd_drive_ids,
    command = get_file_ids(google_email = p0_harmonization_config$google_email,
                           drive_folder = p0_sdd_output_path,
                           file_path = "3_harmonize/out/sdd_drive_ids.csv",
                           depend = p3_sdd_site_info_drive_file
    ),
    read = read_csv(file = !!.x),
    packages = c("tidyverse", "googledrive")
  )
  
)

