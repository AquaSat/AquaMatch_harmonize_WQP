# DOC specific targets list for the harmonization step

p3_doc_targets_list <- list(
  
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
    name = p3_wqp_data_aoi_formatted_doc,
    command = format_columns(p2_wqp_data_aoi_doc),
    format = "feather"
  ),
  
  # Join in column containing site type info
  tar_target(
    name = p3_wqp_data_aoi_sitetype_doc,
    command = left_join(
      x = p3_wqp_data_aoi_formatted_doc,
      y = p1_wqp_inventory_aoi_doc %>%
        select(OrganizationIdentifier, MonitoringLocationIdentifier,
               ResolvedMonitoringLocationTypeName, CharacteristicName,
               OrganizationFormalName, ProviderName,
               MonitoringLocationTypeName)
    )
  ),
  
  
  # Pre-harmonization -------------------------------------------------------
  
  # Time and time zone fills
  tar_target(
    name = p3_wqp_data_aoi_date_time_doc,
    command = fill_date_time(dataset = p3_wqp_data_aoi_sitetype_doc,
                             site_data = p2_site_counts_doc),
    packages = c("tidyverse", "lutz", "sf", "sfheaders")
  ),
  
  tar_target(
    name = p3_wqp_data_aoi_ready_doc,
    command = clean_wqp_data(wqp_data = p3_wqp_data_aoi_date_time_doc,
                             char_names_crosswalk = p1_char_names_crosswalk_doc,
                             # Convert list of sites by param to single df
                             site_data = p2_site_counts_doc,
                             wqp_metadata = p1_wqp_inventory_aoi_doc),
    packages = c("tidyverse", "feather")
  ),
  
  tar_target(
    name = p3_cleaned_wqp_data_doc,
    command = read_feather(p3_wqp_data_aoi_ready_doc$wqp_data_clean_path),
    packages = "feather",
    format = "feather"
  ),
  
  
  # Harmonization -----------------------------------------------------------
  
  tar_target(
    name = p3_doc_harmonized,
    command = harmonize_doc(raw_doc = p3_cleaned_wqp_data_doc,
                            p_codes = p3_p_codes),
    packages = c("tidyverse", "feather", "ggrepel", "scales", "snakecase")
  ),
  
  tar_file_read(
    name = p3_doc_tiering_record,
    command = p3_doc_harmonized$doc_tiering_record_path,
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p3_doc_preagg_grouped,
    command = p3_doc_harmonized$doc_grouped_preagg_path,
    read = read_feather(path = !!.x),
    packages = "feather"),
  
  tar_file_read(
    name = p3_doc_agg_harmonized,
    command = p3_doc_harmonized$doc_harmonized_path,
    read = read_csv(file = !!.x)
  ),
  
  tar_file_read(
    name = p3_doc_agg_harmonized_feather,
    command = {
      out_path <- gsub(x = p3_doc_harmonized$doc_harmonized_path,
                       pattern = ".csv",
                       replacement = ".feather")
      
      write_feather(x = read_csv(file = p3_doc_harmonized$doc_harmonized_path),
                    path = out_path)
      
      out_path
    },
    packages = c("targets", "feather"),
    read = read_feather(path = !!.x)
  ),
  
  # Export
  tar_target(
    name = p3_doc_agg_harmonized_feather_drive_file,
    command = {
      p0_check_doc_drive
      export_single_file(target = p3_doc_agg_harmonized_feather,
                         drive_path = p0_doc_output_path,
                         stable = p0_harmonization_config$doc_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$doc_stable_date)
      },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  ),
  
  
  # Site info ---------------------------------------------------------------
  
  # Generate site metadata after harmonization is complete
  tar_file_read(
    name = p3_doc_harmonized_site_info,
    command = {
      # Pull and clean data
      doc_sites <- get_site_info(dataset = p3_doc_preagg_grouped)
      
      out_path <- "3_harmonize/out/doc_harmonized_site_info.feather"
      
      doc_sites %>%
        write_feather(path = out_path)
      
      out_path
    },
    read = read_feather(path = !!.x),
    packages = c("tidyverse", "dataRetrieval", "feather")
  ),
  
  # Export
  tar_target(
    name = p3_doc_site_info_drive_file,
    command = {
      p0_check_doc_drive
      export_single_file(target = p3_doc_harmonized_site_info,
                         drive_path = p0_doc_output_path,
                         stable = p0_harmonization_config$doc_use_stable,
                         google_email = p0_harmonization_config$google_email,
                         date_stamp = p0_harmonization_config$doc_stable_date)
      },
    packages = c("tidyverse", "googledrive"),
    error = "stop"
  )
  
)

