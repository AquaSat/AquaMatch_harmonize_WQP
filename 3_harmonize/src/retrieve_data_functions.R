# A generalized function to retrieve a datset from Google Drive
retrieve_data <- function(link_table, folder_pattern){
  
  # Download the data from Google Drive and save to a location,
  # which is named based on the original filepath (folder_pattern) used when
  # exporting in the first pipeline
  download_path <- gsub(pattern = folder_pattern,
                        x = link_table$local_path,
                        replacement = "3_harmonize/in/")
  
  # Run the download
  drive_download(file = link_table$drive_link,
                 path = download_path,
                 overwrite = TRUE)
  
  # Read dataset into pipeline
  read_rds(download_path)
}

# A function to retrieve a specific parameter datset from Google Drive
retrieve_param_data <- function(link_table = p3_wqp_data_aoi_links,
                                parameter_string){
  
  # Isolate row with information on the specified parameter
  param_info <- link_table %>%
    filter(parameter == parameter_string)
  
  # Download the data from Google Drive and save to a location, which is named
  # based on the original filepath used when exporting in the first pipeline
  download_path <- gsub(pattern = "2_download/out/",
                        x = param_info$local_path,
                        replacement = "3_harmonize/in/")
  
  # Run the download
  drive_download(file = param_info$drive_link,
                 path = download_path,
                 overwrite = TRUE)
  
  # Read dataset into pipeline
  read_feather(download_path)
  
}