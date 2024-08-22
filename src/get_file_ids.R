#' @title Function to retrieve Google Drive IDs for a specified folder.
#' 
#' @description
#' A function that retrieves the Google Drive IDs for a folder and exports them
#' locally as a csv.
#' 
#' @param google_email A string containing the gmail address to use for
#' Google Drive authentication.
#' 
#' @param drive_folder A string specifying the Google Drive folder location of
#' interest.
#' 
#' @param file_path The output destination (incl. filename) for the table of
#' file info.
#' 
#' @param depend The (non-string) name of a target that should be run before this,
#' e.g. to ensure that Drive uploads from earlier in this workflow are considered.
#'
#' @return A dribble (Google Drive tibble) containing names and IDs of files
#' in the specified folder.
#'
get_file_ids <- function(google_email, drive_folder, file_path, depend = NULL){
  
  # Authorize using the google email provided
  drive_auth(google_email)
  
  # Get info and safe as a csv locally
  drive_ls(path = drive_folder, recursive = TRUE) %>%
    select(name, id) %>%
    write_csv(file = file_path)
  
  # Return path for tracking
  file_path
  
}