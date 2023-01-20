# From USGS WQP pipeline:

#' @title Flag missing results
#' 
#' @description 
#' Function to flag true missing results, i.e. when the result measure value 
#' and detection limit value are both NA, when "not reported" is found in the
#' column "ResultDetectionConditionText", or when any of the strings from
#' `commenttext_missing` are found in the column "ResultCommentText".
#' 
#' @param wqp_data data frame containing the data downloaded from the WQP, 
#' where each row represents a data record. Must contain the columns
#' "DetectionQuantitationLimitMeasure.MeasureValue", "ResultMeasureValue", 
#' "ResultDetectionConditionText", and "ResultCommentText".
#' @param commenttext_missing character string(s) indicating which strings from
#' the WQP column "ResultCommentText" correspond with missing result values.
#' 
#' @returns
#' Returns a data frame containing data downloaded from the Water Quality Portal,
#' where each row represents a data record. New columns appended to the original
#' data frame include flags for missing results. 
#' 
flag_missing_results <- function(wqp_data, commenttext_missing){
  
  wqp_data_out <- wqp_data %>%
    mutate(flag_missing_result = 
             ( is.na(value) & is.na(DetectionQuantitationLimitMeasure.MeasureValue) ) |
             grepl("not reported", ResultDetectionConditionText, ignore.case = TRUE) |
             grepl(paste(commenttext_missing, collapse = "|"), result_comments, ignore.case = TRUE)
    )
  
  return(wqp_data_out)
  
}