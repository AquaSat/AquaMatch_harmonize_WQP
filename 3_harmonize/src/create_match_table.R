# A function that creates a table with shortened names for WQP columns alongside
# their longer original counterparts
create_match_table <- function(){
  
  match_table <- tribble(
    ~short_name,          ~wqp_name, 
    "date", "ActivityStartDate",
    "orig_parameter", "CharacteristicName",
    "parm_cd", "USGSPCode",
    "units", "ResultMeasure.MeasureUnitCode",
    "SiteID", "MonitoringLocationIdentifier",
    "org", "OrganizationFormalName",
    "org_id", "OrganizationIdentifier",
    "time", "ActivityStartTime.Time",
    "value", "ResultMeasureValue_original",
    "value_numeric", "ResultMeasureValue",
    "sample_method", "SampleCollectionMethod.MethodName",
    "analytical_method", "ResultAnalyticalMethod.MethodName",
    "particle_size", "ResultParticleSizeBasisText",
    "date_time", "ActivityStartDateTime",
    "media", "ActivityMediaName",
    "type", "ActivityMediaSubdivisionName",
    # "sample_depth", "ActivityDepthHeightMeasure.MeasureValue",
    # "sample_depth_unit", "ActivityDepthHeightMeasure.MeasureUnitCode",
    #"fraction", "ResultSampleFractionText",
    "status", "ResultStatusIdentifier",
    "field_comments", "ActivityCommentText",
    "lab_comments", "ResultLaboratoryCommentText",
    "result_comments", "ResultCommentText",
    "collection_equipment", "SampleCollectionEquipmentName"
  )
  
  return(match_table)
}