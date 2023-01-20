# A function to pull the parameter codes from the USGS website and save them
# as a table for use in the cleaning process
get_p_codes <- function(){
  
  # Scrape URL
  site_url <- "https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
  
  # Pull table from website
  code_table <- read_html(site_url) %>%
    html_node("table") %>%
    html_table()
  
  # Get parameter codes from table
  p_codes <- code_table %>%
    clean_names() %>%
    mutate(parm_cd = str_pad(string = as.character(parameter_code), 
                             width = 5,
                             pad = "0"))
  
  return(p_codes)
}

