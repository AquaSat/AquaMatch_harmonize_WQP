# A function to pull the paramter codes from the USGS website and save them
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

# A function that allows you to knit an Rmd and also return a value from that
# Rmd back to the pipeline. Based on:
# https://stackoverflow.com/questions/58315771/can-rmarkdown-return-a-value-to-a-target
# Targets does not allow this otherwise:
# https://github.com/ropensci/tarchetypes/discussions/125
render_and_return <- function(input_var, input_file, output_file) {
  # Knit the Rmd:
  rmarkdown::render(input = input_file, output_file = output_file,
                    quiet = TRUE)

  # Variable to be returned. Assigned in the report:
  return_value
}

