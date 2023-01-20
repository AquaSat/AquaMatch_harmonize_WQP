# Function for making a nice table that gets a summary of units and the number 
# of observations with that unit code. (Adapted from AquaSat)
unit_kable <- function(data){
  
  data %>%
    group_by(units) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    kable(., "html", caption = "All  parameter and unit combinations") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}

# Function for making a nice table that gets a summary of units and the number 
# of observations with that analytical method. (Adapted from AquaSat)
analytical_kable <- function(data){
  
  data %>%
    group_by(analytical_method) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    kable(., "html", caption = "All analytical methods and their count") %>%
    kable_styling() %>%
    scroll_box(width = "600px", height = "400px")
  
}

# Function for making a nice table that gets a summary of nonsensical units
# and the number of observations with that analytical method.
# (Adapted from AquaSat)
unit_disharmony <- function(data, lookup){
  
  data %>%
    anti_join(x = ., y = lookup, by = "units") %>%
    group_by(units) %>%
    summarize(count = n())  %>%
    kable(., "html", caption = "The following measurements
          were dropped because the units do not make sense") %>%
    kable_styling() %>%
    scroll_box(width = "500px", height = "400px")
  
}