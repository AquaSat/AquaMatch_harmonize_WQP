#!/usr/bin/env Rscript

# list of packages required for this pipeline
required_pkgs <- c("targets", 
  "tarchetypes",
  "sf",
  "tidyverse",
  "feather",
  "lubridate",
  "ggrepel",
  "scales",
  "tigris",
  "tictoc")

# helper function to install all necessary pacakges
package_installer <- function(x) {
  if (x %in% installed.packages()) {
    print(paste0("{", x ,"} package is already installed."))
  } else {
    install.packages(x)
    print(paste0("{", x ,"} package has been installed."))
  }
}

# map function using base lapply
lapply(packages, package_installer)


library(targets)

# This is a helper script to run the pipeline.
{
  tar_make()
  
  # Create a network diagram of the workflow, with a completion timestamp
  temp_vis <- tar_visnetwork()
  
  temp_vis$x$main$text <- paste0("Last completed: ", Sys.time())
  
  htmltools::save_html(html = temp_vis,
                       file = "docs/current_visnetwork.html")
}
