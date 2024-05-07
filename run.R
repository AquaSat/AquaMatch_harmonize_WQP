#!/usr/bin/env Rscript

# Please review README.md prior to running the pipeline! It provides important
# instructions for proper configuration.

# Package handling --------------------------------------------------------

# List of packages required for this pipeline
required_pkgs <- c(
  "arrow",
  "bookdown",
  "config",
  "dataRetrieval",
  "devtools",
  "feather",
  "janitor",
  "kableExtra",
  "lubridate",
  "googledrive",
  "lutz",
  "MASS",
  "pander",
  "retry",
  "rvest",
  "scales",
  "sf",
  "sfheaders",
  "targets", 
  "tarchetypes",
  "tidyverse",
  "tigris",
  "tictoc",
  "viridis",
  "visNetwork",
  "yaml")

# Helper function to install all necessary packages
package_installer <- function(x) {
  if (x %in% installed.packages()) {
    print(paste0("{", x ,"} package is already installed."))
  } else {
    install.packages(x)
    print(paste0("{", x ,"} package has been installed."))
  }
}

# map function using base lapply
lapply(required_pkgs, package_installer)

# NOTE: version 0.9.5 of ggrepel does not work with the workflow. Use version
# 0.9.4 instead for the time being:
# https://github.com/slowkow/ggrepel/issues/253
if ( !("ggrepel" %in% installed.packages() ) ){
  print("Installing package version 0.9.4 of ggrepel.")
  
  package_installer("devtools")
  
  devtools::install_version("ggrepel",
                            version = "0.9.4",
                            repos = "http://cran.us.r-project.org")
} else if ( ( "ggrepel" %in% installed.packages() ) & 
            ( packageVersion("ggrepel") != "0.9.4" )  ){
  print("Installing package version 0.9.4 of ggrepel.")
  
  package_installer("devtools")
  
  devtools::install_version("ggrepel",
                            version = "0.9.4",
                            repos = "http://cran.us.r-project.org")
}

# Load packages for use below
library(tidyverse)
library(googledrive)
library(targets)


# Google Drive auth -------------------------------------------------------

# Confirm Google Drive is authorized locally
drive_auth()
# Select existing account (change if starting from scratch)


# Run pipeline ------------------------------------------------------------

# This is a helper script to run the pipeline.
{
  tar_make(p3_chla_agg_harmonized_feather_drive_file)
  
  # Create a network diagram of the workflow, with a completion timestamp
  temp_vis <- tar_visnetwork()
  
  temp_vis$x$main$text <- paste0("Last completed: ", Sys.time())
  
  htmltools::save_html(html = temp_vis,
                       file = "out/current_visnetwork.html")
}
