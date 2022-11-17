# Created by use_targets().
# https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Packages that all targets need
  packages = c("tidyverse"), 
  # Default
  format = "rds"
)

# Run the R scripts with custom functions:
tar_source("src/functions.R")

# The list of targets/steps
list(
  
  # Input file tracking -----------------------------------------------------
  
  # Parameter datasets
  
  # Color
  tar_file_read(
    name = raw_true_color,
    command = "data/raw_color_202211.feather",
    read = read_feather(path = !!.x),
    packages = c("feather"),
    format = "feather"
  ),
  
  # Secchi
  tar_file_read(
    name = raw_sdd,
    command = "data/raw_secchi_202211.feather",
    read = read_feather(path = !!.x),
    packages = c("feather"),
    format = "feather"
  ),
  
  # Silica
  tar_file_read(
    name = raw_silica,
    command = "data/raw_silica_202211.feather",
    read = read_feather(path = !!.x),
    packages = c("feather"),
    format = "feather"
  ),
  
  
  # Parameter cleaning ------------------------------------------------------
  
  tar_target(
    # Get parameter codes for use in cleaning processes
    name = p_codes,
    command = get_p_codes(),
    packages = c("tidyverse", "rvest", "janitor")
  ),
  
  # # For some reason this runs forever, or not at all, depending on how I call it
  # tar_quarto(
  #   name = sdd_update,
  #   path = "src/sdd_update.qmd",
  #   # Pass along the target into the knitting environment
  #   execute_params = list(p_codes = p_codes, raw_sdd = raw_sdd),
  #   packages = c("kableExtra", "tidyverse", "lubridate",
  #                "tm", "stringr", "dplyr", "pdftools")),
  # 
  # # This one runs forever and returns this message:
  # # Deno has panicked. This is a bug in Deno.
  # tar_quarto(
  #   name = silica_update,
  #   path = "src/silica_update.qmd",
  #   execute_params = list(p_codes = p_codes, raw_silica = raw_silica),
  #   packages = c("kableExtra", "tidyverse", "lubridate",
  #                "forcats", "rvest", "scales", "ggthemes")),
  # 
  # # This one runs forever and returns this message:
  # # Deno has panicked. This is a bug in Deno.
  # tar_quarto(
  #   name = true_color_update,
  #   path = "src/true_color_update.qmd",
  #   execute_params = list(p_codes = p_codes, raw_true_color = raw_true_color),
  #   packages = c("kableExtra", "tidyverse", "lubridate",
  #                "tm", "stringr")
  # ),
  
  # Quarto isn't working, so use Rmds for now:
  
  tar_render(
    name = sdd_update,
    path = "src/sdd_update.Rmd",
    # Pass along the target into the knitting environment
    # execute_params = list(p_codes = p_codes, raw_sdd = raw_sdd),
    packages = c("kableExtra", "tidyverse", "lubridate",
                 "tm", "stringr", "dplyr", "pdftools")),
  
  # This one runs forever and returns this message:
  # Deno has panicked. This is a bug in Deno.
  tar_render(
    name = silica_update,
    path = "src/silica_update.Rmd",
    # execute_params = list(p_codes = p_codes, raw_silica = raw_silica),
    packages = c("kableExtra", "tidyverse", "lubridate",
                 "forcats", "rvest", "scales", "ggthemes")),
  
  # This one runs forever and returns this message:
  # Deno has panicked. This is a bug in Deno.
  tar_render(
    name = true_color_update,
    path = "src/true_color_update.Rmd",
    # execute_params = list(p_codes = p_codes, raw_true_color = raw_true_color),
    packages = c("kableExtra", "tidyverse", "lubridate",
                 "tm", "stringr")
  )
  
)
