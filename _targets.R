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
  
  # TSS: MRB downloaded from the AquaSat figshare
  tar_file_read(
    name = raw_tss,
    command = "data/raw_tss.feather",
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
  
  # Quarto isn't working, so use Rmds for now. Each of these renders an Rmd and
  # returns a cleaned dataset back to the pipeline
  tar_target(sdd_update,
             render_and_return(input_var = list(raw_sdd = raw_sdd,
                                                p_codes = p_codes),
                               input_file = "~/Documents/aquasat_v2/src/sdd_update.Rmd", 
                               output_file = "~/Documents/aquasat_v2/docs/sdd_update.html"),
             packages = c("kableExtra", "tidyverse", "lubridate",
                          "tm", "stringr", "dplyr", "pdftools"),
             format = "feather",
             # Isn't being tracked correctly, I think because the rendering is
             # nested within a function. So, for now always run...
             cue = tar_cue("always")),
  
  tar_target(silica_update,
             render_and_return(input_var = list(raw_silica = raw_silica,
                                                p_codes = p_codes),
                               input_file = "~/Documents/aquasat_v2/src/silica_update.Rmd", 
                               output_file = "~/Documents/aquasat_v2/docs/silica_update.html"),
             packages = c("kableExtra", "tidyverse", "lubridate",
                          "forcats", "rvest", "scales", "ggthemes"),
             format = "feather",
             # Isn't being tracked correctly, I think because the rendering is
             # nested within a function. So, for now always run...
             cue = tar_cue("always")),
  
  tar_target(true_color_update,
             render_and_return(input_var = list(raw_true_color = raw_true_color,
                                                p_codes = p_codes),
                               input_file = "~/Documents/aquasat_v2/src/true_color_update.Rmd", 
                               output_file = "~/Documents/aquasat_v2/docs/true_color_update.html"),
             packages = c("kableExtra", "tidyverse", "lubridate",
                          "tm", "stringr"),
             format = "feather",
             # Isn't being tracked correctly, I think because the rendering is
             # nested within a function. So, for now always run...
             cue = tar_cue("always")),
  
  tar_target(tss_update,
             render_and_return(input_var = list(raw_tss = raw_tss,
                                                p_codes = p_codes),
                               input_file = "~/Documents/aquasat_v2/src/tss_update.Rmd",
                               output_file = "~/Documents/aquasat_v2/docs/tss_update.html"),
             packages = c("kableExtra", "tidyverse", "pander"),
             format = "feather",
             # Isn't being tracked correctly, I think because the rendering is
             # nested within a function. So, for now always run...
             cue = tar_cue("always"))
  
)




















