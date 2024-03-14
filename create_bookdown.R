
bookdown_targets_list <- list(
  
  # Track files -------------------------------------------------------------
  
  tar_file(index, 'index.Rmd'),
  
  tar_file(technical_details_rmd,
           "bookdown_raw/01_technical_details.Rmd"),
  
  tar_file(download_rmd,
           "bookdown_raw/02_download.Rmd"),
  
  tar_file(tiering_overview_rmd,
           "bookdown_raw/03_tiering_overview.Rmd"),
  
  tar_file(chla_harmonization_rmd,
           "bookdown_raw/04_chla_harmonization.Rmd"),
  
  tar_file(doc_harmonization_rmd,
           "bookdown_raw/05_doc_harmonization.Rmd"),
  
  tar_file(references_rmd,
           "bookdown_raw/references.Rmd"),
  
  
  # Knit chapters -----------------------------------------------------------
  
  tar_target(
    technical_details,
    render(
      technical_details_rmd,
      output_file = "01_technical_details",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = "rmarkdown"
  ),
  
  tar_target(
    download_report,
    render(
      download_rmd,
      params = list(
        site_counts = bind_rows(p2_site_counts_chl, p2_site_counts_doc),
        global_grid = p1_global_grid,
        yaml_contents = list(chlorophyll = p1_wqp_params_chl$chlorophyll,
                             doc = p1_wqp_params_doc$doc)),
      output_file = "02_download",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    packages = c("tidyverse", "sf", "tigris", "kableExtra", "rmarkdown")
  ),
  
  tar_target(
    tiering_overview,
    render(
      tiering_overview_rmd,
      output_file = "03_tiering_overview",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    packages = c("tidyverse", "bookdown", "rmarkdown")
  ),
  
  tar_target(
    chla_harmonization_report,
    render(
      chla_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops,
        chla_chars = p1_wqp_params_chl$chlorophyll),
      output_file = "04_chla_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra",
                 "rmarkdown")
  ),
  
  tar_target(
    doc_harmonization_report,
    render(
      doc_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops,
        doc_chars = p1_wqp_params_doc$doc),
      output_file = "05_doc_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra",
                 "rmarkdown")
  ),

  tar_target(
    references,
    render(
      references_rmd,
      output_file = "references",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    packages = c("bookdown", "rmarkdown", "tidyverse")
  ),
  
  # Render book -------------------------------------------------------------
  
  tar_target(
    book,
    render_with_deps(index = index,
                     deps = c(technical_details,
                              download_report,
                              tiering_overview,
                              chla_harmonization_report,
                              doc_harmonization_report,
                              references)),
    cue = tar_cue("always")
  )
  
  
)