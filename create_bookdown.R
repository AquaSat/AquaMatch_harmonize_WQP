
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
  
  tar_file(sdd_harmonization_rmd,
           "bookdown_raw/06_sdd_harmonization.Rmd"),
  
  tar_file(tss_harmonization_rmd,
           "bookdown_raw/07_tss_harmonization.Rmd"),
  
  
  # Knit chapters -----------------------------------------------------------
  
  tar_target(
    technical_details,
    rmarkdown::render(
      technical_details_rmd,
      output_file = "01_technical_details",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
  ),
  
  tar_target(
    download_report,
    rmarkdown::render(
      download_rmd,
      params = list(
        site_counts = bind_rows(p2_site_counts),
        global_grid = p1_global_grid,
        yaml_contents = p1_wqp_params),
      output_file = "02_download",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "sf", "tigris", "kableExtra")
  ),
  
  tar_target(
    tiering_overview,
    rmarkdown::render(
      tiering_overview_rmd,
      output_file = "03_tiering_overview",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown")
  ),
  
  tar_target(
    chla_harmonization_report,
    rmarkdown::render(
      chla_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops,
        chla_chars = p1_wqp_params$chlorophyll),
      output_file = "04_chla_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra")
  ),
  
  tar_target(
    doc_harmonization_report,
    rmarkdown::render(
      doc_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops),
      output_file = "05_doc_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra")
  ),
  
  tar_target(
    sdd_harmonization_report,
    rmarkdown::render(
      sdd_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops),
      output_file = "06_doc_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra")
  ),
  
  tar_target(
    tss_harmonization_report,
    rmarkdown::render(
      tss_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops),
      output_file = "07_tss_harmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra")
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
                              sdd_harmonization_report,
                              tss_harmonization_report)),
    cue = tar_cue("always")
  )
  
  
)
