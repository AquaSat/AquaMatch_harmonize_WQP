
bookdown_targets_list <- list(
  
  # Track files -------------------------------------------------------------
  
  tar_file(index, 'index.Rmd'),
  
  tar_file(download_rmd,
           "bookdown_raw/01_download.Rmd"),
  
  tar_file(pre_harmonization_rmd,
           "bookdown_raw/02_preharmonization.Rmd"),
  
  tar_file(chla_harmonization_rmd,
           "bookdown_raw/03_chla_harmonization.Rmd"),
  
  tar_file(doc_harmonization_rmd,
           "bookdown_raw/04_doc_harmonization.Rmd"),
  
  tar_file(sdd_harmonization_rmd,
           "bookdown_raw/05_sdd_harmonization.Rmd"),
  
  tar_file(tss_harmonization_rmd,
           "bookdown_raw/06_tss_harmonization.Rmd"),
  
  # tar_file(modeling_rmd,
  #          "bookdown_raw/model_report.Rmd"),
  # 
  
  # Knit chapters -----------------------------------------------------------
  
  tar_target(
    download_report,
    rmarkdown::render(
      download_rmd,
      params = list(
        site_counts = bind_rows(p2_site_counts),
        global_grid = p1_global_grid),
      output_file = "01_download",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "sf", "tigris")
  ),
  
  tar_target(
    preharmonization_report,
    rmarkdown::render(
      pre_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops),
      output_file = "02_preharmonization",
      output_dir = 'chapters') %>%
      change_ext(inext = 'md', outext = 'Rmd'),
    format = 'file',
    cue = tar_cue("always"),
    packages = c("tidyverse", "bookdown", "ggrepel", "viridis", "kableExtra")
  ),
  
  tar_target(
    chla_harmonization_report,
    rmarkdown::render(
      chla_harmonization_rmd,
      params = list(
        documented_drops = p3_documented_drops),
      output_file = "03_chla_harmonization",
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
      output_file = "04_doc_harmonization",
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
      output_file = "05_doc_harmonization",
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
      output_file = "06_tss_harmonization",
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
                     deps = c(download_report,
                              preharmonization_report,
                              chla_harmonization_report,
                              doc_harmonization_report,
                              sdd_harmonization_report,
                              tss_harmonization_report)),
    cue = tar_cue("always")
  )
  
  
)