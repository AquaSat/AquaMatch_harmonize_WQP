# Source the functions that will be used to build the targets in p2_targets_list
tar_source("2_download/src/")

p2_targets_list <- list(
  
  # Pull site IDs and total number of records for each site from the WQP inventory
  tar_target(
    p2_site_counts,
    p1_wqp_inventory_aoi %>%
      # Hold onto location info, grid_id, characteristic, and provider data
      # and use them for grouping
      group_by(MonitoringLocationIdentifier, lon, lat, datum, grid_id,
               CharacteristicName, ProviderName) %>%
      # Count the number of rows per group
      summarize(results_count = sum(resultCount, na.rm = TRUE),
                .groups = 'drop') %>%
      # Add the overarching parameter names to the dataset
      left_join(x = .,
                y = p1_wqp_params %>%
                  map2_df(.x,
                          .y = names(.),
                          .f = ~{
                            tibble(CharacteristicName = .x,
                                   parameter = .y)
                          }),
                by = "CharacteristicName") %>%
      group_by(parameter) %>%
      # In case of testing:
      # sample_n(5) %>%
      # Split dataset into a list for iterating downloads by parameter
      split(f = .$parameter)
  ),
  
  # Group the sites into reasonably sized chunks for downloading data 
  # tar_target(
  #   p2_site_counts_grouped,
  #   add_download_groups(p2_site_counts, 
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # Manually breaking up by parameter for now. In the future this should likely
  # be reworked into targets branching. The goal right now is to create a way for
  # each param to be downloaded separately so that an error in one doesn't
  # prevent others from being collected and combined
  # tar_target(
  #   p2_site_counts_grouped_alk,
  #   add_download_groups(p2_site_counts$alkalinity, 
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  tar_target(
    p2_site_counts_grouped_chl,
    add_download_groups(p2_site_counts$chlorophyll, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    p2_site_counts_grouped_sdd,
    add_download_groups(p2_site_counts$secchi, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    p2_site_counts_grouped_tss,
    add_download_groups(p2_site_counts$tss, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  
  tar_target(
    p2_site_counts_grouped_doc,
    add_download_groups(p2_site_counts$doc, 
                        max_sites = 500,
                        max_results = 250000) %>%
      group_by(download_grp) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # tar_target(
  #   p2_site_counts_grouped_temp,
  #   add_download_groups(p2_site_counts$temperature %>%
  #                         # For now exclude STORET - lots of high frequency
  #                         # data there
  #                         filter(ProviderName == "NWIS"), 
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # tar_target(
  #   p2_site_counts_grouped_nitro,
  #   add_download_groups(p2_site_counts$nitrogen,
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # tar_target(
  #   p2_site_counts_grouped_phos,
  #   add_download_groups(p2_site_counts$phosphorus,
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # tar_target(
  #   p2_site_counts_grouped_depth,
  #   add_download_groups(p2_site_counts$depth,
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # tar_target(
  #   p2_site_counts_grouped_ssc,
  #   add_download_groups(p2_site_counts$ssc,
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  # tar_target(
  #   p2_site_counts_grouped_poc,
  #   add_download_groups(p2_site_counts$poc,
  #                       max_sites = 500,
  #                       max_results = 250000) %>%
  #     group_by(download_grp) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  
  
  # Again, manual iteration that should be reworked into branches:
  
  # Map over groups of sites to download data.
  # Note that because error = 'continue', {targets} will attempt to build all 
  # of the "branches" represented by each unique combination of characteristic 
  # name and download group, even if one branch returns an error. This way, 
  # we will not need to re-build branches that have already run successfully. 
  # However, if a branch fails, {targets} will throw an error reading `could
  # not load dependencies of [immediate downstream target]. invalid 'description'
  # argument` because it cannot merge the individual branches and so did not  
  # complete the branching target. The error(s) associated with the failed branch 
  # will therefore need to be resolved before the full target can be successfully 
  # built. A common reason a branch may fail is due to WQP timeout errors. Timeout 
  # errors can sometimes be resolved by waiting a few hours and retrying tar_make().
  # tar_target(
  #   p2_wqp_data_aoi,
  #   fetch_wqp_data(p2_site_counts_grouped,
  #                  char_names = unique(p2_site_counts_grouped$CharacteristicName),
  #                  wqp_args = wqp_args),
  #   pattern = map(p2_site_counts_grouped),
  #   error = "continue"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_alk,
  #   fetch_wqp_data(p2_site_counts_grouped_alk,
  #                  char_names = unique(p2_site_counts_grouped_alk$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_alk),
  #   error = "continue",
  #   format = "feather"
  #   # cue = tar_cue("never")
  # ),
  
  tar_target(
    p2_wqp_data_aoi_chl,
    fetch_wqp_data(p2_site_counts_grouped_chl,
                   char_names = unique(p2_site_counts_grouped_chl$CharacteristicName),
                   wqp_args = p0_wqp_args),
    pattern = map(p2_site_counts_grouped_chl),
    error = "continue",
    format = "feather"
    # cue = tar_cue("never")
  ),
  
  tar_target(
    p2_wqp_data_aoi_sdd,
    fetch_wqp_data(p2_site_counts_grouped_sdd,
                   char_names = unique(p2_site_counts_grouped_sdd$CharacteristicName),
                   wqp_args = p0_wqp_args),
    pattern = map(p2_site_counts_grouped_sdd),
    error = "continue",
    format = "feather"#,
    # cue = tar_cue("never")
  ),
  
  tar_target(
    p2_wqp_data_aoi_tss,
    fetch_wqp_data(p2_site_counts_grouped_tss,
                   char_names = unique(p2_site_counts_grouped_tss$CharacteristicName),
                   wqp_args = p0_wqp_args),
    pattern = map(p2_site_counts_grouped_tss),
    error = "continue",
    format = "feather"#,
    # cue = tar_cue("never")
  ),
  
  tar_target(
    p2_wqp_data_aoi_doc,
    fetch_wqp_data(p2_site_counts_grouped_doc,
                   char_names = unique(p2_site_counts_grouped_doc$CharacteristicName),
                   wqp_args = p0_wqp_args),
    pattern = map(p2_site_counts_grouped_doc),
    error = "continue",
    format = "feather"#,
    # cue = tar_cue("never")
  ),
  
  # tar_target(
  #   p2_wqp_data_aoi_temp,
  #   fetch_wqp_data(p2_site_counts_grouped_temp,
  #                  char_names = unique(p2_site_counts_grouped_temp$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_temp),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_phos,
  #   fetch_wqp_data(p2_site_counts_grouped_phos,
  #                  char_names = unique(p2_site_counts_grouped_phos$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_phos),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_nitro,
  #   fetch_wqp_data(p2_site_counts_grouped_nitro,
  #                  char_names = unique(p2_site_counts_grouped_nitro$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_nitro),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_depth,
  #   fetch_wqp_data(p2_site_counts_grouped_depth,
  #                  char_names = unique(p2_site_counts_grouped_depth$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_depth),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_ssc,
  #   fetch_wqp_data(p2_site_counts_grouped_ssc,
  #                  char_names = unique(p2_site_counts_grouped_ssc$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_ssc),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  # tar_target(
  #   p2_wqp_data_aoi_poc,
  #   fetch_wqp_data(p2_site_counts_grouped_poc,
  #                  char_names = unique(p2_site_counts_grouped_poc$CharacteristicName),
  #                  wqp_args = p0_wqp_args),
  #   pattern = map(p2_site_counts_grouped_poc),
  #   error = "continue",
  #   format = "feather"#,
  #   # cue = tar_cue("never")
  # ),
  
  tar_target(p2_wqp_data_aoi,
             bind_rows(
               # p2_wqp_data_aoi_alk,
               p2_wqp_data_aoi_chl,
               p2_wqp_data_aoi_sdd, 
               p2_wqp_data_aoi_tss,
               p2_wqp_data_aoi_doc, 
               # p2_wqp_data_aoi_temp,
               # p2_wqp_data_aoi_phos, 
               # p2_wqp_data_aoi_nitro,
               # p2_wqp_data_aoi_depth, 
               # p2_wqp_data_aoi_ssc,
               # p2_wqp_data_aoi_poc
             ),
             format = "feather"),
  
  # Summarize the data downloaded from the WQP
  tar_target(
    p2_wqp_data_summary_csv,
    summarize_wqp_download(p1_wqp_inventory_summary_csv, p2_wqp_data_aoi,
                           "2_download/log/summary_wqp_data.csv"),
    format = "file"
  )
  
)