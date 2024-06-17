## AquaSat v2::AquaMatch

This repository is covered by the MIT use license. We request that all downstream uses of this work be available to the public when possible.

### Background

This repository is one part of an expansion and update of the original [AquaSat](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019WR024883) product, a dataset of \~600k coincident field and satellite matchups across four parameters: total suspended solids (TSS), dissolved organic carbon (DOC), chlorophyll-*a* (CHLA), and Secchi disc depth (SDD). The updated product, **AquaMatch**, expands the number of parameters of *in situ* data included in the matching process, adds tiers describing data quality, and adds new satellites and spectral bands. This project repository, AquaMatch_harmonize_WQP, contains the second step in the AquaMatch process: a harmonization workflow for *in situ* [Water Quality Portal (WQP)](waterqualitydata.us/) data following its download. The first step is contained in another repository, [AquaMatch_download_WQP](https://github.com/AquaSat/AquaMatch_download_WQP). The AquaMatch_download_WQP process is dedicated inventorying and downloading the *in-situ* data from the [Water Quality Portal (WQP)](waterqualitydata.us/).

### Technical details

__Workflow__

AquaSat v2 uses the {targets} workflow management R package to reimagine the [original AquaSat codebase](https://github.com/GlobalHydrologyLab/AquaSat). The framework for this workflow is based on code adapted from [this USGS pipeline](https://github.com/USGS-R/ds-pipelines-targets-example-wqp) and has been further developed by members of the [ROSSyndicate](https://github.com/rossyndicate).

Technical details on {targets} workflows are available in the [{targets} User Manual](https://books.ropensci.org/targets/). {targets} workflows are built upon lists of "targets", which can be thought of as analytical steps written out in code. This workflow uses a targets list spread across multiple scripts in an effort to facilitate organization of the code. `_targets.R` serves as the main list of targets and references the other lists of targets, which are defined inside `3a_harmonize.R`, `3b_harmonize_chla.R`, `3c_harmonize_doc.R`, and `create_bookdown.R`. Note that the prefix `3_` is in reference to scripts `1_inventory.R` and `2_download.R` from [AquaMatch_download_WQP](https://github.com/AquaSat/AquaMatch_download_WQP). The pipeline should be run using the `run.R` script, which helps ensure that all the necessary packages are installed. To run a single parameter's component of the pipeline (e.g., chlorophyll *a*) you can use `tar_make(*parameter name*_harmonization_report)`. This will knit the bookdown document associated with that parameter and all upstream harmonization steps.

__Setup__

There are a couple aspects of the workflow that users will need to edit for their use case:

1.  The authorization of the [{googledrive}](https://googledrive.tidyverse.org/index.html) R package. This workflow requires {googledrive} in order to store WQP downloads. Be sure to authorize a Google account to use for online storage when running this workflow. Information on authorization can be found [here](https://googledrive.tidyverse.org/reference/drive_auth.html). The `run.R` script is set up to assume that an account has already been authorized on the user's computer.

2.  The `config.yml` file. This file contains a few configuration profiles ("default", "admin_update", and "use_stable") that dictate some settings relevant to the user's runs of the pipeline. "default" runs the pipeline **without** using any stable versions of datasets made with [AquaMatch_download_WQP](https://github.com/AquaSat/AquaMatch_download_WQP). "use_stable" uses publicly-posted stable versions of datasets but is intended for use by users outside of the ROSSyndicate. "admin_update" is intended for use by ROSSyndicate members when updating harmonized datasets that stable versions of the WQP downloads.

    There are a few settings that may need to be customized within these profiles by the end user: `download_repo_directory`, `google_email`, and `*_stable_date`. `download_repo_directory` is explained in the next point in this list. `google_email` is the Google account the user has authorized locally on their computer (see point 1 above). `*_stable_date` refers to the file version dates that the user wishes to use from those available on Google Drive. For example, if the user wants to use chlorophyll data from March 19th, 2024 they would set `chl_stable_date` to `"20240319"`.

3.  The path, "../AquaMatch_download_WQP/", specified in `download_repo_directory` of `config.yml`. Users will need to have the [AquaMatch_download_WQP](https://github.com/AquaSat/AquaMatch_download_WQP) repository downloaded on their computer when using this workflow, as it references files inside AquaMatch_download_WQP. The `download_repo_directory` should be the path to the "download" repository on your computer. The version stored on the AquaSat GitHub will contain files that link to versions of the data that the AquaSat team has downloaded, but if you are running your own version of the pipeline you will want to run the full AquaMatch_download_WQP pipeline from scratch on your computer before proceeding with the workflow in this repository.

__Organization and documentation__

In general, `src/` folders in this repository contain source code for customized functions used by the {targets} pipeline. The numbered R scripts have functions defined in their respective folders (e.g., `3_harmonize/src/`, etc.).

The `_book/` folder in the project root contains {bookdown} style documentation for the pipeline, primarily focused on the data harmonization steps and decisions made in these steps. The {bookdown} document is the place to look for specifics on, e.g., how chlorophyll data were handled, cleaned, aggregated, and tiered. Additionally, it contains information on our overarching tiering philosophy and its application to specific parameters.

If the `run.R` script has been used to generate the current pipeline version, you can find an html file with the current network diagram for the pipeline in `docs/current_visnetwork.html`.
