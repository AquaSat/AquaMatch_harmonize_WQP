## AquaSat v2::AquaMatch

This repository is covered by the MIT use license. We request that all downstream uses of this work be available to the public when possible.

This repository is part of an expansion of the original [AquaSat](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019WR024883) product, a dataset of 600k coincident field and satellite matchups. The new product, **AquaSat v2**, expands the amount of *in-situ* data included in the matching process, adds tiers describing data quality, and adds new satellites and spectral bands. This project repository ("AquaMatch") is dedicated to the processes of inventorying, downloading, and harmonizing *in-situ* data from sources such as the [Water Quality Portal (WQP)](waterqualitydata.us/). 

AquaMatch uses the {targets} workflow management R package to reimagine the [original AquaSat codebase](https://github.com/GlobalHydrologyLab/AquaSat). The framework for this workflow is based on code adapted from [this USGS pipeline](https://github.com/USGS-R/ds-pipelines-targets-example-wqp) and has been further developed by members of the [ROSSyndicate](https://github.com/rossyndicate).

Technical details on {targets} workflows are available in the  [{targets} User Manual](https://books.ropensci.org/targets/). {targets} workflows are built upon lists of "targets", which can be thought of as analytical steps written out in code. This workflow uses a targets list spread across multiple scripts in an effort to facilitate organization of the code. `_targets.R` serves as the main list and references the other lists, which are defined inside `1_inventory.R`, `2_download.R`, and `3_harmonize.R`. These are all combined into a single pipeline using `_targets.R`. 

In general, `src/` folders in this repository contain source code for customized functions used by the {targets} pipeline. The numbered R scripts have functions defined in their respective folders (e.g., `1_inventory/src/`, etc.).

The `_book/` folder in the project root contains {bookdown} style documentation for the pipeline, primarily focused on the data harmonization steps and decisions made in these steps. The {bookdown} document is the place to look for specifics on, e.g., how data pertaining to variables such as chlorophyll were handled, cleaned, aggregated, and tiered. 

If the `run.R` script has been used to generate the current pipeline version, you can find an html file with the current network diagram for the pipeline in `docs/current_visnetwork.html`.


## Parameter-specific methods

For Chl-a: 

For DOC: it must be filtered (by characteristic name or ResultSampleFractionText) before going into the tiering strucutre
- ResultSampleFractionText - dissolved, total, filtered


## Tiering Philosophy

Many columns related to the direct interoperability of data in the WQP are often filled with NAs specifically field and lab methodology columns and 
explicit depth-related columns (provide some context of what proportion, maybe?). A purely restrictive filtering of WQP would result in very limited 
data, in part, due to the inconsistent entry of data from data providers, therefore, we are building this dataset to resolve as many NAs as possible, 
but to also include NAs as 'inclusive' data.

Our philosophy focuses on the following columns from the WQP:
ReusltAnalyticalMethod.MethodName - analytical method - first stop!
SampleCollectionMethod.MethodName - field collection
ActivityDepthHeightMeasure.MeasureValue - discrete depth for grab samples
ActivityTopDepthHeightMeasure.MeasureValue, ActivityBottomDepthHeightMeasure.MeasureValue - for integrated samples

Generally speaking, we only further resolve the flags/tiers if there is a persistent 1% of samples that make it 'worth it' to reclassify from inclusive tier.

If an NA in analytical methods: inclusive, unless resolvable by another column USGSPCode, ResultLaboratoryCommentText, MethodDescriptionText, ResultCommentText. 

If an NA in field comments: look in field comments ActivityCommentText


