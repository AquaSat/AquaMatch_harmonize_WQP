## AquaSat2

AquaSat2 project built using the `targets` package. As of 2023-01-05 this workflow is a combination of code adapted from [this USGS pipeline](https://github.com/USGS-R/ds-pipelines-targets-example-wqp) along with other code developed by members of [ROSS](https://github.com/rossyndicate).

Because of its origins, this targets pipeline currently uses a target list inside of `_targets.R` as well as ones inside `1_inventory.R`, `2_download.R`, and `3_harmonize.R`. These are all combined into a single pipeline inside of `_targets.R`. In the future the entire pipeline will be structured around numeric steps/scripts (i.e., `1_inventory.R`, `2_...`, etc.).

Similarly, there is a `src/` folder in the project root directory that contains `functions.R` and `Rmd`s related to the items defined in `_targets.R`. The numbered R scripts have functions defined in their respective folders (e.g., `1_inventory/src/`, etc.).

The `docs/` folder in the project root contains report outputs from `Rmd` files as well as other project related files that are not scripts or input/output data. Report outputs are currently proof of concept placeholders composed mostly or entirely of figures that may end up in final versions of the report.

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


