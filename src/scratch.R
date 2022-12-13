library(targets)
library(tarchetypes)

library(tidyverse)
# library(kableExtra)
library(lubridate)
library(stringr)
# library(rvest)
# library(scales)
# library(ggthemes)

tar_load(raw_true_color)
# raw_true_color <- tar_read(wqp_data_aoi_formatted_filtered) %>%
#   filter(parameter == "true_color")

tar_load(p_codes)

# harmonize_true_color <- function(raw_true_color, p_codes){

# First step is to read in the data and make it workable, we'll then filter
# the data to 1984 and beyond
raw_true_color <- raw_true_color %>% 
  dplyr::select(date = ActivityStartDate,
                parameter = CharacteristicName,
                parm_cd = USGSPCode,
                units = ResultMeasure.MeasureUnitCode,
                SiteID = MonitoringLocationIdentifier,
                org = OrganizationFormalName,
                org_id = OrganizationIdentifier,
                time = ActivityStartTime.Time,
                value = ResultMeasureValue,
                sample_method = SampleCollectionMethod.MethodName,
                analytical_method = ResultAnalyticalMethod.MethodName,
                particle_size = ResultParticleSizeBasisText,
                date_time = ActivityStartDateTime,
                media = ActivityMediaName,
                type = ActivityMediaSubdivisionName,
                sample_depth = ActivityDepthHeightMeasure.MeasureValue,
                sample_depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
                fraction = ResultSampleFractionText,
                status = ResultStatusIdentifier,
                field_comments = ActivityCommentText,
                lab_comments = ResultLaboratoryCommentText,
                result_comments = ResultCommentText) %>%
  left_join(p_codes, by = 'parm_cd') %>%
  mutate(year = year(date),
         units = trimws(units)) %>%
  filter(year >= 1984) %>%
  rowid_to_column(., "index")


# Initial data cleaning Steps ---------------------------------------------

# First off, let's focus our data set to "water" samples (as done in original
# AquaSat). Then we can remove any samples from our data set that failed, or 
# don't have enough lab metadata to make assumptions about the values presented.
# (In the future, these `grepl` functions can be based on the word matrices we
# just produced upstream!)



true_color_no_data_samples <- raw_true_color %>%
  filter(is.na(value)&is.na(units)&is.na(lab_comments)&is.na(result_comments)) #identify samples that have no meaningful data related to an NA value

true_color_fails_removed <- raw_true_color %>%
  filter(media == "Water",
         status %in% c('Accepted','Final','Historical','Validated'),
         # No failure-related field comments, slightly different list of words than
         # lab and result list (not including things that could be used to describe
         # field conditions like "warm", "ice", etc.)
         !grepl(pattern = paste0(c("fail", "suspect", "error", "beyond accept", "interference", 
                                   "questionable", "outside of accept", "problem", "contaminat", 
                                   "improper", "violation", "invalid", "unable", "no test", "cancelled", 
                                   "instrument down", "no result", "time exceed", "not accept", "QC EXCEEDED"),
                                 collapse = "|"),
                x = field_comments,
                ignore.case = T) | is.na(field_comments), 
         # No failure-related lab, should we remove controls comments
         !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                         "questionable", "outside of accept", "problem", "contaminat",
                         "improper", "warm", "violation", "invalid", "unable", "no test",
                         "cancelled", "instrument down", "no result", "time exceed",
                         "not accept", "QC EXCEEDED", "not ice", "ice melt", "PAST HOLDING TIME",
                         "beyond", "exceeded", "failed", "exceededs"),
                       collapse = "|"),
                lab_comments,
                ignore.case = T) | is.na(lab_comments),
         # No failure-related result comments
         !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                         "questionable", "outside of accept", "problem", "contaminat",
                         "improper", "warm", "violation", "invalid", "unable", "no test",
                         "cancelled", "instrument down", "no result", "time exceed",
                         "not accept", "QC EXCEEDED", "not ice", "ice melt",
                         "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
                       collapse = "|"),
                result_comments,
                ignore.case = T) | is.na(result_comments),
         # No failure-related values
         !grepl(paste0(c("fail", "suspect", "error", "beyond accept", "interference",
                         "questionable", "outside of accept", "problem", "contaminat",
                         "improper", "warm", "violation", "invalid", "unable", "no test",
                         "cancelled", "instrument down", "no result", "time exceed",
                         "not accept", "QC EXCEEDED", "not ice", "ice melt", "PAST HOLDING TIME"),
                       collapse = "|"),
                value,
                ignore.case = T) | is.na(value),
         !index %in% true_color_no_data_samples$index)

# Message about types of data kept:
print(paste('We kept',
            round(x = nrow(true_color_fails_removed) /
                    nrow(raw_true_color) * 100,
                  digits = 2),
            '% of samples, because the method used did not make sense. These methods are:'))

rm(pcodes, true_color_no_data_samples) 
gc()

# Next we begin filtering the results to exclude qualitative results 
# this is because there are various color specifications (such as green,
# yellow, brown, etc.) that although give an estimation of apparent color do
# not provide a relevant scale which is comparable to the other
# apparent/ true color scales. 

# Remove the qualitative results from the dataset
true_color_fails_removed$value <- gsub(pattern = "[a-zA-Z]",
                                       replacement = "",
                                       x = true_color_fails_removed$value)

# Create a dataframe from values to identify special characters and see if
# they're worth keeping
testsamples <- true_color_fails_removed %>% 
  count(value) 

# No failure-related values
true_color_vals_filtered <- true_color_fails_removed %>%
  filter(!grepl("*<|#|>| |-|,|/",
                value,
                ignore.case = F) | is.na(value))

# Rerun to make sure special characters were removed, still having a problem
# removing asterisk without removing valied values
testsamples <- true_color_vals_filtered %>% 
  count(value)

# write_feather(true_color_vals_filtered,"../data/true_color_vals_filtered.feather")
# 
# gc()

# The result of dropping all values that are qualitative and not quantitative is
print(paste('We kept',
            round(nrow(true_color_vals_filtered) /
                    nrow(true_color_fails_removed) * 100,
                  2),
            '% of samples, because the values were qualitative and not quantitative.'))

# Since we kept the majority of the data, exclusion of qualitative data should
# not be impactful for the rest of the dataset.

# There are three parameter classifications when there should be two
# Apparent/True Color, the third "Color" classification needs to be further
# explore so for now we'll split the data into three dataframes. 

true_color_apparent <- true_color_vals_filtered %>% 
  filter(parameter == "Apparent color") 

true_color_color <- true_color_vals_filtered %>% 
  filter(parameter == "Color") 

true_color_true <- true_color_vals_filtered %>% 
  filter(parameter == "True color")

parameter_count <- true_color_vals_filtered %>% 
  count(parameter) %>% 
  mutate(paramer_percentage = n / sum(n) * 100)

# The good news is that the true color measurments are the significant portion
# of the data, with apparent color also being the other significant variable
# that we know how to deal with, while color is only a small fraction of the data
parameter_barplot <- ggplot(true_color_vals_filtered) +
  geom_bar(aes(parameter, fill = parameter)) +
  theme_classic() +
  labs(x = "Parameter Variable",
       y = "Parameter Count")

# Next we will analyze the analytical and sample methods to see how the samples
# are being analyzed and processed
apparent_methods <- true_color_apparent %>% 
  count(analytical_method)

color_methods <- true_color_color %>% 
  count(analytical_method)

true_color_methods <- true_color_true %>% 
  count(analytical_method)

# First we will see how the unique analytical methods are being analyzed.
# Unfortunately it seems like a little over a third of the data is being lost
# due to no analytical method being present.
true_color_unique_methods <- true_color_vals_filtered %>% 
  count(analytical_method)%>% 
  mutate(samp_perc = n / sum(n) * 100)

true_color_sample_methods <- true_color_vals_filtered %>% 
  count(sample_method) %>% 
  mutate(samp_perc = n / sum(n) * 100)

# Now we create a classification groups based on analytical methods to sort
# samples into (visual, spectrophotogrametry, photogrametry)

visual_methods <- tibble(group = c("visual"),
                         analytical_method = c("2120 B ~ Color in Water by Visual Comparison",
                                               "Color - Visual Comparison Method",
                                               "Color in Water by Visual Comparison",
                                               "STANDARD METHODS 2120B COLOR BY VISUAL",
                                               "2320 B ~ Alkalinity by Gran Titration"))

specphoto_methods <- tibble(group = c("Spectrophotometry"),
                            analytical_method = c("2120 C ~ Color in Water by Spectrophotometry",
                                                  "Color - Spectrophotometric Single Wavelength Method",
                                                  "Color by Spectrophotometric Analysis",
                                                  "Color in Water by Spectrophotometry Modified",
                                                  "Color in Water by Spectrophotometry"))

photo_methods <- tibble(group = c("Photometry"),
                        analytical_method = c("2120 E ~ Color in Water Using the ADMI Method",
                                              "Color by Calculating ADMI Values",
                                              "Color, wf, visual comparison",
                                              "Color by Calculating ADMI Values"))

ambiguous_methods<-tibble(group = c("Ambiguous"),
                          analytical_method = c("10-308-00-1 A ~ Determination of Color in Water",
                                                "Analytical procedure not specified",
                                                "Color",
                                                "COLOR",
                                                "Color - Pt/Co units",
                                                "COLOR (PLATINUM-COBALT UNITS)",
                                                "COLOR 345 C",
                                                "COLOR 345 F .45",
                                                "COLOR 440 F .22",
                                                "Color Analysis Using Platinum/Cobalt",
                                                "COLOR IN WATERS",
                                                "Color, APHA Platinum-Cobalt",
                                                "Color, True",
                                                "DEP-SOP-NU-094",
                                                "EPA110.2",
                                                "Estero Bay Aquatic Preserve tributary sampling",
                                                "Field measurement/observation, generic method",
                                                "Field Office procedures",
                                                "FIIR - NPHL",
                                                "Laboratory Procedures for Water Quality Chemical Analysis",
                                                "LAKE COUNTY QUALITY SYSTEMS MANUAL",
                                                "Lake Trafford",
                                                "LEGACY",
                                                "Legacy STORET migration; analytical procedure not specified",
                                                "Measurement of Water Color",
                                                "N/A Calculation",
                                                "Other of Unknown Procedure",
                                                "Other or Unknown Procedure",
                                                "SM182120B",
                                                "SM2120C",
                                                "Standard Methods for the Examination of Water and Wastewater",
                                                "To be updated",
                                                "USEPA Methods for Chemical Analysis or Water and Wastewater; EPA 600/4-79-020",
                                                NA,
                                                "Color (EPA)",
                                                "Field - Color",
                                                "ANALYSIS OF COLOR IN WATERS - MODIFIED LACHAT METHOD 10-308-00-1-A",
                                                "Apparent color (EPA110.2 /DODEC)",
                                                "Apparent color (StdMeth /DODEC)",
                                                "Apparent Color, Hach Color Wheel Method",
                                                "Color in the Field by Unknown Limnology Color Chart",
                                                "Color, APHA Platinum-Cobalt",
                                                "COLOR,APPARENT(UNFILTERED SAMPLE) PLAT-COB UNITS",
                                                "DEP Field Analytical Procedures",
                                                "General Listing of Field and Lab Analytical Procedures for Manatee County",
                                                "HACH KIT",
                                                "I1250",
                                                "LAKE COUNTY QUALITY SYSTEMS MANUAL",
                                                "LaMotte Smart 2 Colorimeter",
                                                "LEGACY",
                                                "Legacy STORET migration; analytical procedure not specified",
                                                "Other of Unknown Procedure",
                                                "QA Plan #900456"))

# Combine methods lists
methods_stacked <- rbind(visual_methods, specphoto_methods, photo_methods, ambiguous_methods)

# Now join the new method classifications and we will drop the datat with no
# analytical methods since we can not be sure how the data is being analyzed
# to sort into the classifcations for inslusion in the final aquasat dataset.

true_color_method_groups <- left_join(x = true_color_vals_filtered,
                                      y = methods_stacked,
                                      by = "analytical_method")

# Check to make sure observations all have group vals
true_color_goup_NA <- true_color_method_groups %>% 
  filter(is.na(group))

# Drop the ambiguous methods since we want to keep high quality data, will have
# to check to make sure that we do want to drop these vals
true_color_ambiguous_methods_dropped <- true_color_method_groups %>% 
  filter(!group == "Ambiguous")

# Message about ambiguous records
print(paste('We kept',
            round(nrow(true_color_ambiguous_methods_dropped) /
                    nrow(true_color_vals_filtered) * 100,
                  2),
            '% of samples, because the method could not be classified into an apparent/true color standard:'))

rm(true_color_vals_filtered, true_color_apparent, true_color_color, true_color_true)
gc()

# Now we rerun and filter out nonsensical methods like filtered methods for
# apparent color that can't exist because apparent color is nonfiltered while
# checking to make sure we dropped everything else
apparent_methods_check <- true_color_ambigous_methods_dropped %>% 
  filter(parameter == "Apparent color") %>% 
  count(analytical_method) %>% 
  mutate(percent = n / sum(n) * 100)

color_methods_check <- true_color_ambigous_methods_dropped %>% 
  filter(parameter == "Color") %>% 
  count(analytical_method)%>% 
  mutate(percent = n / sum(n) * 100)

true_color_methods_check <- true_color_ambigous_methods_dropped %>% 
  filter(parameter == "True color") %>% 
  count(analytical_method) %>% 
  mutate(percent = n / sum(n) * 100)

apparent_methods_plot <- ggplot(apparent_methods_check) +
  geom_col(aes(x = analytical_method, y = percent)) +
  theme_classic()+
  labs(x = "Analytical method",
       y = "Percent of data",
       title = "Apparent Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

color_methods_plot <- ggplot(color_methods_check) +
  geom_col(aes(x = analytical_method, y = percent)) +
  theme_classic() +
  labs(x = "Analytical method",
       y = "Percent of data",
       title = "Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

true_color_methods_plot <- ggplot(true_color_methods_check) +
  geom_col(aes(x = analytical_method, y = percent)) +
  theme_classic() +
  labs(x = "Analytical method",
       y = "Percent of data",
       title = "True Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

# The next step is to drop methods that can not occur with specific parameter
# classification i.e apparent color can only be measured using visual methods
# because all other standard require the sample to be filtered before measuring
# and therefore making it apparent color. Luckily the Color parameter
# classification comes with the USGS P-code so we can be sure that these are
# actually True color measurements (00080,	Physical	Color, water, filtered,
# platinum cobalt units	Agree, Dissolved, Color, PCU)

# Should we change these Apparent Color to True Color or not since we cant be
# 100% sure what they logged here?











return(
  list(
    true_color_vals_filtered = true_color_vals_filtered,
    parameter_count = parameter_count,
    parameter_barplot = parameter_barplot,
    true_color_unique_methods = true_color_unique_methods,
    true_color_sample_methods = true_color_sample_methods,
    apparent_methods_plot = apparent_methods_plot,
    color_methods_plot = color_methods_plot,
    true_color_methods_plot = true_color_methods_plot
  )
)



# }