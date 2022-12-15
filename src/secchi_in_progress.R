library(targets)

library(tidyverse)
library(lubridate)

tar_load(p_codes)

# The version from KW:
tar_load(raw_sdd)

# Manual matchup of secchi analytical methods
sdd_method_matchup <- read_csv(file = "data/sdd_method_matchup.csv") %>%
  select(analytical_method, method_grouping)

# First step is to read in the data and make it workable, we'll then filter
# the data to 1984 and beyond

raw_sdd <- raw_sdd %>% 
  select(
    date = ActivityStartDate,
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
    result_comments = ResultCommentText
  ) %>%
  left_join(x = .,
            y = p_codes,
            by = "parm_cd") %>%
  mutate(year = year(date),
         units = trimws(units)) %>%
  filter(year >= 1984,
         media %in% c("Water", "water"),
         type %in% c("Surface Water", "Water", "Estuary", "Ocean Water",
                     "Mixing Zone") | is.na(type)) %>%
  # Add an index to control for cases where there's not enough identifying info
  # to track a unique record
  rowid_to_column(., "index")

# Get an idea of how many analytical methods exist:
length(unique(raw_sdd$analytical_method))

raw_sdd %>%
  count(analytical_method) %>%
  arrange(desc(n))

# Add a new column describing the methods groups
grouped_sdd <- raw_sdd %>%
  left_join(x = .,
            y = sdd_method_matchup,
            by = c("analytical_method"))

sdd_method_groups_plot <- grouped_sdd %>%
  count(method_grouping) %>%
  ggplot() +
  geom_bar(aes(x = method_grouping, y = n),
           fill = "white",
           color = "black",
           stat = "identity") +
  ylab("Record count") +
  xlab("Method group") +
  ggtitle("SDD aggregation counts") +
  theme_bw()




# Now count the fraction column: 
raw_sdd %>%
  count(fraction) %>%
  arrange(desc(n))

# Create a column to lump things that do/don't make sense for the fraction column
filtered_sdd <- raw_sdd %>%
  mutate(aquasat_fraction = if_else(
    condition = fraction %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
    true = "Makes sense",
    false = "Nonsensical"))

# Now count the sample_method column: 
raw_sdd %>%
  count(sample_method) %>%
  arrange(desc(n))

# Add a new column describing the sample_method group:
# <Insert grouping methods here>

# Now count the units column: 
raw_sdd %>%
  count(units) %>%
  arrange(desc(n))

# Add a new column describing the units group:
# <Insert grouping methods here>

# Next check the value column for units
# Use that to backfill missing units?
# See how practical this is
# e.g.:
raw_sdd$value[raw_sdd$value %>%
                grepl(pattern = "[a-zA-Z] ")] %>%
  unique()


return(
  list(
    sdd_method_groups_plot = sdd_method_groups_plot
  )
)
