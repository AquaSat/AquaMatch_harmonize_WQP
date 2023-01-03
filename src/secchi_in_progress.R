library(targets)

library(tidyverse)
library(lubridate)

tar_load(p_codes)

# The version from KW:
# tar_load(raw_sdd)

# The version using the pipeline:
raw_sdd <- tar_read(wqp_data_aoi_formatted_filtered) %>%
  filter(parameter == "secchi")

# Manual matchup of secchi analytical methods
sdd_analytical_method_matchup <- read_csv(file = "data/sdd_analytical_method_matchup.csv")

# Manual matchup of secchi sample methods
sdd_sample_method_matchup <- read_csv(file = "data/sdd_sample_method_matchup.csv")

# Manual matchup of secchi collection equipment
sdd_equipment_matchup <- read_csv(file = "data/sdd_collection_equipment_matchup.csv")

# Column renaming match table
match_table <- tar_read(wqp_col_match)

# harmonize_sdd <- function(raw_sdd, p_codes, match_table, sdd_method_matchup){

# First step is to read in the data and make it workable, we'll then filter
# the data to 1984 and beyond

raw_sdd <- raw_sdd %>%
  rename_with(~ match_table$short_name[which(match_table$wqp_name == .x)],
              .cols = match_table$wqp_name) %>%
  left_join(x = ., y = p_codes, by = "parm_cd") %>%
  # Remove trailing white space in labels (Is this still necessary?)
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
print(
  paste0(
    "Number of secchi analytical methods present: ",
    length(unique(raw_sdd$analytical_method))
  )
)

analytical_counts <- raw_sdd %>%
  count(analytical_method) %>%
  arrange(desc(n))

# Add a new column aggregating the analytical methods groups
grouped_analytical_methods_sdd <- raw_sdd %>%
  left_join(x = .,
            y = sdd_analytical_method_matchup,
            by = c("analytical_method"))

# Plot methods grouping counts
sdd_analytical_method_groups_plot <- grouped_analytical_methods_sdd %>%
  count(analytical_method_grouping) %>%
  ggplot() +
  geom_bar(aes(x = analytical_method_grouping, y = n),
           fill = "white",
           color = "black",
           stat = "identity") +
  ylab("Record count") +
  xlab("Analytical method group") +
  ggtitle("SDD aggregation counts") +
  theme_bw()

# Now count the fraction column: 
fraction_counts <- grouped_analytical_methods_sdd %>%
  count(fraction) %>%
  arrange(desc(n))

# Create a column to lump things that do/don't make sense for the fraction column
grouped_fractions_sdd <- grouped_analytical_methods_sdd %>%
  mutate(aquasat_fraction = if_else(
    condition = fraction %in% c(NA, "Total", " ", "None", "Unfiltered", "Field"),
    true = "Makes sense",
    false = "Nonsensical"))

sdd_fraction_groups_plot <- grouped_fractions_sdd %>%
  count(aquasat_fraction) %>%
  ggplot() +
  geom_bar(aes(x = aquasat_fraction, y = n),
           fill = "white",
           color = "black",
           stat = "identity") +
  ylab("Record count") +
  xlab("Fraction group") +
  # ggtitle("SDD aggregation counts") +
  theme_bw()

# Now count the sample_method column: 
sample_counts <- grouped_fractions_sdd %>%
  count(sample_method) %>%
  arrange(desc(n))

# Add a new column describing the sample_method group:
grouped_sample_methods_sdd <- grouped_fractions_sdd %>%
  left_join(x = .,
            y = sdd_sample_method_matchup,
            by = c("sample_method"))

sdd_sample_methods_groups_plot <- grouped_sample_methods_sdd %>%
  count(sample_method_grouping) %>%
  ggplot() +
  geom_bar(aes(x = sample_method_grouping, y = n),
           fill = "white",
           color = "black",
           stat = "identity") +
  ylab("Record count") +
  xlab("Sample methods group") +
  theme_bw()

# Now count the collection_equipment column:
equipment_counts <- grouped_sample_methods_sdd %>%
  count(collection_equipment) %>%
  arrange(desc(n))

group_equipment_sdd <- grouped_sample_methods_sdd %>%
  left_join(x = .,
            y = sdd_equipment_matchup,
            by = c("collection_equipment"))

sdd_equipment_groups_plot <- group_equipment_sdd %>%
  count(equipment_grouping) %>%
  ggplot() +
  geom_bar(aes(x = equipment_grouping, y = n),
           fill = "white",
           color = "black",
           stat = "identity") +
  ylab("Record count") +
  xlab("Equipment group") +
  theme_bw()

# Now count the units column: 
unit_counts <- raw_sdd %>%
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
    sdd_analytical_method_groups_plot = sdd_analytical_method_groups_plot,
    sdd_fraction_groups_plot = sdd_fraction_groups_plot,
    sdd_equipment_groups_plot = sdd_equipment_groups_plot,
    sdd_sample_methods_groups_plot = sdd_sample_methods_groups_plot,
    analytical_counts = analytical_counts,
    fraction_counts = fraction_counts,
    sample_counts = sample_counts,
    unit_counts = unit_counts,
    equipment_counts = equipment_counts
  )
)
# }