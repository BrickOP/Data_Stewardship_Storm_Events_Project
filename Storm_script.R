rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readr)
library(naniar)
library(ggplot2)
library(scales)

# Define the folder path
folder_path <- "D:/MS Data Analytics/Fall 2024/DAT 511/Final Project/Data_Stewardship_Storm_Events_Project"

# Define the file paths for the unzipped CSV files
details_file <- file.path(folder_path, "data", "StormEvents_details-ftp_v1.0_d2024_c20250401.csv.gz")
fatalities_file <- file.path(folder_path, "data", "StormEvents_fatalities-ftp_v1.0_d2024_c20250401.csv.gz")
locations_file <- file.path(folder_path, "data", "StormEvents_locations-ftp_v1.0_d2024_c20250401.csv.gz")

# Load the CSV files from .gz into R
details <- read_csv(details_file)
fatalities <- read_csv(fatalities_file)
locations <- read_csv(locations_file)

# Join the datasets by EVENT_ID
joined_data <- details %>%
  left_join(locations, by = "EVENT_ID") %>%
  left_join(fatalities, by = "EVENT_ID")

# Save the joined data to a new CSV file
output_file <- file.path(folder_path, "StormEvents_joined_data.csv")
write_csv(joined_data, output_file)

# Inform the user
message("Joined data saved to: ", output_file)

# Optional: View the first few rows of the joined data
print(head(joined_data))

# Glimpse of the dataset
glimpse(joined_data)

# Bunch of missing values in the dataset

# Set a seed for reproducibility
set.seed(786)

# Take a random sample of 5000 rows for visualization
sample_data <- joined_data %>%
  sample_n(5000)

# Visual missingness overview
# Calculate % missing for each variable
missing_prop <- colMeans(is.na(sample_data))

# Identify variables with >80% missingness
highlight_vars <- names(missing_prop[missing_prop > 0.8])

# Create vis_miss plot
p <- vis_miss(sample_data) +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1,
    color = ifelse(names(sample_data) %in% highlight_vars, "red", "black"),
    face = ifelse(names(sample_data) %in% highlight_vars, "bold", "plain")
  ))

print(p)

# Percentage of missing values per column

missing_summary <- colSums(is.na(joined_data)) / nrow(joined_data) * 100
missing_summary <- sort(missing_summary, decreasing = TRUE)
print(missing_summary)

gg_miss_var(sample_data)  # Bar chart of missing by variable

# To explore co-missing variables
gg_miss_upset(sample_data, nsets = 10)

# Since 90% more is substantial missingness, we will dropping those columns
# Also looked at data dictionary to determine if these columns are necessary for further analysis or to answer the questions
# Identify columns with >90% missingness
cols_to_drop <- names(missing_summary[missing_summary > 90])

# Drop those columns from the dataset
joined_data_cleaned <- joined_data %>%
  select(-all_of(cols_to_drop))

# Print dropped columns
cat("Dropped columns with >90% missingness:\n")
print(cols_to_drop)

# This is where I preprocessed all the data Now we will do cleaning with respect to Questions raised
##############################################################################################
# Question 1

# Select key variables for analysis
health_data <- joined_data_cleaned %>%
  select(EVENT_ID, EVENT_TYPE, STATE, DEATHS_DIRECT, DEATHS_INDIRECT, INJURIES_DIRECT, INJURIES_INDIRECT)

# Standard U.S. states + DC
us_states <- c(state.name, "District of Columbia") %>% toupper()

# Filter the data
health_data <- health_data %>%
  filter(STATE %in% us_states)

# Visualize missingness as percentages
gg_miss_var(health_data, show_pct = TRUE) +
  labs(title = "Percentage of Missing Values in Key Variables",
       y = "Percentage Missing",
       x = "Variables") +
  theme_minimal()

na_counts <- sapply(health_data, function(x) sum(is.na(x)))

# Looks like there is around 0% missingness in these columns
na_counts <- sapply(health_data, function(x) sum(is.na(x)))

health_data <- health_data %>%
  mutate(TOTAL_HARM = DEATHS_DIRECT + DEATHS_INDIRECT + INJURIES_DIRECT + INJURIES_INDIRECT)

# Check % of rows where there was zero harm
no_harm_rows <- health_data %>%
  filter(TOTAL_HARM == 0)

cat("Percentage of events with zero harm: ", nrow(no_harm_rows) / nrow(health_data) * 100, "%\n")

# Unique event types
unique(health_data$EVENT_TYPE)

# Group and summarize 
event_harm_summary <- health_data %>%
  group_by(EVENT_TYPE) %>%
  summarise(
    TOTAL_DEATHS = sum(DEATHS_DIRECT + DEATHS_INDIRECT, na.rm = TRUE),
    TOTAL_INJURIES = sum(INJURIES_DIRECT + INJURIES_INDIRECT, na.rm = TRUE),
    TOTAL_HARM = sum(TOTAL_HARM, na.rm = TRUE)  
  ) %>%
  arrange(desc(TOTAL_HARM))

# Plot top 10 harmful event types

event_harm_summary %>%
  slice_max(TOTAL_HARM, n = 10) %>%
  ggplot(aes(x = reorder(EVENT_TYPE, TOTAL_HARM), y = TOTAL_HARM, fill = TOTAL_HARM)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = TOTAL_HARM), hjust = -0.1, size = 3.5, color = "black") +
  scale_fill_gradient(low = "#FFE0B2", high = "#D84315") +
  coord_flip() +
  labs(
    title = "Top 10 Most Harmful Weather Events in the U.S for 2024.",
    subtitle = "Based on total reported injuries and deaths (Population Health Impact)",
    x = "Event Type",
    y = "Total Harm (Injuries + Deaths)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  expand_limits(y = max(event_harm_summary$TOTAL_HARM) * 1.1)

# Question 2

# Since there are repetitive event ids due to fatalities and location being on individual level

# Deduplicate at the event level
event_level_data <- health_data %>%
  distinct(EVENT_ID, STATE, EVENT_TYPE, .keep_all = TRUE)

# Count unique events per state and type
event_counts_by_state <- event_level_data %>%
  group_by(STATE, EVENT_TYPE) %>%
  summarise(EVENT_COUNT = n(), .groups = "drop")

# Most frequent event per state
most_common_event_by_state <- event_counts_by_state %>%
  group_by(STATE) %>%
  slice_max(order_by = EVENT_COUNT, n = 1, with_ties = FALSE) %>%
  arrange(STATE)

print(most_common_event_by_state, n= 51)

# plot
most_common_event_by_state %>%
  ggplot(aes(x = reorder(STATE, EVENT_COUNT), y = EVENT_COUNT)) +
  geom_segment(aes(xend = STATE, y = 0, yend = EVENT_COUNT), color = "gray80") +
  geom_point(aes(color = EVENT_TYPE), size = 4) +
  coord_flip() +
  labs(
    title = "Most Common Weather Event Type in Each U.S. State (2024)",
    subtitle = "Based on the frequency of distinct storm events",
    x = "State",
    y = "Event Count",
    color = "Event Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.position = "bottom"
  )

# Question 3

# Use MONTH_NAME column already available in joined_data_cleaned
seasonal_data <- joined_data_cleaned %>%
  select(EVENT_TYPE, MONTH_NAME) %>%
  filter(!is.na(MONTH_NAME), !is.na(EVENT_TYPE))

# Count how many times each event type occurs in each month
event_monthly_counts <- seasonal_data %>%
  group_by(EVENT_TYPE, MONTH_NAME) %>%
  summarise(EVENT_COUNT = n(), .groups = "drop")

# Ensure MONTH_NAME is ordered correctly (January to December)
event_monthly_counts$MONTH_NAME <- factor(
  event_monthly_counts$MONTH_NAME,
  levels = month.name,  
  ordered = TRUE
)


# Plot heatmap of event frequency by month and type

ggplot(event_monthly_counts, aes(x = MONTH_NAME, y = reorder(EVENT_TYPE, -EVENT_COUNT), fill = EVENT_COUNT)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Seasonality of Weather Events in the U.S. (2024)",
    subtitle = "Heatmap shows event frequency by type and month",
    x = "Month",
    y = "Event Type",
    fill = "Event Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10))
  )

# Question 4 : Which event types are associated with the greatest total economic damage, 
# including both property and crop losses?

# Function to convert damage strings like "10K", "1.5M", "3B" to numeric
convert_damage <- function(damage) {
  damage <- toupper(trimws(damage))  # ensure consistency
  numeric_value <- as.numeric(gsub("[KMB]", "", damage))
  
  multiplier <- case_when(
    grepl("K", damage) ~ 1e3,
    grepl("M", damage) ~ 1e6,
    grepl("B", damage) ~ 1e9,
    TRUE ~ 1
  )
  
  return(numeric_value * multiplier)
}

# Select relevant columns for economic damage
economic_data <- joined_data_cleaned %>%
  select(EVENT_TYPE, DAMAGE_PROPERTY, DAMAGE_CROPS)

# Convert to numeric, preserving NAs
economic_data <- economic_data %>%
  mutate(
    PROP_DMG_NUM = ifelse(is.na(DAMAGE_PROPERTY), NA, convert_damage(DAMAGE_PROPERTY)),
    CROP_DMG_NUM = ifelse(is.na(DAMAGE_CROPS), NA, convert_damage(DAMAGE_CROPS)),
    TOTAL_ECONOMIC_DAMAGE = rowSums(cbind(PROP_DMG_NUM, CROP_DMG_NUM), na.rm = TRUE)
  )

# Group by event type and sum the total damage
economic_summary <- economic_data %>%
  group_by(EVENT_TYPE) %>%
  summarise(
    TOTAL_PROPERTY_DAMAGE = sum(PROP_DMG_NUM, na.rm = TRUE),
    TOTAL_CROP_DAMAGE = sum(CROP_DMG_NUM, na.rm = TRUE),
    TOTAL_ECONOMIC_DAMAGE = sum(TOTAL_ECONOMIC_DAMAGE, na.rm = TRUE)
  ) %>%
  arrange(desc(TOTAL_ECONOMIC_DAMAGE))

# View top 10 economically damaging event types
head(economic_summary, 10)


# Format dollar labels in billions or millions as appropriate
format_dollars <- function(x) {
  ifelse(x >= 1e9,
         dollar(x, scale = 1e-9, suffix = "B"),
         dollar(x, scale = 1e-6, suffix = "M"))
}

# Create the enhanced bar chart
economic_summary %>%
  slice_max(TOTAL_ECONOMIC_DAMAGE, n = 10) %>%
  ggplot(aes(x = reorder(EVENT_TYPE, TOTAL_ECONOMIC_DAMAGE), y = TOTAL_ECONOMIC_DAMAGE, fill = TOTAL_ECONOMIC_DAMAGE)) +
  
  # Use gradient fill to emphasize severity
  geom_col(show.legend = FALSE) +
  
  # Add dollar labels at the end of bars
  geom_text(aes(label = format_dollars(TOTAL_ECONOMIC_DAMAGE)),
            hjust = -0.1, size = 3.5, color = "black") +
  
  # Flip coordinates for better readability
  coord_flip() +
  
  # Gradient scale with custom colors
  scale_fill_gradient(low = "#FFECB3", high = "#BF360C") +
  
  # Scale y-axis for clean dollar formatting
  scale_y_continuous(labels = format_dollars) +
  
  # Descriptive labels and titles
  labs(
    title = "Top 10 Weather Event Types by Total Economic Damage (2024)",
    subtitle = "Combined property and crop damages reported by NOAA Storm Events data",
    x = "Event Type",
    y = "Total Economic Damage (USD)"
  ) +
  
  # Theming for a clean and professional look
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#333333"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10), color = "#555555"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 25, 10, 10)
  ) +
  
  # Add padding to prevent label cut-off
  expand_limits(y = max(economic_summary$TOTAL_ECONOMIC_DAMAGE) * 1.12)
