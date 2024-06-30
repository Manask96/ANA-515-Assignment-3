# Load necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Read the dataset (replace with your actual file path)
data <- read.csv("/Users/mkore/Downloads/StormEvents_details-ftp_v1.0_d1950_c20210803.csv")

# Step 2: Limit the dataframe to the specified columns
data <- data %>% select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)
print("Step 2: Limited dataframe to specified columns")
print(head(data))

# Step 3: Arrange the data by the state name
data <- data %>% arrange(STATE)
print("Step 3: Arranged data by state name")
print(head(data))

# Step 4: Change state and county names to title case
data <- data %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))
print("Step 4: Changed state and county names to title case")
print(head(data))

# Step 5: Limit to the events listed by county FIPS (CZ_TYPE of "C") and remove the CZ_TYPE column
data <- data %>% filter(CZ_TYPE == "C") %>% select(-CZ_TYPE)
print("Step 5: Limited to events listed by county FIPS and removed the CZ_TYPE column")
print(head(data))

# Step 6: Pad the state and county FIPS with a “0” at the beginning and then unite the two columns to make one FIPS column
data <- data %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, 3, pad = "0"), CZ_FIPS = str_pad(CZ_FIPS, 3, pad = "0"))
data <- data %>% unite("FIPS", STATE_FIPS, CZ_FIPS, sep = "")
print("Step 6: Padded state and county FIPS with '0' and united the two columns")
print(head(data))

# Step 7: Change all the column names to lower case
data <- data %>% rename_all(tolower)
print("Step 7: Changed all column names to lower case")
print(head(data))

# Step 8: Create a dataframe with state information (name, area, region)
data("state")
state_info <- data.frame(state = state.name, area = state.area, region = state.region)
print("Step 8: Created a dataframe with state information")
print(head(state_info))

# Step 9: Create a dataframe with the number of events per state and merge it with the state information dataframe
events_per_state <- data %>% group_by(state) %>% summarise(events = n())
merged_data <- merge(events_per_state, state_info, by.x = "state", by.y = "state", all.x = TRUE)
print("Step 9: Created a dataframe with the number of events per state and merged with state information")
print(head(merged_data))

# Step 10: Remove any states that are not in the state information dataframe
merged_data <- merged_data %>% filter(!is.na(area))
print("Step 10: Removed any states that are not in the state information dataframe")
print(head(merged_data))

# Step 11: Create the plot
print("Step 11: Created the plot")
ggplot(merged_data, aes(x = area, y = events, color = region)) +
  geom_point() +
  labs(x = "Land area (square miles)", y = "# of storm events in 2017") +
  theme_minimal()
