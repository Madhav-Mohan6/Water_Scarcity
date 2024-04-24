install.packages("tidyverse")
install.packages("maps")
library(tidyverse)
library(maps)
library(scales)

limited_water_data <- read_csv("/cloud/project/unicef_indicator_1.csv")
basic_water_data <- read_csv("/cloud/project/unicef_indicator_3.csv")
metadata <- read_csv("/cloud/project/unicef_metadata.csv")
continent_data <- read_csv("/cloud/project/data_right_3.csv")


# Merge the data based on year and country
merged_data <- merge(limited_water_data, basic_water_data, by = c("country", "time_period"), all = TRUE)

# Calculate the sum of obs_value for each country and year
merged_data <- mutate(merged_data, total_obs = obs_value.x + obs_value.y)

# Determine the number of countries per year where the sum of obs_value is less than 50%
countries_less_than_50 <- merged_data %>%
  group_by(time_period) %>%
  summarise(num_countries_less_than_50 = sum(total_obs < 50, na.rm = TRUE))

# Create a line chart
ggplot(countries_less_than_50, aes(x = time_period, y = num_countries_less_than_50)) +
  geom_line(color = "#eb4034") +
  geom_point() +
  labs(x = "Year", y = "Countries Count", title = "Countries with Total Water Availability < 50% by Year") + 
  xlim(2012, 2023)

# Filter data for the year 2021
data_2021 <- filter(merged_data, time_period == 2021)

# Sort the data by total_obs in ascending order
data_2021_sorted <- arrange(data_2021, total_obs)

# Take the top 10 countries with the least total_obs
top_10_countries <- head(data_2021_sorted, 10)

# Create a bar chart
ggplot(top_10_countries, aes(x = reorder(country, total_obs), y = total_obs)) +
  geom_bar(stat = "identity", fill = "#eb4034") +
  labs(x = "Country", y = "Water Availability (%)", title = "Top 10 Countries with Least Water Availability in 2021") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Read world map data
world_map <- map_data("world")

# Filter data for the year 2021
data_2021 <- filter(merged_data, time_period == 2021)

# Merge with world map data
merged_map_data <- merge(world_map, data_2021, by.x = "region", by.y = "country", all.x = TRUE)

# Plot world map with geom_polygon
ggplot() +
  geom_polygon(data = merged_map_data, aes(x = long, y = lat, group = group, fill = total_obs)) +
  scale_fill_gradient(low = "#e80c0c", high = "#37c43e", name = "Total obs_value", limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(legend.position = "hide") +
  labs(title = "Total Water Availability in Each Country in 2021")

# Merge with continent information
merged_data <- merge(merged_data, continent_data, by = "country", all.x = TRUE)

# Merge with metadata information
merged_data <- merge(metadata, merged_data, by.x = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "year"), by.y = c("country", "alpha_2_code", "alpha_3_code", "numeric_code", "time_period"), all.x = TRUE)

filtered_data <- merged_data %>% 
  filter(!is.na(continent))

# Plot scatter plot with ggplot2
ggplot(filtered_data, aes(x = `Population, total`, y = `Life expectancy at birth, total (years)`, size = total_obs, color = continent)) +
  geom_point(alpha = 0.1) +
  scale_size_continuous(name = "Total obs", range = c(2, 10)) +
  scale_color_manual(values = c("Asia" = "blue", "Europe" = "#006000", "Africa" = "red", "North America" = "purple", "South America" = "orange", "Oceania" = "yellow")) +
  geom_smooth(method = "lm", se = FALSE) + # Add trend line
  labs(x = "Life expectancy", y = "Population", title = "Life Expectancy vs Population", color = "Continent") +
  theme_minimal() + 
  guides(size = FALSE) + 
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"), breaks = seq(0, max(filtered_data$`Population, total`), by = 200000000))


# Filter data for the three countries
african_countries <- merged_data %>%
  filter(country %in% c("Central African Republic", "Ethiopia", "Madagascar"))

# Plot time series chart with trend line
ggplot(african_countries, aes(x = year, y = total_obs, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend line
  labs(x = "Year", y = "Water Availability (%)", title = "Water Availability Over Time for Selected Countries") +
  scale_color_manual(values = c("Central African Republic" = "blue", "Ethiopia" = "red", "Madagascar" = "green")) +
  theme_minimal() + 
  xlim(2012, 2023) + 
  facet_wrap(~country, scales = "free_y") + 
  guides(color = FALSE)
