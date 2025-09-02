# Load necessary libraries
library(ggplot2)
library(dplyr)
library(maps)

# Example data: State Outdoor Recreation GDP as a percentage of State GDP with abbreviations
data <- data.frame(
  state = tolower(c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", 
                    "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", 
                    "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", 
                    "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", 
                    "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", 
                    "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", 
                    "virginia", "washington", "west virginia", "wisconsin", "wyoming", "district of columbia")),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
                   "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
                   "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  value = c(2.2, 4.6, 2.7, 2.5, 2.1, 3.2, 1.6, 1.6, 3.6, 2.2, 6.3, 3.3, 2.2, 2.0, 2.1, 2.0, 2.0, 2.9, 3.7, 1.8, 1.8, 
            2.1, 2.5, 2.6, 2.6, 4.6, 2.1, 3.3, 3.4, 1.8, 2.4, 2.1, 2.0, 1.9, 2.2, 2.6, 1.9, 2.4, 2.6, 2.5, 2.2, 3.4, 4.8, 
            1.9, 2.8, 2.1, 2.9, 4.1, 0.8,0.2,0.3)
)

# Load map data for US states
us_states <- map_data("state")

# Merge map data with recreation GDP data
merged_data <- us_states %>%
  left_join(data, by = c("region" = "state"))

# Compute state centroids for labeling
state_centroids <- merged_data %>%
  group_by(region) %>%
  summarize(
    long = mean(range(long)),
    lat = mean(range(lat)),
    abbreviation = first(abbreviation),
    value = first(value)
  )

# Define manual positions for small states and their labels
small_states <- data.frame(
  abbreviation = c("MA", "RI", "CT", "NJ", "DE", "MD", "DC"),
  long_label = c(-64, -68, -68, -75, -75, -75, -77), # x positions for labels
  lat_label = c(43, 41.4, 40.6, 40, 39.5, 38.8, 39), # y positions for labels
  long_point = c(-71.5, -71.4, -72.7, -74.5, -75.2, -76.5, -77.05), # x positions for points
  lat_point = c(42, 41.7, 41.6, 39.8, 39.2, 39, 38.9) # y positions for points
)

# Add the manual labels to the centroids data
state_centroids <- state_centroids %>%
  left_join(small_states, by = "abbreviation")

# Plot the map with labels and external callouts
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", 
                      name = "Percent of State GDP") +
  # Add state labels for most states
  geom_text(data = state_centroids %>% filter(is.na(long_label)), 
            aes(x = long, y = lat, label = paste0(abbreviation, "\n", value)),
            inherit.aes = FALSE, size = 3, color = "black") +
  # Add external labels for small states
  geom_text(data = state_centroids %>% filter(!is.na(long_label)),
            aes(x = long_label, y = lat_label, label = paste0(abbreviation, "\n", value)),
            inherit.aes = FALSE, size = 3, color = "black") +
  # Add lines connecting external labels to states
  geom_segment(data = state_centroids %>% filter(!is.na(long_label)),
               aes(x = long_label, y = lat_label, xend = long_point, yend = lat_point),
               inherit.aes = FALSE, color = "black", size = 0.5) +
  labs(title = "State Outdoor Recreation Value Added as a Percent of State GDP, 2023") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
