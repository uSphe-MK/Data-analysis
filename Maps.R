# Load necessary libraries again after reinstallation
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)
# Read the data from the Excel file
ant_data <- ant_univariate_data

# Calculate the mean longitude and latitude for each site
site_means <- ant_data %>%
  group_by(Sites) %>%
  summarise(mean_long = mean(long, na.rm = TRUE),
            mean_lat = mean(lat, na.rm = TRUE))

# Convert to spatial data frame
site_means_sf <- st_as_sf(site_means, coords = c('mean_long', 'mean_lat'), crs = 4326)

# Plot the map
plot <- ggplot() +
  geom_sf(data = site_means_sf, aes(geometry = geometry), color = 'blue', size = 3) +
  theme_minimal() +
  labs(title = 'Map of Sites with Mean Positions', x = 'Longitude', y = 'Latitude')

print(plot)

ant_data$long <- as.numeric(ant_data$long)
ant_data$lat <- as.numeric(ant_data$lat)


# Register your API key with ggmap
register_google(key = "AIzaSyByNjYBDqB9A8UmFnBJ-37Cj1LqFvV33jU")

# Define location and zoom level
location <- c(lon = c(mean_long = mean(ant_data$long, na.rm = TRUE)),
              lat = c(mean_lat = mean(ant_data$lat, na.rm = TRUE)))

# Create the map using ggmap with Google as the source
Lajuma <- get_googlemap(center = location, zoom = 10, maptype = "roadmap")

# Plot the map
ggmap(Lajuma)

# Sample data frame with lat/lon data
locations <- data.frame(ant_data,
  lon = c(mean_long = mean(ant_data$long, na.rm = TRUE)),
  lat = c(mean_lat = mean(ant_data$lat, na.rm = TRUE)))

# Get the map
base_map <- get_googlemap(center = locations, zoom = 10, maptype = "roadmap")

# Plot the map with points
ggmap(base_map) +
  geom_point(data = sites, aes(x = long, y = lat), color = "red", size = 0.7)+
  geom_text(data = sites, aes(x = long, y = lat, label = Sites), 
            vjust = 0, hjust = 1.2, size = 3, color = "white")

#You can adjust parameters in get_googlemap() to customize your map further:
  
zoom: Zoom level (from 3 to 21)
maptype: Type of map (e.g., "roadmap", "satellite", "hybrid", "terrain")
color: Color scheme ("color" or "bw" for black and white)


# Calculate mean longitude and latitude
mean_long <- mean(ant_univariate_data$long, na.rm = TRUE)
mean_lat <- mean(ant_univariate_data$lat, na.rm = TRUE)

# Create a data frame for locations
location <- data.frame(lon = mean_long, lat = mean_lat)
sites<-data.frame(ant_univariate_data)


# Get the Google Map using the center as a numeric vector
base_map <- get_googlemap(center = c(lon = location$lon, lat = location$lat), 
                          zoom = 11, maptype = "hybrid",scale = 4)

# Plot the map
ggmap(base_map)

