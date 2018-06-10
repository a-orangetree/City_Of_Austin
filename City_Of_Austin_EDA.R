library(ggthemes)
library(ggExtra)
library(ggmap)

#################
# Please Run City_Of_Austin_Preprocessing.R before running this file
#################

map_of_austin <- get_map(location = c(lon = -97.7431, lat = 30.2672))

######################
# EDA

glimpse(austin_zip_codes_map)


# Lots of NAs, which may indicate that some data sets extend beyond Austin proper
city_of_austin_data <- austin_zip_codes %>% 
  filter(!is.na(Median_home_value)) %>% 
  left_join(austin_zip_codes_coordinates, by = c('zip_code' = 'zip_code'))



# Median Rent and Median Home Value could be collinear
cor(city_of_austin_data$Median_rent, city_of_austin_data$Median_home_value)


# NUMBER OF PUBLIC ART INSTALLATIONS
# Two zip codes have more than 60 public art installations
ggplot(filter(city_of_austin_data, public_art < 60), aes(x = public_art, y = Median_home_value)) +
  geom_point() +
  geom_smooth()


# MEAN PARK ACRES
# Four zip codes have more than 250 acres on average of park space
ggplot(filter(city_of_austin_data, mean_park_acres < 250), aes(x = mean_park_acres, y = Median_home_value)) +
  geom_point() +
  geom_smooth()


# MEDIAN PARK ACRES
# One zip code has more than 950 acres on average of park space
ggplot(filter(city_of_austin_data, median_park_acres < 950), aes(x = median_park_acres, y = Median_home_value)) +
  geom_point() +
  geom_smooth()


# TOTAL NUMBER OF PARKS
ggplot(city_of_austin_data, aes(x = total_parks, y = Median_home_value)) +
  geom_point() +
  geom_smooth()


# % HISPANIC/LATINO
ggplot(city_of_austin_data, aes(x = Hispanic, y = Median_home_value)) +
  geom_point(aes(color = Pop_below_poverty_line)) +
  geom_smooth(color = 'red') +
  labs(y = 'Median Home Value', x = '% of Hispanic Population') +
  # ggtitle('Home Values vs Percent of Hispanic Population') +
  # theme(plot.title = element_text(hjust = 0.5)) +
  theme_fivethirtyeight()


# MEDIAN INCOME
ggplot(city_of_austin_data, aes(x = Median_income, y = Median_home_value)) +
  geom_point() +
  geom_smooth(color = 'yellow') +
  labs(y = 'Median Home Value', x = 'Median Income') +
  # ggtitle('Home Values vs Median Income') +
  # theme(plot.title = element_text(hjust = 0.5)) +
  theme_fivethirtyeight()


# MEDIAN INCOME
ggplot(city_of_austin_data, aes(x = Chg_median_rent, y = Median_home_value)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Change in Median Rent', x = 'Median Income')


# WATER CONSUMPTION
# ggplot(city_of_austin_data, aes(x = total_water_consumption, y = Median_home_value)) +
#   geom_point() +
#   geom_smooth() +
#   labs(y = 'Median Home Value', x = 'Water Consumption in 2017')


# MAP OF HISPANIC/LATINO
ggmap(map_of_austin) +
  geom_point(aes(x = longitude, y = latitude, size = Median_home_value, color = Hispanic)
             ,data = city_of_austin_data
             ,alpha = .5
             ,na.rm = TRUE) 


# MAP OF PUBLIC ART
ggmap(map_of_austin) +
  geom_point(aes(x = `Location Longitude`, y = `Location Latitude`)
             ,data = public_art_data
             ,alpha = .5
             ,na.rm = TRUE) 


# MAP OF VENUES
ggmap(map_of_austin) +
  geom_point(aes(x = longitude, y = latitude, size = Median_home_value, color = Unemployment)
             ,data = city_of_austin_data
             ,alpha = .5
             ,na.rm = TRUE)


# MAP OF LIBRARIES
ggmap(map_of_austin) +
  geom_point(aes(x = longitude, y = latitude)
             ,data = library_data
             ,alpha = .5
             ,na.rm = TRUE) 


# MAP OF TRAFFIC CAMERAS
ggmap(map_of_austin) +
  geom_point(aes(x = Longitude, y = Latitude)
             ,data = traffic_camera_data
             ,alpha = .5
             ,na.rm = TRUE) 
