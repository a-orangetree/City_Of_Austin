#################
# Please Run City_Of_Austin_Preprocessing.R before running this file
#################

######################
# EDA

glimpse(austin_zip_codes)


# Lots of NAs, which may indicate that some data sets extend beyond Austin proper
city_of_austin_data <- austin_zip_codes %>% 
  filter(!is.na(`Median home value`))

glimpse(city_of_austin_data)
dim(city_of_austin_data)


# Median Rent and Median Home Value could be collinear
cor(city_of_austin_data$`Median rent`, city_of_austin_data$`Median home value`)


# NUMBER OF PUBLIC ART INSTALLATIONS
# Two zip codes have more than 60 public art installations
ggplot(filter(city_of_austin_data, public_art < 60), aes(x = public_art, y = `Median home value`)) +
  geom_point() +
  geom_smooth()


# MEAN PARK ACRES
# Four zip codes have more than 250 acres on average of park space
ggplot(filter(city_of_austin_data, mean_park_acres < 250), aes(x = mean_park_acres, y = `Median home value`)) +
  geom_point() +
  geom_smooth()


# MEDIAN PARK ACRES
# One zip code has more than 950 acres on average of park space
ggplot(filter(city_of_austin_data, median_park_acres < 950), aes(x = median_park_acres, y = `Median home value`)) +
  geom_point() +
  geom_smooth()


# TOTAL NUMBER OF PARKS
ggplot(city_of_austin_data, aes(x = total_parks, y = `Median home value`)) +
  geom_point() +
  geom_smooth()


# % HISPANIC/LATINO
ggplot(city_of_austin_data, aes(x = `Hispanic or Latino, of any race`, y = `Median home value`)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Median Home Value', x = '% Hispanic/Latino')


# MEDIAN INCOME
ggplot(city_of_austin_data, aes(x = `Median household income`, y = `Median home value`)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Median Home Value', x = 'Median Income')


# MEDIAN INCOME
ggplot(city_of_austin_data, aes(x = `Change in median rent, 2000-2012`, y = `Median home value`)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Change in Median Rent', x = 'Median Income')


# WATER CONSUMPTION
ggplot(city_of_austin_data, aes(x = total_water_consumption, y = `Median home value`)) +
  geom_point() +
  geom_smooth() +
  labs(y = 'Median Home Value', x = 'Water Consumption in 2017')


# MAP OF HISPANIC/LATINO
ggmap(map_of_austin) +
  geom_point(aes(x = longitude, y = latitude, size = `Median home value`, color = `Hispanic or Latino, of any race`)
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
  geom_point(aes(x = longitude, y = latitude, size = `Median home value`, color = venues)
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
