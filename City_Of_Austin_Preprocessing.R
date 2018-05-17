library(tidyverse)
# library(ggmap)
# library(zipcode)


# Used to extract coordinates for each zip code
data(zipcode)

# Creates a plot of Austin for use below
# map_of_austin <- get_map(location = c(lon = -97.7431, lat = 30.2672))


###################
# Import Data  - all data can be found at: https://data.austintexas.gov/

# file <- read_file('data/Annual_Crime_2014.csv') 



# CRIME 2014
# These coordinates do not appear to correspond to Austin
crime_2014_data <- read_csv('data/Annual_Crime_2014.csv') %>%
  mutate(Year = rep(2014, dim(.)[1]))
glimpse(crime_2014_data)


# CRIME 2015
# These coordinates do not appear to correspond to Austin
crime_2015_data <- read_csv('data/Annual_Crime_Dataset_2015.csv') %>%
  mutate(Year = rep(2015, dim(.)[1]))
glimpse(crime_2015_data)


# CRIME 2016
# These coordinates do not appear to correspond to Austin
crime_2016_data <- read_csv('data/2016_Annual_Crime_Data.csv') %>%
  mutate(Year = rep(2016, dim(.)[1]))
glimpse(crime_2016_data)


# HOUSING 2014
housing_2014_data <- read_csv('data/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv') %>% 
  select(-`Homes affordable to people earning less than $50,000`
         , -`Rentals affordable to people earning less than $25,000`
         ,-`Rent-restricted units`, -`Housing Choice Voucher holders`, -`Percentage of rental units in poor condition`
         ,-`Percent change in number of housing units, 2000-2012`
         , -`Owner units affordable to average retail/service worker`
         ,-`Rental units affordable to average retail/service worker`, -`Rental units affordable to average artist`
         ,-`Owner units affordable to average artist`, -`Rental units affordable to average teacher`
         ,-`Owner units affordable to average teacher`, -`Rental units affordable to average tech worker`
         ,-`Owner units affordable to average tech worker`, -`Average monthly transportation cost`
         ,-`Percentage of housing and transportation costs that is transportation-related`) %>% 
  mutate(`Population below poverty level` = as.double(str_replace(`Population below poverty level`,'%',''))/100
         ,`Median household income` = as.integer(str_replace(`Median household income`,'\\$',''))
         ,`Non-White, Non-Hispanic or Latino` = as.double(str_replace(`Non-White, Non-Hispanic or Latino`,'%',''))/100
         ,`Hispanic or Latino, of any race` = as.double(str_replace(`Hispanic or Latino, of any race`,'%',''))/100
         ,`Population with disability` = as.double(str_replace(`Population with disability`,'%',''))/100
         ,Unemployment = as.double(str_replace(Unemployment,'%',''))/100
         ,`Large households (5+ members)` = as.double(str_replace(`Large households (5+ members)`,'%',''))/100
         ,`Median rent` = as.integer(str_replace(`Median rent`, '\\$', ''))
         ,`Median home value` = as.integer(str_replace(`Median home value`, '\\$', ''))
         ,`Change in percentage of population below poverty, 2000-2012` = 
           as.double(str_replace(`Change in percentage of population below poverty, 2000-2012`,'%',''))/100
         ,`Change in median rent, 2000-2012` = as.double(str_replace(`Change in median rent, 2000-2012`,'%',''))/100
         ,`Change in median home value, 2000-2012` = 
           as.double(str_replace(`Change in median home value, 2000-2012`,'%',''))/100
         ,`Percentage of homes within 1/4-mi of transit stop` = 
           as.double(str_replace(`Percentage of homes within 1/4-mi of transit stop`,'%',''))/100)
glimpse(housing_2014_data)


# ELECTRIC CAR CHARGING
elec_car_char_data <- read_csv('data/Electric_Vehicle_Charging_Network.csv') 
glimpse(elec_car_char_data)
# View(elec_car_char_data)


# RESTAURANT INSPECTIONS
# read_csv had problems parsing data with a few rows. Not concerned at the moment.
restaurant_reviews_data <- read_csv('data/Restaurant_Inspection_Scores.csv') 
problems(restaurant_reviews_data)
glimpse(restaurant_reviews_data)


# PUBLIC ART
public_art_data <- read_csv('data/City_of_Austin_Public_Art_Collection.csv') 
glimpse(public_art_data)


# PUBLIC VENUES
art_spaces_data <- read_csv('data/Creative_Workspaces__Performance_Venues__Galleries___Museums.csv') 
glimpse(art_spaces_data)

# disciplines <- sort(unique(art_spaces_data$Discipline))
# 
# art_spaces_data2 <- art_spaces_data %>%
#   group_by(ZIP, Discipline) %>%
#   summarise(f = n()) %>%
#   spread(Discipline, f, fill = 0) %>%
#   setNames(c("ZIP", paste(disciplines, sep = "")))


# PARKS
park_data <- read_csv('data/City_of_Austin_Parks_data.csv') 
glimpse(park_data)

management_priorities <- sort(unique(park_data$MANAGEMENT_PRIORITY))

park_data2 <- park_data %>%
  group_by(ZIP_CODE, MANAGEMENT_PRIORITY) %>%
  summarise(f = n()) %>%
  spread(MANAGEMENT_PRIORITY, f, fill = 0) %>%
  setNames(c("ZIP_CODE", paste(management_priorities, sep = "")))


# LIBRARIES
library_data <- read_csv('data/Austin_Public_Library_Locations.csv') %>% 
  mutate(zip_code = str_extract(Address, 'TX\\s[0-9]+')
         ,zip_code = as.integer(substring(zip_code, nchar(zip_code)-5, nchar(zip_code)))
         ,latitude = str_extract(`Latitude / Longitude`, '.+\\,')
         ,latitude = as.double(substring(latitude, 2, nchar(latitude) - 1))
         ,longitude = str_extract(`Latitude / Longitude`, '\\,+.+')
         ,longitude = as.double(substring(longitude, 3, nchar(longitude) - 3)))
glimpse(library_data)


# CAMPAIGN FINANCE --- NOT BEING USED
campaign_finance_data <- read_csv('data/Campaign_Finance_Data_-_Report_Detail_Dataset.csv') %>% 
  mutate(zip_code = substring(Filer_City_State_Zip, nchar(Filer_City_State_Zip)-5, nchar(Filer_City_State_Zip))
         ,Contrib_Total = as.double(str_extract(Contrib_Total, '\\d+\\.\\d+'))
         ,Expend_Total = as.double(str_extract(Expend_Total, '\\d+\\.\\d+'))
         ,Unitemized_Contrib_Total = as.double(str_extract(Unitemized_Contrib_Total, '\\d+\\.\\d+'))
         ,Unitemized_Expend_Total = as.double(str_extract(Unitemized_Expend_Total, '\\d+\\.\\d+'))
         ,Contrib_Balance = as.double(str_extract(Contrib_Balance, '\\d+\\.\\d+'))
         ,Outstand_Loan = as.double(str_extract(Outstand_Loan, '\\d+\\.\\d+'))
         ,Unitemized_InKind_Total = as.double(str_extract(Unitemized_InKind_Total, '\\d+\\.\\d+'))
         ,Unitemized_Pledge_Total = as.double(str_extract(Unitemized_Pledge_Total, '\\d+\\.\\d+'))
         ,Unitemized_Loan_Total = as.double(str_extract(Unitemized_Loan_Total, '\\d+\\.\\d+'))
         ,Unitemized_Unpaid_Total = as.double(str_extract(Unitemized_Unpaid_Total, '\\d+\\.\\d+'))
         ,Unitemized_Cred_Card_Total = as.double(str_extract(Unitemized_Cred_Card_Total, '\\d+\\.\\d+')))
glimpse(campaign_finance_data)


# TRAFFIC CAMERAS --- NOT BEING USED
# No zip codes, but does have latitude/longitude coordinates
traffic_camera_data <- read_csv('data/Traffic_Cameras.csv') %>% 
  filter(`Camera Status` == "TURNED_ON")
glimpse(traffic_camera_data)


# WATER CONSUMPTION
water_consumption_data <- read_csv('data/Austin_Water_-_Residential_Water_Consumption.csv') %>% 
  mutate(Year = substring(`Year Month`, 0, 4)) %>% 
  filter(Year != '2044')
glimpse(water_consumption_data) 

customer_class <- sort(unique(water_consumption_data$`Customer Class`))

water_consumption_data2 <- water_consumption_data %>%
  filter(Year == '2017') %>% 
  group_by(`Postal Code`, `Customer Class`) %>%
  summarise(f = median(`Total Gallons`)) %>%
  spread(`Customer Class`, f, fill = 0) %>%
  setNames(c("Postal Code", paste(customer_class, sep = "")))


###################
# Aggregation


crimes_by_zip2014 <- crime_2014_data %>% 
  group_by(`GO Location Zip`) %>% 
  count() %>% 
  rename(crimes_2014 = n)


crimes_by_zip2015 <- crime_2015_data %>% 
  group_by(`GO Location Zip`) %>% 
  count() %>% 
  rename(crimes_2015 = n)


crimes_by_zip2015B <- crime_2015_data %>% 
  group_by(`GO Location Zip`, `Highest NIBRS/UCR Offense Description`) %>% 
  count() %>% 
  rename(crimes_2015 = n)


crimes_by_zip2016 <- crime_2016_data %>% 
  group_by(`GO Location Zip`) %>%  
  count() %>% 
  rename(crimes_2016 = n)


crimes_by_zip2016B <- crime_2016_data %>% 
  group_by(`GO Location Zip`, `Highest NIBRS/UCR Offense Description`) %>% 
  count() %>% 
  rename(crimes_2016 = n)


car_charges_by_zip <- elec_car_char_data %>% 
  group_by(`Postal Code`) %>%  
  count() %>% 
  rename(ev_charge_stations = n)


car_charges_by_zipB <- elec_car_char_data %>% 
  group_by(`Postal Code`) %>% 
  count(`Customer Category`) %>% 
  rename(ev_charge_stations = n)


rest_reviews_by_zip <- restaurant_reviews_data %>% 
  group_by(`Zip Code`) %>% 
  summarise(mean_review = mean(Score)) 


public_art_by_zip <- public_art_data %>% 
  group_by(`Location Zip Code`) %>% 
  count() %>% 
  rename(public_art = n)


art_spaces_by_zip <- art_spaces_data %>% 
  group_by(ZIP) %>%  
  count() %>% 
  rename(venues = n)


parks_by_zip <- park_data %>% 
  group_by(ZIP_CODE) %>% 
  summarise(total_parks = n()
            ,median_park_acres = median(PARK_ACRES)
            ,mean_park_acres = mean(PARK_ACRES))


libraries_by_zip <- library_data %>% 
  group_by(zip_code) %>% 
  count() %>% 
  rename(libraries = n)


water_consumption_by_zip <- water_consumption_data %>% 
  filter(Year == '2017') %>% 
  group_by(`Postal Code`) %>% 
  summarise('total_water_consumption' = sum(`Total Gallons`))


##################
# Join

# austin_zip_codes <- filter(zipcode, city == 'Austin', state == 'TX') %>%
#   select(zip, latitude, longitude) %>%
#   mutate(zip_code = as.double(zip)) %>% 
#   unique()

# Create a base list of city of Austin zip codes from http://www.city-data.com/zipmaps/Austin-Texas.html
# austin_zip_codes <- tibble(zip_code = c(78610, 78613, 78617, 78641, 78652, 78653, 78660, 78664, 78681, 78701,
#                                         78702, 78703, 78704, 78705, 78712, 78717, 78719, 78721, 78722, 78723,
#                                         78724, 78725, 78726, 78727, 78728, 78729, 78730, 78731, 78732, 78733,
#                                         78734, 78735, 78736, 78737, 78738, 78739, 78741, 78742, 78744, 78745,
#                                         78746, 78747, 78748, 78749, 78750, 78751, 78752, 78753, 78754, 78756,
#                                         78757, 78758, 78759))

austin_zip_codes <- crimes_by_zip2015 %>% 
  rename('zip_code' = 'GO Location Zip')

dim(austin_zip_codes)

austin_zip_codes <- left_join(austin_zip_codes, crimes_by_zip2014, by = c('zip_code' = 'GO Location Zip'))
austin_zip_codes <- left_join(austin_zip_codes, crimes_by_zip2016, by = c('zip_code' = 'GO Location Zip'))
austin_zip_codes <- left_join(austin_zip_codes, car_charges_by_zip, by = c('zip_code' = 'Postal Code'))
austin_zip_codes <- left_join(austin_zip_codes, rest_reviews_by_zip, by = c('zip_code' = 'Zip Code'))
austin_zip_codes <- left_join(austin_zip_codes, public_art_by_zip, by = c('zip_code' = 'Location Zip Code'))
austin_zip_codes <- left_join(austin_zip_codes, art_spaces_by_zip, by = c('zip_code' = 'ZIP'))
# austin_zip_codes <- left_join(austin_zip_codes, art_spaces_data2, by = c('zip_code' = 'ZIP'))
austin_zip_codes <- left_join(austin_zip_codes, housing_2014_data, by = c('zip_code' = 'Zip Code'))
austin_zip_codes <- left_join(austin_zip_codes, parks_by_zip, by = c('zip_code' = 'ZIP_CODE'))
austin_zip_codes <- left_join(austin_zip_codes, park_data2, by = c('zip_code' = 'ZIP_CODE'))
# austin_zip_codes <- left_join(austin_zip_codes, water_consumption_by_zip, by = c('zip_code' = 'Postal Code'))
austin_zip_codes <- left_join(austin_zip_codes, water_consumption_data2, by = c('zip_code' = 'Postal Code'))
austin_zip_codes <- left_join(austin_zip_codes, libraries_by_zip, by = c('zip_code' = 'zip_code'))

austin_zip_codes <- austin_zip_codes %>% 
  mutate(libraries = ifelse(is.na(libraries), 0, libraries))

# zipcode_coordinates <- zipcode %>% 
#   select(zip, latitude, longitude) %>% 
#   mutate(zip = as.double(zip))
# 
# austin_zip_codes <- left_join(austin_zip_codes, zipcode_coordinates, by = c('zip_code' = 'zip'))

dim(austin_zip_codes)
# View(austin_zip_codes)

names(austin_zip_codes)

austin_zip_codes <- austin_zip_codes %>% 
  rename(
         # 'Craft_Design' = "Craft/Design"
         # ,'culinary_arts' = "culinary arts"
         # , 'Culinary_Arts' = "Culinary Arts"
         # , 'Film_TV_Media' = "Film/TV/Media" 
         # , 'Graphic_Design' = "Graphic Design, Printmaking"
         # , 'Literary_Arts' = "Literary Arts" 
         # , 'Technology_Other' = "Technology,Other"
         # , 'Textile_Apparel' = "Textile/Apparel"  
         # , 'Theater_Arts' = "Theater Arts"
         # , 'Visual_Arts' = "Visual Arts"
          'Pop_below_poverty_line' = "Population below poverty level" 
         , 'Median_income' = "Median household income"    
         , 'Non_White_Non_Hispanic' = "Non-White, Non-Hispanic or Latino"  
         , 'Hispanic' = "Hispanic or Latino, of any race"   
         , 'Pop_with_disability' = "Population with disability"  
         , 'Large_household' = "Large households (5+ members)"  
         , 'Median_rent' = "Median rent"        
         , 'Median_home_value' = "Median home value"  
         , 'Chg_pop_below_pov_line' = "Change in percentage of population below poverty, 2000-2012"
         , 'Chg_median_rent' = "Change in median rent, 2000-2012"   
         , 'Chg_median_home_value' = "Change in median home value, 2000-2012"     
         , 'Perc_home_within_quarter_mile_transit' = "Percentage of homes within 1/4-mi of transit stop"   
         , 'Active_Park' = "Active Park"
         , 'Historical_Cultural' = "Historical/Cultural"  
         , 'Mixed_Use' = "Mixed-use Park"      
         , 'Natural_Area' = "Natural Area" 
         , 'Passive_Park' = "Passive Park" 
         , 'Special_Use_Area' = "Special Use Area"  
         , 'Irrigation_MultiFamily' = "Irrigation - Multi-Family" 
         , 'Irrigation_Residential' = "Irrigation - Residential" 
         , 'MultiFamily' = "Multi-Family")
