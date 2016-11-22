#---------------------------------------------- Preparing the data -------------------------------------------
#packages to be used
library(dplyr)
library(ggplot2)

#import rainfall data
weather_data <- read.csv("./data/weather_data.csv", header = TRUE)

#import yield data
yield_data <- read.csv("./data/yield_data.csv", header = TRUE)

#replace negative values (missing or questionable data) with 0
weather_data$rain[weather_data$rain < 0] <- 0

#create rainfall table with dates
rainfall_mm <- weather_data %>%
  mutate(rainfall_mm = rain * 25.4) %>%
  select(year, rainfall_mm)

#create a date value column
rainfall_mm$date <- as.Date(with(weather_data, paste(year, month, day, sep="-")), "%Y-%m-%d" )

#transform dates into values
yield_data$plant_date <- as.Date(yield_data$plant_date, "%m/%d/%Y")
yield_data$harvest_date <- as.Date(yield_data$harvest_date, "%m/%d/%Y")

#convert bu/ac into T/ha
yld_t_ha <- yield_data %>%
  mutate(yield_t_ha = round((yield * .0673), 2)) %>%
  select(year, plant_date,harvest_date, yield_t_ha)

for(i in 1:nrow(yld_t_ha)){
  yld_t_ha$season_rainfall_mm[i] <- 
    filter(rainfall_mm,date >= yld_t_ha$plant_date[i] & date <= yld_t_ha$harvest_date[i]) %>%
    select(.,rainfall_mm) %>%
    sum(.)}

#---------------------------------------------- Analyzing the data -------------------------------------------
#plot yield data over years
ggplot(master_data, aes(x = year, y = yield_t_ha)) + geom_line() + xlab("Year") + ylab("Yield (T/ha)" )

#plot rainfall data over years
ggplot(master_data, aes(x = year, y = yearly_rain_mm)) + geom_line() + xlab("Year") + ylab("Rainfall (mm)")