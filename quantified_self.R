library(dplyr)
library(rbokeh)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)

water_fp <- read.csv("Water Record.csv", header = TRUE, stringsAsFactors = FALSE)

### direct water use
water_fp$date <- as.Date(water_fp$date, format = "%m/%d/%y")
water_fp$dow <- wday(water_fp$date, label = TRUE)
water_fp$bath <- water_fp$Gallons + water_fp$Gallons.1 + water_fp$Gallons.2
water_fp$kitchen <- water_fp$Gallons.3 + water_fp$Gallons.4
water_fp$laundry <- water_fp$Gallons.5
# line chart by room
water_fp %>% select(date, bath, kitchen, laundry) %>% gather(room, water, -date) %>% 
  as.data.frame() %>% 
  figure(ylab = "Water usage (Gallons)", ylim = c(-3, 70)) %>% ly_lines(x = date, y = water, color = room) %>%
  ly_points(x = date, y = water, color = room, hover=c(date, room, water))
# average water use by DOW
water_fp %>% select(dow, bath, kitchen, laundry) %>% group_by(dow) %>%
  summarise(bath = mean(bath), kitchen = mean(kitchen), laundry = mean(laundry)) %>%
  gather(room, water, -dow) %>% as.data.frame() %>%
  figure(xlab = "Day of Week", ylab = "Avg Water usage (Gallons)", ylim = c(-5, 70)) %>% 
  ly_bar(x = dow, y = water, color = room, position = "stack")
# stacked area
water_fp %>% select(date, bath, kitchen, laundry) %>% gather(room, water, -date) %>% 
  as.data.frame() %>% ggplot(aes(x = date, y = water, fill = room)) + geom_area() + 
  labs(y = "Water usage (Gallons)")
# assumptions table
assump <- read_excel("Water Record.xlsx", sheet = "Assump")

### indirect water use
# line chart by type
water_fp$food <- water_fp$Gallons.7 + water_fp$Gallons.8 + water_fp$Gallons.9
water_fp %>% select(date, Gallons.6, electricity.gallons, food) %>% 
  rename(gasoline = Gallons.6, eletricity = electricity.gallons) %>%
  gather(type, water, -date) %>% as.data.frame() %>% 
  figure(ylab = "Water usage (Gallons)") %>% ly_lines(x = date, y = water, color = type) %>%
  ly_points(x = date, y = water, color = type, hover=c(date, type, water))
# gasoline breakdown
water_fp %>% select(date, work.miles, restaurant.miles, entertainment.miles..move..travel..party., grocery, church) %>% 
  rename(work = work.miles, restaurant = restaurant.miles, entertainment = entertainment.miles..move..travel..party.) %>%
  gather(purpose, water, -date) %>% as.data.frame() %>%
  figure(xlab = "Water usage (Gallons)", xlim = c(-5, 100), height = 800, width = 800) %>% 
  ly_bar(x = water, y = date, color = purpose, position = "stack")
# food breakdown
water_fp %>% select(date, Gallons.7, Gallons.8, Gallons.9) %>% 
  rename(breakfast = Gallons.7, lunch = Gallons.8, dinner = Gallons.9) %>%
  gather(meal, water, -date) %>% as.data.frame() %>%
  figure(xlab = "Water usage (Gallons)", xlim = c(-5, 2500)) %>% 
  ly_bar(x = water, y = date, color = meal, position = "stack")
# average water use by DOW
water_fp %>% select(dow, Gallons.6, electricity.gallons, food) %>% group_by(dow) %>%
  summarise(gasoline = mean(Gallons.6), electricity = mean(electricity.gallons), food = mean(food)) %>%
  gather(type, water, -dow) %>% as.data.frame() %>%
  figure(xlab = "Day of Week", ylab = "Avg Water usage (Gallons)", ylim = c(-0.1, 1.7)) %>% 
  ly_bar(x = dow, y = water, color = type, position = "fill")

### Total water footprint
# direct & indirect water
water_fp$direct <- water_fp$bath + water_fp$kitchen + water_fp$laundry
water_fp$indirect <- water_fp$Gallons.6 + water_fp$electricity.gallons + water_fp$food
water_fp %>% select(date, direct, indirect) %>% 
  gather(type, water, -date) %>% as.data.frame() %>% 
  ggplot(aes(x = date, y = water, fill = type)) + geom_area() + 
  geom_hline(yintercept=2220, linetype="dashed", color = "red") + 
  labs(y = "Water usage (Gallons)")
# direct & indirect boxplot
water_fp %>% select(date, direct, indirect) %>% 
  gather(type, water, -date) %>% as.data.frame() %>% 
  figure(ylab = "Water usage (Gallons)") %>%
  ly_boxplot(type, water)
# direct & indirect boxplot by DOW
water_fp$total = water_fp$direct + water_fp$indirect
water_fp %>% select(dow, total) %>%
  figure(ylab = "Water usage (Gallons)", xlab = "Day of Week") %>%
  ly_boxplot(dow, total)

# notes
# According to the GRACE’s Water Footprint Calculator Methodology, the average values for a typical profile of someone 
# in the US and got a value of 2,220 gallons per person per day. The researcher excluded greywater, rain barrels, 
# pools and all recycling, because most people in the US don’t have or, on a regular basis, do those things.

### Carbon Ecological footprint
# carbon footprint
carbon <- read_excel("Water Record.xlsx", sheet = "Carbon")
carbon %>% gather(type, carbon, -Category) %>% as.data.frame() %>%
  figure(xlab = "Carbon Emissions (Lbs)", ylab = "", legend_location = "bottom_right") %>% 
  ly_bar(x = carbon, y = Category, color = type, position = "stack")
# ecological footprint
eco <- read_excel("Water Record.xlsx", sheet = "Ecological")
eco %>% figure(xlab = "Consumption Category", ylab = "Ecological Footprint (global hectares or gha)") %>% 
  ly_bar(x = Consumption_Category, y = Ecological_Footpint) 
# us trend in # earth
us_trend <- read.csv('usa_eco_trends.csv', header = TRUE, stringsAsFactors = FALSE)
us_trend %>% figure(ylab = "# of Earths") %>% ly_lines(x = year, y = Num_of_Earths) %>%
  ly_points(x =year, y = Num_of_Earths, hover=c(year, Num_of_Earths))
# insert picture