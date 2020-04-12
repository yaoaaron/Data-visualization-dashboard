library(rnoaa)
library(dplyr)

chi_month <- read.csv("USC00111497.csv", header=TRUE, stringsAsFactors = FALSE)
chi_month <- chi_month[,c("DATE","TAVG", "SNOW", "PRCP")]

options(noaakey = "")

# chicago area
library(lawn)
lawn_bbox_polygon(c(-88.347130, 41.577416, -87.303982, 42.360804)) %>% view    #340 stations
lawn_bbox_polygon(c(-87.79, 42.15, -87.78, 42.135)) %>% view   # narrow down to 1 station

city_loc <- ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='asc', offset=0, limit =1000)$data
# 1870-10-15 2018-11-02 Chicago, IL US 1.0000 CITY:US170006

#ncdc_stations(extent = c(41.577416, -88.347130, 42.360804,-87.303982 ))
chicago_station <- ncdc_stations(datasetid = "GHCND", locationid = "CITY:US170006", limit = 1000)$data
ncdc_stations(datasetid='GHCND', stationid='GHCND:USC00111497')


# list all available datatype for station
ncdc_datacats(stationid='GHCND:USC00111497', limit = 100)
# ncdc_datasets()
chi_datatype <- ncdc_datatypes(datasetid = "GHCND", stationid = "GHCND:USC00111497", limit = 1000)$data
chi_datatype_mon <- ncdc_datatypes(datasetid = "GHCNDMS", stationid = "GHCND:USC00111497", limit = 1000)$data

# Monthly mean temperature
temp_monthly = data.frame()
for (i in seq(1763, 2013, 10)) {
ten_year <- ncdc(datasetid = "GHCNDMS", stationid = "GHCND:USC00111497", datatypeid = "MNTM",
             startdate = paste(i, "-01-01", sep=""), enddate = paste(i+9, "-12-31", sep=""), limit = 1000)$data
temp_monthly <- rbind(temp_monthly, ten_year)
}

library(lubridate)
chi_month$DATE <- as.Date(paste(chi_month$DATE, "-01", sep=""))
chi_month$Month <- as.factor(month(chi_month$DATE))
chi_month$year <- as.factor(year(chi_month$DATE))
library(ggplot2)
chi_month %>% filter(year != '1981') %>% group_by(Month) %>% summarise(avg_temp = mean(TAVG, na.rm=TRUE)) %>% 
  ggplot(aes(x=Month, y = avg_temp)) + geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=round(avg_temp,2)), vjust=1.6, color="black", size=3.5) +
  theme_minimal() +
  labs(x = "Month", y = expression(paste("Avg Monthly Temperature (", degree ~ C, " ) - Average by Month"))) 
# monthly
chi_month %>% filter(year != '1981') %>% ggplot(aes(x=DATE, y=TAVG)) + geom_line(color = "red", size = 0.8) + 
  geom_hline(yintercept = mean(chi_month$TAVG, na.rm=TRUE), color = "black") + 
  geom_smooth(method="lm")
# yearly
temp_yearly_plt <- chi_month %>% filter(year != '1981') %>% group_by(year) %>% 
  summarise(yearly_avg = mean(TAVG, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=yearly_avg, group = 1)) + geom_point() + 
  geom_line(color = "red", size = 0.8) + 
  geom_hline(yintercept = mean(chi_month$TAVG, na.rm=TRUE), color = "black") + 
  geom_smooth(method="lm") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Year", y = expression(paste("Avg Yearly Temperature (", degree ~ C, " )"))) 
temp_yearly_plt
# didn't use
chi_month %>% filter(year != '1981') %>% filter(Month %in% c(1,7)) %>% ggplot(aes(x = DATE, y = TAVG, color = Month)) + 
  geom_line(size = 0.8, alpha=0.8) + 
  stat_smooth(aes(group = Month), method = "loess", color = "black") + 
  theme_minimal() + 
  labs(x = "Month", y = expression(paste("Avg Monthly Temperature (", degree ~ C, " )"))) 

# Total snow fall
chi_month %>% group_by(Month) %>% summarise(avg_snow = mean(SNOW, na.rm=TRUE)) %>% 
  ggplot(aes(x=Month, y = avg_snow)) + geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=round(avg_snow, 2)), vjust=1.6, color="black", size=3.5) +
  theme_minimal() +
  labs(x = "Month", y = "Total Monthly Snowfall - Average by Month") 

chi_month %>% ggplot(aes(x=DATE, y=SNOW)) + geom_line(color = "red", size = 0.8) + 
  geom_hline(yintercept = mean(chi_month$SNOW, na.rm=TRUE), color = "black") + 
  geom_smooth(method="lm")

chi_month %>% filter(Month %in% c(1,2,3,12)) %>% ggplot(aes(x = DATE, y = SNOW, color = Month)) + 
  geom_line(size = 0.8, alpha=0.5) + 
  geom_smooth(aes(group = Month), method = "loess", alpha = 0.8, se=FALSE, linetype="dashed", size = 1.5) + 
  theme_minimal() + 
  labs(x = "Month", y = "Total Monthly Snowfall") 

# Total precipitation
chi_month %>% group_by(Month) %>% summarise(avg_PRCP = mean(PRCP, na.rm=TRUE)) %>% 
  ggplot(aes(x=Month, y = avg_PRCP)) + geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label=round(avg_PRCP, 2)), vjust=1.6, color="black", size=3.5) +
  theme_minimal() +
  labs(x = "Month", y = "Total Monthly Precipitation - Average by Month") 

chi_month %>% ggplot(aes(x=DATE, y=PRCP)) + geom_line(color = "red", size = 0.8) + 
  geom_hline(yintercept = mean(chi_month$PRCP, na.rm=TRUE), color = "black") + 
  geom_smooth(method="lm")

chi_month %>% filter(Month %in% c(2,8)) %>% ggplot(aes(x = DATE, y = PRCP, color = Month)) + 
  geom_line(size = 0.8, alpha=0.5) + 
  geom_smooth(aes(group = Month), method = "loess", alpha = 0.8, se=FALSE, linetype="dashed", size = 1.5) + 
  theme_minimal() + 
  labs(x = "Month", y = "Total Monthly Precipitation") 


###### Ozone pollution
ozone <- read.csv('ozone_chi.csv', header=TRUE, stringsAsFactors = FALSE)
ozone$Date <- as.Date(ozone$Date, "%m/%d/%Y")
ozone <- ozone[,c('Date', 'Daily.Max.8.hour.Ozone.Concentration')]
ozone$month <- as.factor(month(ozone$Date))
ozone$year <- as.factor(year(ozone$Date))
ozone$mon_year <- as.Date(paste(as.character(ozone$year), as.character(ozone$month), '01', sep="-"))

# draft
ozone %>% filter(!year %in% c('2018', '2017', '2016', '2015', '2014')) %>% group_by(mon_year) %>% 
  summarise(avg_ozone = mean(Daily.Max.8.hour.Ozone.Concentration, na.rm=TRUE)) %>% 
  ggplot(aes(x=mon_year, y=avg_ozone)) + geom_line(color = "red") + 
  geom_hline(yintercept = mean(ozone$Daily.Max.8.hour.Ozone.Concentration, na.rm=TRUE), color = "black") + 
  geom_smooth(method="lm")

# monthly final
library(plotly)
p <- ozone %>% filter(!year %in% c('2018', '2017', '2016', '2015', '2014')) %>% group_by(mon_year) %>% 
  summarise(avg_ozone = mean(Daily.Max.8.hour.Ozone.Concentration, na.rm=TRUE)) %>% 
  plot_ly(x = ~mon_year, y = ~avg_ozone, mode = 'lines') %>% 
  layout(xaxis = list(title = "Year"), yaxis = list(title="Ozone Concentration (ppm)"))
p

# yearly
ozone_yearly_plt <- ozone %>% filter(!year %in% c('2018', '2017', '2016', '2015', '2014')) %>% group_by(year) %>% 
  summarise(yearly_avg = mean(Daily.Max.8.hour.Ozone.Concentration, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y = yearly_avg, group=1)) + geom_point() + 
  geom_line(color = "red", size = 0.8) + 
  geom_smooth(method="lm") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Year", y = "Ozone Concentration (ppm)") 
ozone_yearly_plt

#### GHG
library(reshape2)
ghg <- read.csv('ghg_by_gas.csv', header = TRUE, stringsAsFactors = FALSE)
ghg <- as.data.frame(t(as.matrix(ghg)))
colnames(ghg) <- as.character(unlist(ghg[1,]))
ghg = ghg[-1, ]
ghg <- tibble::rownames_to_column(ghg, "year")
ghg$year <- substring(ghg$year, 2)
ghg[] <- lapply(ghg, function(x) as.numeric(as.character(x)))
# by gas
ghg_melt <- melt(ghg[,-6], id.vars="year")
ggplot(data=ghg_melt, aes(x=year, y=value, group=variable)) + 
  geom_line(aes(color = variable), size = 1) + 
  labs(x = "Year", y = "Greennouse Gas Emissions") + 
  theme(legend.title=element_blank())
# by economic sector
ghg_sec <- read.csv('ghg_by_sector.csv', header=TRUE, stringsAsFactors = FALSE)
ghg_sec <- as.data.frame(t(as.matrix(ghg_sec)))
colnames(ghg_sec) <- as.character(unlist(ghg_sec[1,]))
ghg_sec = ghg_sec[-1, ]
ghg_sec <- tibble::rownames_to_column(ghg_sec, "year")
ghg_sec$year <- substring(ghg_sec$year, 2)
ghg_sec[] <- lapply(ghg_sec, function(x) as.numeric(as.character(x)))


ggplot(data=melt(ghg_sec[,-9], id.vars="year"), aes(x=year, y=value, group=variable)) + 
  geom_line(aes(color = variable), size = 1) + 
  labs(x = "Year", y = "Greennouse Gas Emissions") + 
  theme(legend.title=element_blank())

# ozone, temp, ghg combined

# final combined graph
year_data <- data.frame("year" = 1982:2018)
year_data$year <- factor(year_data$year)
temp_yearly <- chi_month %>% filter(year != '1981') %>% group_by(year) %>% 
  summarise(yearly_avg = mean(TAVG, na.rm=TRUE)) %>% mutate(measure = "Temp") %>% right_join(year_data)
temp_yearly$measure[which(is.na(temp_yearly$measure))] <- 'Temp'
ozone_yearly <- ozone %>% filter(!year %in% c('2018', '2017', '2016', '2015', '2014')) %>% 
  filter(year != '1981') %>% group_by(year) %>% 
  summarise(yearly_avg = mean(Daily.Max.8.hour.Ozone.Concentration, na.rm=TRUE)) %>% 
  mutate(measure = "Ozone") %>% right_join(year_data)
ozone_yearly$measure[which(is.na(ozone_yearly$measure))] <- 'Ozone'
ghg_yearly <- ghg %>% select(year, Total) %>% mutate(measure = "GHG") %>% rename(yearly_avg = Total) %>% 
   mutate(year = factor(year)) %>% right_join(year_data)
ghg_yearly$measure[which(is.na(ghg_yearly$measure))] <- 'GHG'
combined <- rbind(temp_yearly, ozone_yearly, ghg_yearly)

ggplot(combined, aes(x=year, y = yearly_avg, group = 1)) + geom_line(aes(color=measure)) + 
  facet_grid(measure ~., scales = "free_y") + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Year", y = "") 

