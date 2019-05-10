#load data
sf_data <- read.csv("sf_business_cleaned.csv")
str(sf_data)

#summary of the industries
table(sf_data$naics_code)

#load needed packages
library(tidyverse)
library(lubridate)
library(ggplot2)


###########################################
###########################################

#landscape

#select out variables we want to look at
eda_sd <- sf_data %>% select(owner_number,business_number,location_number,owner_name,business_name,zipcode,neighborhood,business_start,business_end,location_start,location_end,naics_code)

#modify the date type data
eda_sd$business_start <- as.POSIXct(eda_sd$business_start)
eda_sd$business_end <- as.POSIXct(eda_sd$business_end)

#duration on the market
eda_sd_ex<- eda_sd %>% group_by(owner_number,business_number,location_number,owner_name,business_name,zipcode,neighborhood,naics_code) %>% mutate(time_diff= abs(as.numeric(business_end-business_start,units="days"))) 

#today date
eda_sd_ex <- eda_sd_ex %>% mutate(current_date = "2019-05-04")

eda_sd_ex$current_date <- as.POSIXct(eda_sd_ex$current_date)


#businesses death
eda1 <- eda_sd_ex %>% filter(!is.na(time_diff))

#businesses still operating
eda2 <- eda_sd_ex %>% filter(is.na(time_diff)) %>% mutate(duration = abs(as.numeric(current_date-business_start,units="days")))


#business in the most recent 5 years that are still operating until now
eda_recent_5 <- eda2 %>% filter(business_start>=as.Date(("2014-01-01")))

#by_location
by_location <- eda_recent_5 %>% group_by(neighborhood) %>% summarise(count=n()) %>% drop_na(neighborhood) %>% arrange(desc(count)) %>% head(5)

#by_industry
by_industry <- eda_recent_5 %>% filter(neighborhood %in% c("Financial District/South Beach","Mission","South of Market","Sunset/Parkside","Bayview Hunters Point"))  %>%  group_by(neighborhood,naics_code) %>% summarise(count=n()) %>% drop_na(naics_code,neighborhood) %>% arrange(desc(count),.by_group=TRUE) 

#growth rate YoY

growth_rate_top_5 <- eda_recent_5 %>% filter(neighborhood %in% c("Financial District/South Beach","Mission","South of Market","Sunset/Parkside","Bayview Hunters Point"))
growth_rate_top_5 <- growth_rate_top_5 %>% mutate(year=format(as.Date(location_start, format="%Y-%m-%d"),"%Y"))
growth_rate_top_5 <- growth_rate_top_5 %>% filter(naics_code=="Construction") %>% group_by(neighborhood,year) %>% summarise(business_open=n_distinct(location_start))

#growth rate YoY of bayview
growth_rate_industry_bayview <- eda_recent_5 %>% filter(neighborhood=="Bayview")%>% mutate(year=format(as.Date(location_start, format="%Y-%m-%d"),"%Y")) %>% group_by(neighborhood,naics_code,year) %>% summarise(business_open=n_distinct(location_start))

growth_rate_industry_bayview <- growth_rate_industry_sunset %>% group_by(neighborhood,naics_code) %>% mutate(lag=lag(business_open)) %>% mutate(YoY=round(((business_open-lag)/business_open)*100,2))


#growth rate YoY of sunset
growth_rate_industry_sunset <- eda_recent_5 %>% filter(neighborhood=="Sunset/Parkside")%>% mutate(year=format(as.Date(location_start, format="%Y-%m-%d"),"%Y")) %>% group_by(neighborhood,naics_code,year) %>% summarise(business_open=n_distinct(location_start))

growth_rate_industry_sunset <- growth_rate_industry_sunset %>% group_by(neighborhood,naics_code) %>% mutate(lag=lag(business_open)) %>% mutate(YoY=round(((business_open-lag)/business_open)*100,2))


#top players in construction industry in SF
construction <- eda_recent_5 %>% filter(naics_code=="Construction") %>% group_by(neighborhood) %>% drop_na(neighborhood) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(25)

#top players in construction industry in Bayview
bayview_construction <- eda_recent_5 %>% filter(neighborhood=="Bayview Hunters Point"&naics_code=="Construction") %>% group_by(business_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>%  head(5)

############################################
############################################

#bayview
bayview <- eda_recent_5 %>% filter(neighborhood=="Bayview Hunters Point"&naics_code=="Construction")

#modify the date type data
bayview$location_start <- as.POSIXct(bayview$location_start)
bayview$location_end <- as.POSIXct(bayview$location_end)

#duration on the market for location
bayview <- bayview %>% group_by(owner_number,business_number,location_number,owner_name,business_name,zipcode,neighborhood,naics_code) %>% mutate(time_diff_loc= abs(as.numeric(location_end-location_start,units="days"))) %>% filter(is.na(time_diff_loc))

#top 3 players are still around and their duration on the market
bayview_construction_live <- bayview %>% filter(neighborhood=="Bayview Hunters Point"&naics_code=="Construction") %>% group_by(business_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>%  head(3)

#their location start and duration on the market
bayview_construction_live_loc <- bayview %>% filter(business_name %in% bayview_construction_live$business_name) %>% select(business_name,location_start,duration) %>% arrange(desc(location_start)) %>% mutate(duration=duration/365)


############################################
############################################

#Sunset/Parkside
sunset <- eda_recent_5 %>% filter(neighborhood=="Sunset/Parkside"&naics_code=="Construction")

#modify the date type data
sunset$location_start <- as.POSIXct(sunset$location_start)
sunset$location_end <- as.POSIXct(sunset$location_end)

#duration on the market for location
sunset <- sunset %>% group_by(owner_number,business_number,location_number,owner_name,business_name,zipcode,neighborhood,naics_code) %>% mutate(time_diff_loc= abs(as.numeric(location_end-location_start,units="days"))) %>% filter(is.na(time_diff_loc))

#top 3 players are still around and their duration on the market
sunset_construction_live <- sunset %>% group_by(business_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>%  head(10)

#their location start and duration on the market
sunset_construction_live_loc <- sunset %>% filter(business_name %in% sunset_construction_live$business_name) %>% select(business_name,location_start,duration) %>% arrange(desc(location_start)) %>% mutate(duration=duration/365)

############################################
############################################



