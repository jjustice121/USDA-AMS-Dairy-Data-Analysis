#AMS Dairy Data Presentation Script############################
#Author: Jessie Justice
#Date: 08/09/2024

#Import Libraries
library(tidyverse)
library(fpp3)
library(ggplot2)
library(lubridate)

#Set Working Directory
setwd("C:/Users/djset/Desktop/Independent Projects/AMS Dairy Presentation")

#Ingest Data

#Milk Price and Production
mp <- read.csv("Data In/Milk price and production data.csv")

#Cheese Data
ch <- read.csv("Data In/Cheese data.csv")

#Class III Data
c3 <- read.csv("Data In/Class III data.csv")


#Wrangle/Process Data

#Notes on Data Frequency
#Class III Data is Monthly (i.e. reports are released at end of each respective month)
#Milk Price and Production Survey Data is Monthly
#Cheese Data is Weekly (i.e. reports are released at end of each week)

#Notes on observation Level
#All Datasets are on a National level

#consider hierarchy for analysis: Milk Production is broadest category, Class III is a sub-type of Milk Production and, Cheese is a sub-type of Category III Production


#Process Milk Production Data#

#check NA values in all columns
colSums(is.na(mp))

#note: We have no values for Price Received or Production (i.e. the Value column), so we don't need to impute anything

#check number of unique values in each column
sapply(mp, function(x) length(unique(x)))

#note: We can see here that many of the columns we want to drop have no variation/are just indicator columns with a single value

#Remove Columns
mp <- mp %>% select(-c('Week.Ending','State.ANSI','Ag.District','Ag.District.Code','County','County.ANSI','Zip.Code','Region','Watershed','watershed_code','Domain.Category','CV....'))

#Pivot Data.Items Column into two new columns (one for price and one for quantity)
mp <- mp %>% pivot_wider(names_from = Data.Item, values_from = Value)

#convert quantitative columns to numeric and replace commas in Milk Production Column
mp <- mp %>% mutate(`MILK - PRICE RECEIVED, MEASURED IN $ / CWT` = as.numeric(gsub(",","",`MILK - PRICE RECEIVED, MEASURED IN $ / CWT`)), `MILK - PRODUCTION, MEASURED IN LB` = as.numeric(gsub(",","",`MILK - PRODUCTION, MEASURED IN LB`)))

#add numeric indicator for month

#convert Month strings to "Title" Case so we can match with Month.abb
mp <- mp %>% mutate(Period = str_to_title(Period))

mp[['Month']] <- match(mp[['Period']], month.abb)

#filter out quarterly data points, reorder columns to put numeric month column next to string month column , arrange observations by descending year and month
mp <- mp %>% filter(!(Period %in% c('Jan Thru Mar','Apr Thru Jun','Jul Thru Sep','Oct Thru Dec'))) %>% select(c(1,2,3,10,4:9)) %>% arrange(desc(Year), desc(Month))

#change month variable and convert dataframe to Monthly tsibble object
mp <- mp %>% mutate(Date = my(paste(Period,Year, sep ='-')))

mp <- mp %>% mutate(Date = yearmonth(Date)) %>% as_tsibble(index = Date)


#Process Class III Data#

#Make Date Column for tsibble
c3 <- c3 %>% mutate(Date = yearweek(Week.Ending.Date)) %>% as_tsibble(index = Date)


#Process Cheese Data#

#Strip commas and convert sales columns to numeric 
ch <- ch %>% mutate(Cheddar.40.Sales = as.numeric(gsub(",","",Cheddar.40.Sales)), Cheddar.500.Sales = as.numeric(gsub(",","", Cheddar.500.Sales)))

#Make Date Column for tsibble and remove duplicates (there are 6 duplicated rows in the Cheese data)
ch <- ch %>% mutate(Date = yearweek(Week.Ending.Date)) %>% distinct() %>% as_tsibble(index = Date)

#Aggregate up to Month Level


#split cheese data into 40 lb Cheddar and 500 lb Cheddar Datasets

#40 lb Cheddar data
#ch40 <- ch %>% select(Cheddar.40.Weighted.Prices,Cheddar.40.Sales,Week.Ending.Date,Report.Date,Date) %>% 

#500 lb Cheddar data
#ch40 <- ch %>% select(Cheddar.40.Weighted.Prices,Cheddar.40.Sales,Date) %>% as_tsibble(index = Date)

#Notes: We don't know if the prices are in $/ cwt or $/lb or just a general average


#for plotting (especially for seasonality, trend, etc.), it might be easier to just consider recent data

#Plot time series data (Basic Time Plots)

#Milk Production and Pricing Data Plots#

#for Monthly Milk Production (2000-Present)
autoplot(mp,`MILK - PRODUCTION, MEASURED IN LB`)

#for Monthly Milk Prices (Average)(2000-Present)
autoplot(mp,`MILK - PRICE RECEIVED, MEASURED IN $ / CWT`)

#take one years worth of milk data to check seasonality of data

#total milk production has been strong, with a positive trend over
#the past 15 years

#Class III Data Plots#
autoplot(c3, Class.3.Price)


#Cheese Data Plots#
autoplot(ch, Cheddar.40.Sales)
autoplot(ch, Cheddar.40.Weighted.Prices)



autoplot(ch, Cheddar.500.Sales)
autoplot(ch, Cheddar.500.Weighted.Price)




