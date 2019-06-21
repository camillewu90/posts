library(tidyverse)
library(dplyr)
data<-read.csv("telecom.csv",stringsAsFactors = FALSE)
## look at the structure of the data
glimpse(data)
## look for na in MonthlyCharges

is.na(data$MonthlyCharges)
anyNA(data$MonthlyCharges)
is.nan(data$MonthlyCharges)

## summarize the number of nas in MonthlyCharges
data %>% 
  summarize(na.count=sum(is.na(MonthlyCharges)))

#replace the missing value with median of MonthlyCharges
data<-data %>% 
  mutate(MonthlyCharges=replace(MonthlyCharges,
                                list= is.na(MonthlyCharges),
                                values=median(MonthlyCharges,na.rm=TRUE)))
## look for na in TotalCharges (character vector), R does not recognize na and N/A as missing values
## the easiest way is to coerce TotalCharges to numeric vector first, so NAs will be correctly detected
## but this might not always be a good idea, because you might miss other anomlies 

is.na(data$TotalCharges)

data %>%
  summarize(count=sum(is.na(TotalCharges)))

## replace not correct missing value to NA
data<-data %>%
  mutate(TotalCharges=replace(TotalCharges,
                              list = TotalCharges == 'na',
                              values = NA)) %>%
  mutate(TotalCharges=replace(TotalCharges,
                              list = TotalCharges == 'N/A',
                              values = NA))
## now take another look
is.na(data$TotalCharges)
data$TotalCharges<-as.numeric(data$TotalCharges)
glimpse(data$TotalCharges)

## replace missing values with median

data<-data %>%
  mutate(TotalCharges = replace(TotalCharges,
                                list = is.na(TotalCharges),
                                values = median(TotalCharges, na.rm = TRUE)))
## now look for na in Payment Method

is.na(data$PaymentMethod)
glimpse(data$PaymentMethod)

## replace all missing data to be unavialble
data<-data %>%
  mutate(PaymentMethod = replace(PaymentMethod,
                                 list = PaymentMethod %in% c('--',''),
                                 values = 'unavailable'))


all(colSums(is.na(data)) == 0)
