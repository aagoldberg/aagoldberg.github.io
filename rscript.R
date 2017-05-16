library(dplyr)
library(tidyr)
library(data.table)
library(foreign)
library(stringr)
library(withr)
library(devtools)
library(mice)
library(VIM)

#Loading to data from github
anesRawMap <- read.csv("https://raw.githubusercontent.com/aagoldberg/AnesFinal/master/anes_timeseries_2016.csv")
variables <- c("V161010d", "V161010f", "V162034a", "V162037a", "V162035", "V162098", "V162103", "V162106", "V162125x", "V162139", "V162158", "V162185", "V162255x")
anes_data_map <- anesRawMap[, variables]

#clean up survey data and organize survey data

###Combine presidential vote and preference (from those who didn't vote) to provide larger sample size
anes_data_map$V999999[anes_data_map$V162034a==1] <- 1
anes_data_map$V999999[anes_data_map$V162034a==2] <- 2
anes_data_map$V999999[anes_data_map$V162034a==3] <- 3
anes_data_map$V999999[anes_data_map$V162034a==4] <- 3
anes_data_map$V999999[anes_data_map$V162034a==5] <- 3

anes_data_map$V999999[anes_data_map$V162037a==1] <- 1
anes_data_map$V999999[anes_data_map$V162034a==2] <- 2
anes_data_map$V999999[anes_data_map$V162037a==3] <- 3
anes_data_map$V999999[anes_data_map$V162034a==4] <- 3
anes_data_map$V999999[anes_data_map$V162037a==5] <- 3
table(anes_data_map$V999999, useNA = "ifany")

anes_data_map <- subset(anes_data_map, !is.na(anes_data_map$V999999))
anes_data_map <- anes_data_map[-c(3:5)]

#add column names
colnames(anes_data_map) <- c("state", "cd", "therm_unions", "therm_lgbt", "therm_muslims", "flag_pride", "reducing_debt", "immigration_hurts_jobs", "government_size", "obama_muslim", "total_pres")

###Remove non-response codes
anes_data_map[anes_data_map < 0] <- NA
anes_data_map[, 3:5][anes_data_map[, 3:5] > 100] <- NA # Make invalid therm values NA

#impute data for non-response data
aggr(anes_data_map, bars=F, sortVars=T)
MICE <- mice(anes_data_map, predictorMatrix = quickpred(anes_data_map), method = "mean", printFlag = F)
anes_data_map <- mice::complete(MICE, action = 1)

#create percentages for Clinton/Trump performance
anes_map_pres <- anes_data_map %>%
  group_by(state, cd) %>%
  count(count = total_pres) %>%
  spread(count, n) %>%
  rename(HRC=`1`, DJT=`2`, OTH = `3`) %>%
  replace(is.na(.), 0) %>%
  mutate(HRCper = round((HRC/(HRC+DJT+OTH)),1)) %>%
  mutate(DJTper = round((DJT/(HRC+DJT+OTH)),1)) %>%
  mutate(state_cd = paste(state, cd, sep='_'))

#adjust other variables to the sample 0 to 1 percent scale used in the presidential performance variable
anes_map_rest <- anes_data_map %>%
  group_by(state, cd) %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(mean)) %>%
  mutate(therm_unions = therm_unions/max(therm_unions)) %>%
  mutate(therm_lgbt = therm_lgbt/max(therm_lgbt)) %>%
  mutate(therm_muslims = therm_muslims/max(therm_muslims)) %>%
  mutate(flag_pride = flag_pride/max(flag_pride)) %>%
  mutate(reducing_debt = reducing_debt/max(reducing_debt)) %>%
  mutate(immigration_hurts_jobs = immigration_hurts_jobs/max(immigration_hurts_jobs)) %>%
  mutate(government_size = government_size/max(government_size)) %>%
  mutate(obama_muslim = obama_muslim/max(obama_muslim)) %>%
  mutate(total_pres = total_pres/max(total_pres)) %>%
  mutate(state_cd = paste(state, cd, sep='_'))

#combine them back and remove duplicate columns
anes_map_comb <- merge(x = anes_map_pres, y = anes_map_rest, by = "state_cd", all.x = TRUE)
drops <- c("state.y", "cd.y", "HRC", "DJT", "OTH")
anes_map <- anes_map_comb[ , -which(names(anes_map_comb) %in% drops)]
anes_map <- rename(anes_map, state= state.x, cd = cd.x)

str(anes_map)
head(anes_map_comb)

#preparing data to match and add to the .dbf of the .shp mapping file:

#survey data labels congressional districts with "1" for states with only 1 district. The dbf uses "0".
anes_map["cd"][anes_map$state == "2",] <- 0
anes_map["cd"][anes_map$state == "10",] <- 0
anes_map["cd"][anes_map$state == "30",] <- 0
anes_map["cd"][anes_map$state == "38",] <- 0
anes_map["cd"][anes_map$state == "46",] <- 0
anes_map["cd"][anes_map$state == "50",] <- 0
anes_map["cd"][anes_map$state == "56",] <- 0
anes_map["cd"][anes_map$state == "11",] <- 98

#Add 0 padding to columns needed to match with the .dbf
anes_map <- anes_map %>%
  mutate(cd = as.factor(formatC(cd, width = 2, format = "d", flag = "0"))) %>%
  mutate(state = as.factor(formatC(state, width=2, format = "d", flag = "0"))) %>%
  mutate(state_cd = paste(state, cd, sep='_'))   %>%
  mutate_each(funs(round(.,1)), therm_unions, therm_lgbt, therm_muslims, flag_pride, reducing_debt, immigration_hurts_jobs, government_size, obama_muslim, total_pres)
  
#import .dbf
anes_dbf <- read.dbf("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m1/cb_2016_us_cd115_5m.dbf")
str(anes_dbf_merged)
str(anes_map)

#join survey data to the .dbf using a "state_cd" key
anes_dbf$state_cd <- as.factor(paste(anes_dbf$STATEFP, anes_dbf$CD115FP, sep='_'))
anes_dbf_merged <- merge(anes_dbf, anes_map, by="state_cd", all = TRUE)

#export the new data as a .dbf
setwd("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m")
setwd("/Users/andrew/Documents/School/AnesFinal/aagoldberg.github.io")
write.dbf(anes_dbf_merged, "cb_2016_us_cd115_5mV7")





