library(dplyr)
library(tidyr)
library(data.table)

anesRawMap <- read.csv("https://raw.githubusercontent.com/aagoldberg/AnesFinal/master/anes_timeseries_2016.csv")
variables <- c("V161010d", "V161010f", "V162034a", "V162037a", "V162035", "V162098", "V162103", "V162106", "V162125x", "V162139", "V162140", "V162158", "V162185", "V162255x")
anes_data_map <- anesRawMap[, variables]
anes_data_map[anes_data_map < 0] <- NA

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
anes_data_map <- anes_data_map[-c(3:4)]
colnames(anes_data_map) <- c("state", "cd", "therm_feminists", "therm_unions", "therm_lgbt", "therm_muslims", "flag_pride", "reducing_debt", "millionaires_tax", "immigration_hurts_jobs", "government_size", "obama_muslim", "total_pres")

str(anes_data_map)
summary(anes_data_map)

library(mice)
library(VIM)
aggr(anes_data_map, bars=F, sortVars=T)
MICE <- mice(anes_data_map, predictorMatrix = quickpred(anes_data_map), method = "mean", printFlag = F)
anes_data_map <- mice::complete(MICE, action = 1)

anes_map_pres <- anes_data_map %>%
  group_by(state, cd) %>%
  count(count = total_pres) %>%
  spread(count, n) %>%
  rename(HRC=`1`, DJT=`2`, OTH = `3`) %>%
  replace(is.na(.), 0) %>%
  mutate(HRCper = round((HRC/(HRC+DJT+OTH)),1)) %>%
  mutate(DJTper = round((DJT/(HRC+DJT+OTH)),1)) %>%
  mutate(state_cd = paste(state, cd, sep='_'))

anes_map_rest <- anes_data_map %>%
  group_by(state, cd) %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(mean)) 


anes_map_comb <- merge(x = anes_map_pres, y = anes_map_rest, by = "state_cd", all.x = TRUE)

drops <- c("state.y", "cd.y", "HRC", "DJT", "OTH")
anes_map <- anes_map_comb[ , -which(names(anes_map_comb) %in% drops)]
anes_map <- rename(anes_map, state= state.x, cd = cd.x)

str(anes_map)
head(anes_map_comb)

#combining data to test differences in state_cd
library(foreign)
library(dplyr)
library(stringr)
library(withr)
library(devtools)

#anes_dbf <- read.dbf("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m/cb_2016_us_cd115_5m.dbf")
#anes_csv <- read.csv("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m/PresResDat.csv")

#states with only one CD
anes_map["cd"][anes_map$state == "2",] <- 0
anes_map["cd"][anes_map$state == "10",] <- 0
anes_map["cd"][anes_map$state == "30",] <- 0
anes_map["cd"][anes_map$state == "38",] <- 0
anes_map["cd"][anes_map$state == "46",] <- 0
anes_map["cd"][anes_map$state == "50",] <- 0
anes_map["cd"][anes_map$state == "56",] <- 0
anes_map["cd"][anes_map$state == "11",] <- 98

anes_map <- anes_map %>%
  mutate(cd = as.factor(formatC(cd, width = 2, format = "d", flag = "0"))) %>%
  mutate(state = as.factor(formatC(state, width=2, format = "d", flag = "0"))) %>%
  mutate(state_cd = paste(state, cd, sep='_'))   %>%
  mutate_each(funs(round(.,1)), therm_feminists, therm_unions, therm_lgbt, therm_muslims, flag_pride, reducing_debt, millionaires_tax, immigration_hurts_jobs, government_size, obama_muslim, total_pres)
  
#now join data to dbf
anes_dbf <- read.dbf("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m/cb_2016_us_cd115_5m.dbf")
#anes_dbf <- read.dbf("https://github.com/aagoldberg/aagoldberg.github.io/blob/master/cb_2016_us_cd115_5m.dbf")
str(anes_dbf)
str(anes_map)


anes_dbf$state_cd <- as.factor(paste(anes_dbf$STATEFP, anes_dbf$CD115FP, sep='_'))
anes_dbf_merged <- merge(anes_dbf, anes_map, by="state_cd", all = TRUE)
str(anes_dbf_merged)
anes_dbf_merged[anes_dbf_merged == NA] == NA

anes_dbf_merged[,-13:-1] <- lapply(anes_dbf_merged[,-13:-1], factor)
setwd("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m")
write.dbf(anes_dbf_merged, "cb_2016_us_cd115_5m")

str(anes_dbf)

getOption("max.print") <- 2000

#checking joins
str(anes_csv_dplyr)
anes_csv$cd_sp <- as.factor(formatC(anes_csv$CD, width = 2, format = "d", flag = "0"))
summary(anes_csv)
anes_comb <- left_join(anes_dbf_dplyr, anes_csv_dplyr, by = "state_cd")
summary(anes_comb)




anes_csv2 <- anes_csv_dplyr[,c(9,8,6,7)]
colnames(anes_csv2) <- c("State", "CD", "HRCper", "DJTper")

setwd("/Users/andrew/Documents/School/AnesFinal/cb_2016_us_cd115_5m")
write.csv(anes_csv2, file = "PresResDat2.csv")
setwd("/Users/andrew/Documents/School/AnesFinal/aagoldberg.github.io")
write.csv(anes_csv2, file = "PresResDat3.csv")
#/Users/andrew/Documents/School/AnesFinal/aagoldberg.github.io/index.html



getwd()
nrow(anes_csv)
nrow(anes_csv2)
summary(anes_csv)
summary(anes_csv2)
summary(anes_dbf_dplyr)

anes_csv2[anes_csv2$State == "50",]
