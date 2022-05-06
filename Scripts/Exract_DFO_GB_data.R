# This script grabs the latest GB SST data from the datashop, fits the Condition-SST relationship to this data up to 2018
# and then plots the most recent data onto the figure.

#####################################################################################################

library(tidyverse)
library(data.table)
library(MASS)
library(readxl)
library(bbmle)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(mgcv)
library(lubridate)
library(pander)
library(scales)
library(R.matlab)
library(cowplot)
library(visreg)

#####################################################################################################
#Import Files from shared drive
#####################################################################################################

GB_scallops_sst_file <- "D:/Github/Paper_2_Cond_Env/Data/Georges_Bank_scallop_sst_May_3_2022.stat"

#setwd("Y:/Projects/Condition_Environment/Data/")
setwd("Y:/Projects/Condition_Environment/")
# directory you have the survey results stored in...
direct <- "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/"
#direct <- "E:/R/Data/Survey_data/2018/Survey_summary_output/"

# Looking a relationship between Condition and SST and phytoplankton


# Here we load in the data and get everything ready for further analyses.
#----------------------------------------------------------------------------------

# Scallop condition data
dat.cf <- read.csv("D:/Github/Paper_2_Cond_Env/Data/Full_may_aug_condition_ts.csv")

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

sst_scallop_data <- read.table(GB_scallops_sst_file, sep = "" , header = TRUE, na.strings ="")
sst_scallop_data$weight <- 0
sst_scallop_data$year <- substr(sst_scallop_data$date.id, 1, 4)
sst_scallop_data$month <- substr(sst_scallop_data$date.id, 5, 7)
sst_scallop_data$monthID <- substr(sst_scallop_data$date.id, 1, 7)
sst_scallop_data$identifier <- substr(sst_scallop_data$date.id, 8,8)

#####################################################################################################
#Everything Good?
#####################################################################################################

head(sst_scallop_data)

#####################################################################################################
#use the 'percent_coverage' column to calculate a weighted average
#iterates through every row, if the row and the row below share a year and a month they will receive weights
#calculated using the percent coverage column"


#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

for (i in seq(1, nrow(sst_scallop_data)-1, 1)){
  j <- i + 1
  if (substr(sst_scallop_data$date.id[i], 1, 7) == substr(sst_scallop_data$date.id[j], 1, 7)) {
    sst_scallop_data$weight[i] <- sst_scallop_data$X.coverage[i] / (sst_scallop_data$X.coverage[i] + sst_scallop_data$X.coverage[j])
    sst_scallop_data$weight[j] <- sst_scallop_data$X.coverage[j] / (sst_scallop_data$X.coverage[i] + sst_scallop_data$X.coverage[j])
  }
}

#sst_data$weighted_mean = weighted.mean(sst_data$mean_sst, sst_data$weight)
#head(sst_data)

#####################################################################################################
#Calculate all the weights for each variable for each observation

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

sst_scallop_data_summary <- sst_scallop_data %>% 
                                            group_by(monthID) %>% 
                                            mutate(GBS_weighted_mean = weighted.mean(mean_sst, weight)) %>%
                                            mutate(GBS_weighted_median = weighted.mean(median_sst, weight)) %>%
                                            mutate(GBS_weighted_min = weighted.mean(min_sst, weight)) %>%
                                            mutate(GBS_weighted_max = weighted.mean(max_sst, weight)) %>%
                                            mutate(GBS_weighted_Std_dev = weighted.mean(stdev_sst, weight))

#####################################################################################################
#Everything Good?
#####################################################################################################

head(sst_scallop_data_summary)

#####################################################################################################
#Remove Duplicates and get rid of non-essential columns for both data frames

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

no_duplicates_GBS <- sst_scallop_data_summary[!duplicated(sst_scallop_data_summary$monthID),]
keeps <- c("GBS_weighted_mean", "GBS_weighted_median", "GBS_weighted_min", "GBS_weighted_max", "GBS_weighted_Std_dev", "monthID")
All_weights <- no_duplicates_GBS[keeps]


#####################################################################################################
#Convert Month ID to date class (only works if you add the day).
#order by date
#####################################################################################################

All_weights$date <- as.Date(paste(All_weights$monthID,"1"), "%Y%b%d")
final.dat <- All_weights[order(All_weights$monthID),]
final.dat$year <- year(final.dat$date)
final.dat$month <- month(final.dat$date)
#####################################################################################################
#Everything Good?
#####################################################################################################

head(final.dat)
final.dat$date
write.csv(final.dat, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_sst_ts.csv")

# For SST the paper says we use Jan to March from last year and Jan-Feb from this year.
# But my someday paper says Jan to April from last year and Jan to March from this year
# Seems to work better with the original SST paper months and using the median not the means so going for that :-)
SST.last <- aggregate(cbind(GBS_weighted_median,GBS_weighted_mean) ~ year, data=final.dat[final.dat$month %in% 1:3,], FUN = sum)
names(SST.last) <- c("year","SST.last.median","SST.last.mean")

dat.tmp <- aggregate(cbind(GBS_weighted_median,GBS_weighted_mean) ~ year, data=final.dat[final.dat$month %in% 1:2,], FUN = sum)
names(dat.tmp) <-  c("year","SST.cur.median","SST.cur.mean")
dat <- cbind(dat.tmp,as.data.frame(rbind(NA,SST.last[-nrow(SST.last),names(SST.last) != "year"])))

# Make this SST.sum piece which makes our model simple below
dat$SST.sum.median <- dat$SST.cur.median + dat$SST.last.median
dat$SST.sum.mean <- dat$SST.cur.mean + dat$SST.last.mean
#dat$SST.sum.median <-  dat$SST.last.median



# Merge in the condition data
dat <- left_join(dat,dat.cf,by='year')
write.csv(dat, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_may_aug_sst_and_condition_time_series.csv")

# now a simple model, see how these work out!  
aug.mod.mn <- lm(aug~SST.sum.mean,data=dat %>% dplyr::filter(year < 2018))
summary(aug.mod.mn)
aug.mod.med <- lm(aug~ SST.sum.median,data=dat %>% dplyr::filter(year < 2018))
summary(aug.mod.med)

# now a simple model, see how these work out!  
may.mod.mn <- lm(may~SST.sum.mean ,data=dat %>% dplyr::filter(year < 2018))
summary(may.mod.mn)
may.mod.med <- lm(may~SST.sum.median,data=dat %>% dplyr::filter(year < 2018))
summary(may.mod.med)

# Now plot this shit...

ggplot(dat %>% dplyr::filter(year < 2018),aes(y=may,x=SST.sum.median)) + 
                                          geom_point() + geom_smooth(method='lm',color='red',linetype='dashed') + 
                                          geom_text(data=dat %>% dplyr::filter(year >= 2018),aes(y=may,x=SST.sum.median,label = substr(year,3,4)),color='blue') +
                                          geom_vline(xintercept = dat$SST.sum.median[dat$year == 2022],size=1.5,alpha=0.2) +
                                          theme_bw()+ xlab("SST Last + SST Current (°C)") + ylab("Condition (May)")


#####################################################################################################
#End script
#####################################################################################################