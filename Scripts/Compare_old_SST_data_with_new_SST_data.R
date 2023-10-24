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
GB_scallops_new_sst_file <- "D:/Github/Paper_2_Cond_Env/Data/GBscallopWeekly_Oct_2023.dat"

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

sst_old <- read.table(GB_scallops_sst_file, sep = "" , header = TRUE, na.strings ="")
sst_old$weight <- 0
sst_old$year <- substr(sst_old$date.id, 1, 4)
sst_old$month <- substr(sst_old$date.id, 5, 7)
sst_old$monthID <- substr(sst_old$date.id, 1, 7)
sst_old$identifier <- substr(sst_old$date.id, 8,8)

sst_new <- read.table(GB_scallops_new_sst_file, sep = "" , header = TRUE, na.strings ="")
names(sst_new) <- c("year",'nothing','start_date','end_date','sst',"sd_sst","pixels")
sst_new$sst[sst_new$sst == 9999] <- NA
sst_new$sd_sst [sst_new$sd_sst  == 9999] <- NA
sst_new$weight <- 100*(sst_new$pixels/max(sst_new$pixels))
sst_new$start_date <- as_date(sst_new$start_date)
sst_new$end_date <- as_date(sst_new$end_date)
sst_new$mid_date <- sst_new$end_date-3
sst_new$month <- tolower(lubridate::month(sst_new$mid_date,label=T,abbr=T))
sst_new$monthID <- paste0(sst_new$year,sst_new$month)



#####################################################################################################
#Everything Good?
#####################################################################################################

head(sst_old)

#####################################################################################################
#use the 'percent_coverage' column to calculate a weighted average
#iterates through every row, if the row and the row below share a year and a month they will receive weights
#calculated using the percent coverage column"


#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

for (i in seq(1, nrow(sst_old)-1, 1)){
  j <- i + 1
  if (substr(sst_old$date.id[i], 1, 7) == substr(sst_old$date.id[j], 1, 7)) {
    sst_old$weight[i] <- sst_old$X.coverage[i] / (sst_old$X.coverage[i] + sst_old$X.coverage[j])
    sst_old$weight[j] <- sst_old$X.coverage[j] / (sst_old$X.coverage[i] + sst_old$X.coverage[j])
  }
}

#sst_data$weighted_mean = weighted.mean(sst_data$mean_sst, sst_data$weight)
#head(sst_data)

#####################################################################################################
#Calculate all the weights for each variable for each observation

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

sst_old_summary <- sst_old %>% group_by(monthID) %>% 
                               mutate(GBS_weighted_mean = weighted.mean(mean_sst, weight)) %>%
                               mutate(GBS_weighted_median = weighted.mean(median_sst, weight)) %>%
                               mutate(GBS_weighted_min = weighted.mean(min_sst, weight)) %>%
                               mutate(GBS_weighted_max = weighted.mean(max_sst, weight)) %>%
                               mutate(GBS_weighted_Std_dev = weighted.mean(stdev_sst, weight))


sst_new_summary <- sst_new %>% group_by(monthID) %>% 
                               mutate(w_mean_new = weighted.mean(sst, weight)) %>%
                               mutate(w_sd_new = weighted.mean(sd_sst, weight))


#####################################################################################################
#Everything Good?
#####################################################################################################

head(sst_old_summary)

#####################################################################################################
#Remove Duplicates and get rid of non-essential columns for both data frames

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

no_duplicates_GBS <- sst_old_summary[!duplicated(sst_old_summary$monthID),]
keeps <- c("GBS_weighted_mean", "GBS_weighted_median", "GBS_weighted_min", "GBS_weighted_max", "GBS_weighted_Std_dev", "monthID")
All_weights_old <- no_duplicates_GBS[keeps]

no_duplicates_GBS <- sst_new_summary[!duplicated(sst_new_summary$monthID),]
keeps <- c("w_mean_new", "w_sd_new", "monthID")
All_weights_new <- no_duplicates_GBS[keeps]

# Now to compare the two time series, they are very very similar.

sst_comparison <- left_join(All_weights_new,All_weights_old,by = "monthID")

corel <- cor.test(sst_comparison$w_mean_new , sst_comparison$GBS_weighted_mean)
corel

lm.res <- lm(GBS_weighted_mean~w_mean_new-1,data=sst_comparison)
summary(lm.res)

windows(11,11)
ggplot(data= sst_comparison) + geom_point(aes(w_mean_new,y=GBS_weighted_mean)) + 
                               geom_abline(slope=1,intercept=0) + xlab("New SST Data") + ylab("Old SST Data")



#####################################################################################################
#Convert Month ID to date class (only works if you add the day).
#order by date
#####################################################################################################

All_weights_old$date <- as.Date(paste(All_weights_old$monthID,"1"), "%Y%b%d")
final.dat <- All_weights_old[order(All_weights_old$monthID),]
final.dat$year <- year(final.dat$date)
final.dat$month <- month(final.dat$date)

# New data...
All_weights_new$date <- as.Date(paste(All_weights_new$monthID,"1"), "%Y%b%d")
final.dat.new <- All_weights_new[order(All_weights_new$monthID),]
final.dat.new$year <- year(final.dat.new$date)
final.dat.new$month <- month(final.dat.new$date)


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

SST.last.new <- aggregate(w_mean_new ~ year, data=final.dat.new[final.dat.new$month %in% 1:3,], FUN = sum)
names(SST.last.new) <- c("year","SST.last")

dat.tmp <- aggregate(cbind(GBS_weighted_median,GBS_weighted_mean) ~ year, data=final.dat[final.dat$month %in% 1:2,], FUN = sum)
names(dat.tmp) <-  c("year","SST.cur.median","SST.cur.mean")
dat <- cbind(dat.tmp,as.data.frame(rbind(NA,SST.last[-nrow(SST.last),names(SST.last) != "year"])))

# Make this SST.sum piece which makes our model simple below
dat$SST.sum.median <- dat$SST.cur.median + dat$SST.last.median
dat$SST.sum.mean <- dat$SST.cur.mean + dat$SST.last.mean
#dat$SST.sum.median <-  dat$SST.last.median

# The new data...
dat.new <- aggregate(w_mean_new ~ year, data=final.dat.new[final.dat.new$month %in% 1:2,], FUN = sum)
names(dat.new) <-  c("year","SST_cur")
dat.new$SST_last <- c(NA,SST.last.new[-nrow(SST.last.new),names(SST.last.new) != "year"])

# Make this SST.sum piece which makes our model simple below
dat.new$SST.sum <- dat.new$SST_cur + dat.new$SST_last
#dat$SST.sum.median <-  dat$SST.last.median


# Merge in the condition data
dat <- left_join(dat,dat.cf,by='year')
dat.new <- left_join(dat.new,dat.cf)
write.csv(dat, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_may_aug_sst_and_condition_time_series.csv")

# now a simple model, see how these work out!  
aug.mod.mn <- lm(aug~SST.sum.mean,data=dat %>% dplyr::filter(year < 2018))
summary(aug.mod.mn)
aug.mod.med <- lm(aug~ SST.sum.median,data=dat %>% dplyr::filter(year < 2018))
summary(aug.mod.med)

# Let's compare models for new years
aug.mod.new <- lm(aug ~ SST_last+SST_cur,data= dat.new)
summary(aug.mod.new)
# May model
may.mod.new <- lm(may ~ SST_last+SST_cur,data= dat.new)
summary(may.mod.new)
# Now subset the old data to the years we have new data for...
aug.mod.mn.comp <- lm(aug~SST.sum.mean,data=dat %>% dplyr::filter(year >2007))
summary(aug.mod.mn.comp)
# and for May
may.mod.comp <- lm(may ~ SST.sum.mean,data=dat %>% dplyr::filter(year >2007))
summary(may.mod.comp)


# now a simple model, see how these work out!  
may.mod.mn <- lm(may~SST.sum.mean ,data=dat %>% dplyr::filter(year < 2018))
summary(may.mod.mn)
may.mod.med <- lm(may~SST.sum.median,data=dat %>% dplyr::filter(year < 2018))
summary(may.mod.med)

# The better full model 
may.mod.2 <- lm(may~SST.cur.median + SST.last.median ,data=dat %>% dplyr::filter(year < 2023))
summary(may.mod.2)

# Now we can predict the condition for the summer of the most recent year, in this case 2022...
predict(may.mod.med,newdata = data.frame(SST.sum.median=  dat$SST.sum.median[dat$year == 2022]))
# How high would 2023 have to be to get to the observed value, since SST current is 
predict(may.mod.2,newdata = data.frame(SST.last.median = 20.68,SST.cur.median =  15.51))
# How about the new model...
predict(aug.mod.new,newdata = data.frame(SST_last = 20.68,SST_cur =  15.51))
predict(may.mod.new,newdata = data.frame(SST_last = 20.68,SST_cur =  15.51))

# Now plot this shit...



ggplot(dat %>% dplyr::filter(year < 2018),aes(y=may,x=SST.sum.median)) + 
                                          geom_point() + geom_smooth(method='lm',color='red',linetype='dashed') + 
                                          geom_text(data=dat %>% dplyr::filter(year >= 2018),aes(y=may,x=SST.sum.median,label = substr(year,3,4)),color='blue') +
                                          geom_vline(xintercept = dat$SST.sum.median[dat$year == 2022],size=1.5,alpha=0.2) +
                                          theme_bw()+ xlab("SST Last + SST Current (?C)") + ylab("Condition (May)")


#####################################################################################################
#End script
#####################################################################################################