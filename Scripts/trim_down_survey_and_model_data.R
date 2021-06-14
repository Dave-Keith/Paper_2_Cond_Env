library(tidyverse)
library(readxl)
library(cowplot)
library(ggthemes)
direct.proj <- "D:/Github/Paper_2_Cond_Env/"
funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/projections.r",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R"
)
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

# Load the survey data 
#load("E:local_backup/NAS/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")  
#vonB <- read.csv("Y:/Offshore/Assessment/Data/Ageing/Von_B_growth_parameters.csv")

#saveRDS(survey.obj,paste0(direct.proj,"Data/surve_obj.RDS"))
#saveRDS(vonB,paste0(direct.proj,"Data/vonb_dat.RDS"))
# Here's all the data needed to do the below. Indeed some of this is created with the below code, but that's been commented out...
dat <- read_xlsx(paste0(direct.proj,"Data/GB_satellite_1985_2019.xlsx"))
gb.proj.catch <- readRDS(paste0(direct.proj,"Data/gb_proj_catch.RDS"))
gb.fish.dat <- readRDS(paste0(direct.proj,"Data/gb_fish_dat.RDS"))
DD.out <- readRDS(paste0(direct.proj,"Data/gb_model_output.RDS"))
sh.dat <- readRDS(paste0(direct.proj,"Data/sh_dat.RDS"))
survey.obj <- readRDS(paste0(direct.proj,"Data/survey_obj.RDS"))
vonB <- readRDS(paste0(direct.proj,"Data/vonb_dat.RDS"))
dat <- read_xlsx(paste0(direct.proj,"Data/GB_satellite_1985_2019.xlsx"))
cond.dat <- read.csv(paste0(direct.proj,"Data/CF_data_May_GB_for_revisions_1986_2019.csv"))

# We can't use the August condition because the relationship starts to break down.  If we need to discuss this 
# I think the answer is that spawning starts in August and it is after the peak of the fishery, so we use May data
# We're gonna have to make the case that we want to use May data for the models going forward.  That
# does at least make the case for a solid spring survey that models the same stations over time and the
# importance of the MW-SH modelling in the spring.  Make the spring survey useful...
# cond.aug <- data.frame(year = survey.obj$GBa[[1]]$year, CF=survey.obj$GBa[[1]]$CF)
# 
# sh.dat <- data.frame(year = survey.obj$GBa[[1]]$year,l.fr = survey.obj$GBa[[1]]$l.bar,l.r = survey.obj$GBa[[1]]$l.k)
# vonB <- vonB %>% dplyr::filter(Bank == "GBa")

#saveRDS(sh.dat,paste0(direct.proj,"Data/sh_dat.RDS"))


#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!
#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!
# But for 1989 and 2015 we do have august condition data, we will use those data for those 2 years.  If
# they end up being weird then perhaps we just leave them out, but we can decide that later
# So we know Aug condition is generally higher than in May, what's that relationship look like?
cond.aug <- data.frame(year = survey.obj$GBa[[1]]$year, CF=survey.obj$GBa[[1]]$CF)
cond.aug  <- cond.aug %>% dplyr::filter(year >= 1986)
cond.all <- left_join(cond.aug,cond.dat,by = 'year')
names(cond.all) <- c("year","Aug","May")
may.aug.mod <- lm(May~Aug,cond.all)
summary(may.aug.mod)

Aug = cond.aug %>% dplyr::filter(year %in% c(1989,2015)) %>% dplyr::select(CF)
pred.dat <- data.frame(year = c(1989,2015), 
                       Aug = Aug$CF,
                       May = c(NA,NA))
pred.dat$May <- predict(may.aug.mod,pred.dat)

cond.all$May[cond.all$year %in% c(1989,2015)] <- pred.dat$May
cond.may <- cond.all[,names(cond.all) != "Aug"]
names(cond.may) <- c("year","CF")
acf(cond.all$Aug,plot=F)
#cond.all.long <- reshape2::melt(cond.all,id = 'year')

# So data shows this is very much just an offset, slope is 1:1, but August tends to be below May
ggplot(cond.all,aes(y = May, x = Aug)) + geom_point() + geom_abline() + geom_smooth(method = 'lm')
#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!
#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!#### IMPORANT NOTE DK!!!!



last.sst <- dat %>% dplyr::filter(Month %in% 1:3) %>% group_by(Year) %>% dplyr::summarise(sst.last = mean(SST,na.rm=T))
# SO this will align the last sst with next year.
last.sst$Year <- last.sst$Year + 1
# And the current year uses Jan and Feb.
this.sst <- dat %>% dplyr::filter(Month %in% 1:2) %>% group_by(Year) %>% dplyr::summarise(sst.this = mean(SST,na.rm=T))

sst.dat <- left_join(this.sst,last.sst,by= "Year")
names(sst.dat) <- tolower(names(sst.dat))
all.dat <- left_join(sst.dat,cond.may,by='year')

mod.all <- lm(CF~ sst.this + sst.last,data = all.dat)
summary(mod.all)

#Based on the paper I'm kinda leaning towards just using the last model to see how well it does...
mod.last <- lm(CF~ sst.last,data = all.dat)
summary(mod.last)

# Get our predicted CF and compare to the Predicted condition we actually used
all.dat$CF.all <- predict(mod.all,all.dat)
all.dat$CF.last <- predict(mod.last,all.dat)
all.dat$CF.used <- c(NA,NA,all.dat$CF[2:(nrow(all.dat)-1)])
# Now take the difference between the different predictive models and the actual condition observed
all.dat$diff.CF.all <- all.dat$CF.all - all.dat$CF
all.dat$diff.CF.last <- all.dat$CF.last - all.dat$CF
all.dat$diff.CF.used <- all.dat$CF.used - all.dat$CF
all.dat$improve.CF.last <- ifelse(abs(all.dat$diff.CF.last) < abs(all.dat$diff.CF.used),1,0)
# So the prediction is closer than our current method in 20 of 29 years with the pred.all.improve model.
# So there is solid evidence that this method is better than what we are doing now.  Now the question is, does it matter
all.dat$improve.CF.all <- ifelse(abs(all.dat$diff.CF.all) < abs(all.dat$diff.CF.used),1,0)
# Now what is the predictive error from the different methods overall, perhaps it is so small that we probably won't care.
all.dat$perc.diff.CF.all <- 100*(all.dat$diff.CF.all)/all.dat$CF
all.dat$perc.diff.CF.last <- 100*(all.dat$diff.CF.last)/all.dat$CF
all.dat$perc.diff.CF.used <- 100*(all.dat$diff.CF.used )/all.dat$CF


all.dat <- left_join(all.dat,sh.dat,by='year')


all.dat$waa.tm1 <- all.dat$CF*(all.dat$l.fr/100)^3

# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
all.dat$laa.t <- vonB$Linf*(1-exp(-vonB$K)) + exp(-vonB$K) * all.dat$l.fr
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
all.dat$waa.t.used <- c(all.dat$CF[-1],all.dat$CF[nrow(all.dat)])*(all.dat$laa.t/100)^3
all.dat$waa.t.all <- all.dat$CF.all*(all.dat$laa.t/100)^3
all.dat$waa.t.last <- all.dat$CF.last*(all.dat$laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.

all.dat$waa.t2 <- all.dat$CF*(all.dat$laa.t/100)^3 # This is the 'realized' weight next year, really just using the growth projection * CF for next year.
# Now the growth, expected and realized.
all.dat$g.used <- all.dat$waa.t.used/all.dat$waa.tm1 # This is the growth estimate that would have been used in the model
all.dat$g.all <- all.dat$waa.t.all/all.dat$waa.tm1 # This is the growth estimate we'd use if we used the full SST predictive model
all.dat$g.last <- all.dat$waa.t.last/all.dat$waa.tm1 # This is the growth estimate we'd use if we used the last year only SST predictive model
all.dat$g.actual <- all.dat$waa.t2/all.dat$waa.tm1 # This is using the actual condition factor and growing the scallops by laa.t
# Percent differences, using g.actual as our 'real' scenario
all.dat$prec.g.used <- 100*(all.dat$g.used - all.dat$g.actual) / all.dat$g.actual
all.dat$prec.g.all <- 100*(all.dat$g.all - all.dat$g.actual) / all.dat$g.actual
all.dat$prec.g.last <- 100*(all.dat$g.last - all.dat$g.actual) / all.dat$g.actual

summary(all.dat$prec.g.used)
summary(all.dat$prec.g.all)
summary(all.dat$prec.g.last)




# Now do the same thing for the recruits.
all.dat$wk.tm1 <- all.dat$CF*(all.dat$l.r/100)^3
all.dat$lk.t <- vonB$Linf*(1-exp(-vonB$K))+exp(-vonB$K)*all.dat$l.r

all.dat$wk.t.used <- c(all.dat$CF[-1],all.dat$CF[nrow(all.dat)])*(all.dat$lk.t/100)^3
all.dat$wk.t.all <- all.dat$CF.all*(all.dat$lk.t/100)^3
all.dat$wk.t.last <- all.dat$CF.last*(all.dat$lk.t/100)^3
all.dat$wk.t2 <- all.dat$CF*(all.dat$lk.t/100)^3

all.dat$gr.used <- all.dat$wk.t.used/all.dat$wk.tm1 # This is the growth estimate that would have been used in the model
all.dat$gr.all <- all.dat$wk.t.all/all.dat$wk.tm1 # This is the growth estimate we'd use if we used the full SST predictive model
all.dat$gr.last <- all.dat$wk.t.last/all.dat$wk.tm1 # This is the growth estimate we'd use if we used the last year only SST predictive model
all.dat$gr.actual <- all.dat$wk.t2/all.dat$wk.tm1 # This is using the actual condition factor and growing the scallops by laa.t
# Percent differences, using g.actual as our 'real' scenario
all.dat$prec.gr.used <- 100*(all.dat$gr.used - all.dat$gr.actual) / all.dat$gr.actual
all.dat$prec.gr.all <- 100*(all.dat$gr.all - all.dat$gr.actual) / all.dat$gr.actual
all.dat$prec.gr.last <- 100*(all.dat$gr.last - all.dat$gr.actual) / all.dat$gr.actual

summary(all.dat$prec.gr.used)
summary(all.dat$prec.gr.all)
summary(all.dat$prec.gr.last)

# Pivot to long form
all.dat.long <- all.dat %>% reshape2::melt(id.vars = 'year',value.name = 'response',variable.name = 'covar')


# Some plots...
# the str_detect is nice tidyverse alternative to grep I think!
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'prec.g.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'^g.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'^CF.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'prec.gr.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'^gr.'))) + geom_boxplot(aes(y = response,x = covar))



# Now load the Model data

#load("E:/local_backup/NAS/Offshore/Assessment/Data/Model/2020/GBa/Results/Final_model_results.RData")
#gb.proj.catch <- proj.dat
#gb.fish.dat <- cpue.dat
# 2020 landings were 4706 tonnes
#gb.fish.dat$GBa <- rbind(gb.fish.dat$GBa,c(2020,4706,0,0,0,0,0,0,0))
# Now save all the pieces I need so I don't have to load anything huge
# saveRDS(gb.proj.catch,"D:/Github/Paper_2_Cond_Env/Data/gb_proj_catch.RDS")
# saveRDS(gb.fish.dat,"D:/Github/Paper_2_Cond_Env/Data/gb_fish_dat.RDS")
# saveRDS(DD.out$GBa,"D:/Github/Paper_2_Cond_Env/Data/gb_model_output.RDS")
# 
# gb.proj.catch <- loadRDS("D:/Github/Paper_2_Cond_Env/Data/gb_proj_catch.RDS")
# gb.fish.dat <- loadRDS("D:/Github/Paper_2_Cond_Env/Data/gb_fish_dat.RDS")
# DD.out <- loadRDS("D:/Github/Paper_2_Cond_Env/Data/gb_model_output.RDS")


# For C.p, all I need are the actual landings for the following year I believe

