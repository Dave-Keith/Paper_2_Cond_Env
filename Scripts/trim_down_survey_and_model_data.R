# Load the survey data 
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")  
# Now figure out what to extract and save that to the our github repo
vonB <- read.csv("Y:/Offshore/Assessment/Data/Ageing/Von_B_growth_parameters.csv")

# We can't use the August condition because the relationship starts to break down.  If we need to discuss this 
# I think the answer is that spawning starts in August and it is after the peak of the fishery, so we use May data
# We're gonna have to make the case that we want to use May data for the models going forward.  That
# does at least make the case for a solid spring survey that models the same stations over time and the
# importance of the MW-SH modelling in the spring.  Make the spring survey useful...
#cond.aug <- data.frame(year = survey.obj$GBa[[1]]$year, CF=survey.obj$GBa[[1]]$CF)
SS.dat <- SS.summary$GBa
live.dat <- surv.Live$GBa
sh.dat <- data.frame(year = survey.obj$GBa[[1]]$year,l.fr = survey.obj$GBa[[1]]$l.bar,l.r = survey.obj$GBa[[1]]$l.k)
vonB <- vonB %>% dplyr::filter(Bank == "GBa")

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
all.dat$waa.t.all <- all.dat$CF.pred.all*(all.dat$laa.t/100)^3
all.dat$waa.t.last <- all.dat$CF.pred.last*(all.dat$laa.t/100)^3
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


all.dat.long <- all.dat %>% reshape2::melt(id.vars = 'year',value.name = 'response',variable.name = 'covar')

# the str_detect is nice tidyverse alternative to grep I think!
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'prec.g.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'^g.'))) + geom_boxplot(aes(y = response,x = covar))
ggplot(all.dat.long %>% dplyr::filter(str_detect(covar,'^CF.'))) + geom_boxplot(aes(y = response,x = covar))

# same thing here but for the recruits
waa.tm1 <- mod.dat$CF*(mod.dat$l.k/100)^3
laa.t <- vonB$Linf*(1-exp(-vonB$K))+exp(-vonB$K)*mod.dat$l.k
waa.t <- c(mod.dat$CF[-1],mod.dat$CF[nrow(mod.dat)])*(laa.t/100)^3
waa.t2 <- mod.dat$CF*(laa.t/100)^3
mod.dat$gR <- waa.t/waa.tm1
mod.dat$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")


# Some checking...

# Back to real code
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat[[bank[i]]]$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.

waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat[[bank[i]]]$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat[[bank[i]]]$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat[[bank[i]]]$l.k
waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
mod.dat[[bank[i]]]$gR <- waa.t/waa.tm1
mod.dat[[bank[i]]]$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")






# Now load the Model data


