# This function is specific to the Condition environment project, but could be useful if you need to 
# get a big block of corrleation/r2 or some other measure data.  This is used to get the data for the correlation figure used in the 
# Condition-Envionment paper collaboration with Emmanuel Devred and Catherine Johnson.


#####################################  Function Summary ########################################################
# ARGUMENTS

# dat:        The raw data you want to compare, it should be a time series, the orginal intent of the data was a monthly time series of some variable.  I called the variable
#             of interest "covar", so that's what you'll need to have in your data.  You also need a 'date' field with the date of the date of the observation
# cond.dat:   In this case this is the condition data, basically this could be any other data set, but for now it is set to pull out the "Condition"  
#             variable, could easily be generatlized to suit some other purpose.
#year         Let's you control the year range you want
#n.months     How many months do you want to go back?  Default = 16
#last.month   What is the most recent month you want to pull from?  Put in as the numeric month so we can back out how many years in the past you want to go

# Here's a function to get all the correlation and r2 value
cor_dat <- function(dat = sst.raw.dat,response = aug.dat,year = 1998:2015,n.months=18,last.month = 4)
{
 
old.month <- n.months - last.month
years.lost <- ceiling(old.month/12)
str.names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# The first month of the data we want, numerical
first.month <- 12-(old.month - 12*(years.lost-1)-1)
num.years <- length(year)
# We know the years we can use in the analysis based on what we trimmed off in the call, if we are going
# back 3 years with our covariate that means the first year of our data is 3 years after the first reading.
if(years.lost > 0) response <- response[response$Year %in% year[-c(1:years.lost)],]

covars <- NULL
for(i in (years.lost+1):num.years)
{
  # First we subset to the data we need, easier said than done!
  # The years that we need to do the correlation bit...
  yoi <- (year[i] - years.lost):year[i]
  # Now subset our covariates to these X number of years.
  tmp1 <- dat[dat$Year %in% yoi,]
  # Now subset to the months within this data we'll need.
  drop.first <- which(tmp1$Year == min(yoi) & tmp1$Month < first.month)
  drop.last <- which(tmp1$Year == max(yoi) & tmp1$Month > last.month)
  tmp2 <- tmp1[-c(drop.first,drop.last),]
  # Need to make sure data is ordered by a date column!!!
  covars[[as.character(year[i])]] <- tmp2[order(tmp2$date),]
}
# So now we have the data we want subset into this covars object.
# Next we want to select our data for the analysis      
    
count <- 1 # start a counter
# Make the response object
resp <- data.frame(start = rep(NA,n.months*n.months),end = NA,
                   r2 = NA,p.r2 = NA,p.r2.lev = NA,
                   pearson=NA,p.pearson = NA,p.pear.lev = NA,
                   kendall=NA,p.kendall = NA,p.kend.lev=NA,
                   start.num = NA,end.num = NA)

for(i in n.months:1)
{
  for(j in n.months:1)
  {
    # Only do this for half the data.
    if(i <=j)
    {
      # First make a covarate time series from the previous covars object
      for(k in 1:length(covars)) 
      {
        
        tmp <- sum(covars[[k]]$covar[i:j],na.rm=T)
        start.month <- covars[[k]]$Month[i]
        end.month <- covars[[k]]$Month[j]
        if(k == 1) tmp.covar <- tmp
        if(k > 1) tmp.covar <- c(tmp.covar,tmp)
      }

      # Run the lm's for each
      lm.resp <- lm(response$Condition~tmp.covar)
      pearson <- cor.test(tmp.covar, response$Condition,use = "na.or.complete",method = "pearson")
      kendall <- cor.test(tmp.covar, response$Condition, use = "na.or.complete",method = "kendall")
      #lm.resp <- lm(tmp.covar~aug.tmp$Condition)
      # Now extract the model results we want.
      resp$r2[count] <- signif(summary(lm.resp)$r.squared,digits=2)
      resp$p.r2[count] <- signif(coefficients(summary(lm.resp))[2,4],digits=2)
      if(resp$p.r2[count] < 0.05) resp$p.r2.lev[count] <- "*"
      if(resp$p.r2[count] < 0.01) resp$p.r2.lev[count] <- "**"
      if(resp$p.r2[count] < 0.001) resp$p.r2.lev[count] <- "***"
      # Now pearson results
      resp$pearson[count] <- signif(pearson$estimate,digits=2)
      resp$p.pearson[count] <- signif(pearson$p.value,digits=2)
      if(resp$p.pearson[count] < 0.05) resp$p.pear.lev[count] <- "*"
      if(resp$p.pearson[count] < 0.01) resp$p.pear.lev[count] <- "**"
      if(resp$p.pearson[count] < 0.001) resp$p.pear.lev[count] <- "***"
      # Finally Kendall
      # Now pearson results
      resp$kendall[count] <- signif(kendall$estimate,digits=2)
      resp$p.kendall[count] <- signif(kendall$p.value,digits=2)
      if(resp$p.kendall[count] < 0.05) resp$p.kend.lev[count] <- "*"
      if(resp$p.kendall[count] < 0.01) resp$p.kend.lev[count] <- "**"
      if(resp$p.kendall[count] < 0.001) resp$p.kend.lev[count] <- "***"
      
      # Figure out what month it is...
      resp$start[count] <- paste(str.names[start.month],"start")
      resp$end[count] <- paste(str.names[end.month],"end")
      resp$start.num[count] <- i
      resp$end.num[count] <- j
      count <- count +1  
      
      
    } # end (if i <= j)
  } # end for(j in 1:n.months)
} # end for(i in 1:n.months)
    resp <- resp[which(!is.na(resp$start)),]
    
  return(list(resp = resp))
} #end function
