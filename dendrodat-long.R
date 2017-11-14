#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)

#read-in 
dendrodat <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrometer-long.csv')
#summary(dendrodat)

#recoding timbersale so that N=0 and Y=1
#dendrodat$timbersale <- mapvalues(dendrodat$timbersale, from = c("N", "Y"), to = c("0", "1"))

#making numeric vars factors as needed
dendrodat$treeid <- factor(dendrodat$treeid)
dendrodat$spnum <- factor(dendrodat$spnum)
dendrodat$sitequal <- factor(dendrodat$sitequal)
#dendrodat$timbersale <- factor(dendrodat$timbersale)
#factor(dendrodat$treeid)
#factor(dendrodat$spnum)
#factor(dendrodat$sitequal)

###############################Variables:
#treeid [char]           = individual tree id 
#spnum [char]            = forestry species code 
#spname [char]           = tree common name 
#spcode [char]           = 4-letter USDA species code 
#site [char]             = FTIG site code (B6B, B10B, C03x [x added for consistency of variable length]) 
#aspect [char]           = south (S) or north (N) 
#sitequal [num]          = site quality (1: , 2: , 3: ) 
#timbersale [char]       = Y (thinned ) or N (unthinned) 
#dominance [char]        = dominant (D), co-dominant (C), or intermediate (I) 
#baselinedbh [num]       = DBH at date of first installation (in inches) 
#tension [char]          = lo, hi or na -- densiometer tension. only available for trees 1-24 (24-32: na) 
#year, month [num]       = year and month of measurement
#growth                  = growth in mm from previous measurement
#rain                    = total rainfall since last measurement
#temp                    = mean max temperature since last measurement

#exploratory
# table(dendrodat$spcode,dendrodat$site) #all sp occur in only 1 or 2 sites
# table(dendrodat$spcode,dendrodat$sitequal) #all but PIST occur in only 1 or 2 site quals
# table(dendrodat$site,dendrodat$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
# table(dendrodat$spcode,dendrodat$timbersale) #at least one of each species in each level of timbersale
# table(dendrodat$site,dendrodat$timbersale) #exactly even number of trees in each timbersale level per plot
# table(dendrodat$site,dendrodat$aspect) #B06B and C03x: N; B10B: S
# table(dendrodat$spcode,dendrodat$dominance) #most trees are co-dominant
# table(dendrodat$tension,dendrodat$baselinedbh) #low tension dendrometers were used below 21.1 in DBH

#problem: some numbers are not integers, problematic with poisson and negbin distributions.
# dendrodat$growth%%1==0 #integer check
#rounds up when there is a non-integer
###################2015
dendrodat$growth <- ceiling(dendrodat$growth)

###################2016
dendrodat$jul16grow <- ceiling(dendrodat$jul16grow)

###################2017
dendrodat$jun17grow <- ceiling(dendrodat$jun17grow); dendrodat$jul17grow <- ceiling(dendrodat$jul17grow)
dendrodat$aug17grow <- ceiling(dendrodat$aug17grow); dendrodat$oct17grow <- ceiling(dendrodat$oct17grow)
dendrodat$tot17grow <- ceiling(dendrodat$tot17grow); 

###################totals. these are done differently because not all are 0.5 (some should be rounded up, some down to get to nearest int)
dendrodat$maygrow<-round(dendrodat$maygrow,digits=1);dendrodat$jungrow<-round(dendrodat$jungrow,digits=1);dendrodat$julgrow<-round(dendrodat$julgrow,digits=1)
dendrodat$auggrow<-round(dendrodat$auggrow,digits=1);dendrodat$sepgrow<-round(dendrodat$sepgrow,digits=1);dendrodat$octgrow<-round(dendrodat$octgrow,digits=1)

dendrodat2 <- dendrodat #new dataframe
#rounding to nearest integer. ##########PROBLEM: 0.3 rounds down to 0.
dendrodat2$maygrow <- ifelse(dendrodat$maygrow%%1 >= 0.5,dendrodat2$maygrow <- ceiling(dendrodat$maygrow), dendrodat2$maygrow <- floor(dendrodat$maygrow))
dendrodat2$jungrow <- ifelse(dendrodat$jungrow%%1 >= 0.5,dendrodat2$jungrow <- ceiling(dendrodat$jungrow), dendrodat2$jungrow <- floor(dendrodat$jungrow))
dendrodat2$julgrow <- ifelse(dendrodat$julgrow%%1 >= 0.5,dendrodat2$julgrow <- ceiling(dendrodat$julgrow), dendrodat2$julgrow <- floor(dendrodat$julgrow))
dendrodat2$auggrow <- ifelse(dendrodat$auggrow%%1 >= 0.5,dendrodat2$auggrow <- ceiling(dendrodat$auggrow), dendrodat2$auggrow <- floor(dendrodat$auggrow))
dendrodat2$sepgrow <- ifelse(dendrodat$sepgrow%%1 >= 0.5,dendrodat2$sepgrow <- ceiling(dendrodat$sepgrow), dendrodat2$sepgrow <- floor(dendrodat$sepgrow))
dendrodat2$octgrow <- ifelse(dendrodat$octgrow%%1 >= 0.5,dendrodat2$octgrow <- ceiling(dendrodat$octgrow), dendrodat2$octgrow <- floor(dendrodat$octgrow))
dendrodat$maygrowr <- dendrodat2$maygrow; dendrodat$jungrowr <- dendrodat2$jungrow; dendrodat$julgrowr <- dendrodat2$julgrow; dendrodat$auggrowr <- dendrodat2$auggrow
dendrodat$sepgrowr <- dendrodat2$sepgrow; dendrodat$octgrowr <- dendrodat2$octgrow


################## visualizations ##################
#plot monthly average growth
# boxplot(dendrodat$maygrow,dendrodat$jungrow,dendrodat$julgrow,dendrodat$auggrow,dendrodat$sepgrow,dendrodat$octgrow,main="allmonths",ylab="Growth in mm", xlab="Month",las=2)
# axis(1,labels=c("May","Jun","Jul","Aug","Sep","Oct"),at=c(1:6),las=1)
#outliers present all but jun
# plot(dendrodat$maygrow,main="maygrow");plot(dendrodat$jungrow,main="jungrow");plot(dendrodat$julgrow,main="julgrow");plot(dendrodat$auggrow,main="auggrow")
# plot(dendrodat$sepgrow,main="sepgrow");plot(dendrodat$octgrow,main="octgrow")   
#q-q plots
# qqnorm(dendrodat$maygrow,main="maygrow");qqline(dendrodat$maygrow,main="maygrow")  #right skew
# qqnorm(dendrodat$jungrow,main="jungrow");qqline(dendrodat$jungrow,main="jungrow")  #light-tailed
# qqnorm(dendrodat$julgrow,main="julgrow");qqline(dendrodat$julgrow,main="julgrow")  #right skew
# qqnorm(dendrodat$auggrow,main="auggrow");qqline(dendrodat$auggrow,main="auggrow")  #right skew
# qqnorm(dendrodat$sepgrow,main="sepgrow");qqline(dendrodat$sepgrow,main="sepgrow")  #many 0's
# qqnorm(dendrodat$octgrow,main="octgrow");qqline(dendrodat$octgrow,main="octgrow")  #many 0's

#log transform--adding one to deal with 0's--0 transforms to 0
# dendrodat$logmaygrow<-log(dendrodat$maygrow+1);dendrodat$logjungrow<-log(dendrodat$jungrow+1);dendrodat$logjulgrow<-log(dendrodat$julgrow+1)
# dendrodat$logauggrow<-log(dendrodat$auggrow+1);dendrodat$logsepgrow<-log(dendrodat$sepgrow+1);dendrodat$logoctgrow<-log(dendrodat$octgrow+1)
#outliers present all but jun
# plot(dendrodat$logmaygrow,main="logmaygrow");plot(dendrodat$logjungrow,main="logjungrow");plot(dendrodat$logjulgrow,main="logjulgrow")
# plot(dendrodat$logauggrow,main="logauggrow");plot(dendrodat$logsepgrow,main="logsepgrow");plot(dendrodat$logoctgrow,main="logoctgrow")
#q-q plots
# qqnorm(dendrodat$logmaygrow,main="logmaygrow");qqline(dendrodat$logmaygrow,main="logmaygrow") #better than untransformed
# qqnorm(dendrodat$logjungrow,main="logjungrow");qqline(dendrodat$logjungrow,main="logjungrow") #worse than untransformed
# qqnorm(dendrodat$logjulgrow,main="logjulgrow");qqline(dendrodat$logjulgrow,main="logjulgrow") #better than untransformed  
# qqnorm(dendrodat$logauggrow,main="logauggrow");qqline(dendrodat$logauggrow,main="logauggrow") #better than untransformed
# qqnorm(dendrodat$logsepgrow,main="logsepgrow");qqline(dendrodat$logsepgrow,main="logsepgrow") #slightly better than untransformed
# qqnorm(dendrodat$logoctgrow,main="logoctgrow");qqline(dendrodat$logoctgrow,main="logoctgrow") #better than untransformed

##########sqrt transform--best transformations
dendrodat$sqrtmaygrow<-sqrt(dendrodat$maygrow);dendrodat$sqrtjungrow<-sqrt(dendrodat$jungrow);dendrodat$sqrtjulgrow<-sqrt(dendrodat$julgrow)
dendrodat$sqrtauggrow<-sqrt(dendrodat$auggrow);dendrodat$sqrtsepgrow<-sqrt(dendrodat$sepgrow);dendrodat$sqrtoctgrow<-sqrt(dendrodat$octgrow)
#outliers present all but jun
# plot(dendrodat$sqrtmaygrow,main="sqrtmaygrow");plot(dendrodat$sqrtjungrow,main="sqrtjungrow");plot(dendrodat$sqrtjulgrow,main="sqrtjulgrow")
# plot(dendrodat$sqrtauggrow,main="sqrtauggrow");plot(dendrodat$sqrtsepgrow,main="sqrtsepgrow");plot(dendrodat$sqrtoctgrow,main="sqrtoctgrow")
#q-q plots
# qqnorm(dendrodat$sqrtmaygrow,main="sqrtmaygrow");qqline(dendrodat$sqrtmaygrow,main="sqrtmaygrow") #better than log
# qqnorm(dendrodat$sqrtjungrow,main="sqrtjungrow");qqline(dendrodat$sqrtjungrow,main="sqrtjungrow") #different shape than untransformed, maybe slightly better
# qqnorm(dendrodat$sqrtjulgrow,main="sqrtjulgrow");qqline(dendrodat$sqrtjulgrow,main="sqrtjulgrow") #slightly better than log 
# qqnorm(dendrodat$sqrtauggrow,main="sqrtauggrow");qqline(dendrodat$sqrtauggrow,main="sqrtauggrow") #similar to log
# qqnorm(dendrodat$sqrtsepgrow,main="sqrtsepgrow");qqline(dendrodat$sqrtsepgrow,main="sqrtsepgrow") #better than log
# qqnorm(dendrodat$sqrtoctgrow,main="sqrtoctgrow");qqline(dendrodat$sqrtoctgrow,main="sqrtoctgrow") #better than log

#write to csv
# write.csv(dendrodat, 'F:/FIG/Dendrometer/Dendrometer Analyses/dendrodat.csv')

#separate datasets for each species
poplar <- dendrodat[dendrodat$spcode=='LITU',]
chestnutoak <- dendrodat[dendrodat$spcode=='QUMO',]
redoak <- dendrodat[dendrodat$spcode=='QURU',]
mockernut <- dendrodat[dendrodat$spcode=='CATO',]
whiteoak <- dendrodat[dendrodat$spcode=='QUAL',]
whitepine <- dendrodat[dendrodat$spcode=='PIST',]
redmaple <- dendrodat[dendrodat$spcode=='ACRU',]
blackbirch <- dendrodat[dendrodat$spcode=='BELE',]
hemlock <- dendrodat[dendrodat$spcode=='TCSA',]
aspen <- dendrodat[dendrodat$spcode=='POGR',]
