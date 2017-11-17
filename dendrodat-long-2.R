#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)

#read-in 
dendrodatl <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrodat-long.csv')
#summary(dendrodatl)

#making numeric vars factors as needed
dendrodatl$treeid   <- factor(dendrodatl$treeid)
dendrodatl$spnum    <- factor(dendrodatl$spnum)
dendrodatl$sitequal <- factor(dendrodatl$sitequal)
dendrodatl$year     <- factor(dendrodatl$year)

dendrol<-dendrodatl 

dendrol$olddbh<-ifelse(((dendrol$year=='2015')&(dendrol$month=='may'))|((dendrol$year=='2015')&(dendrol$month=='jun'))|((dendrol$year=='2015')
                       &(dendrol$month=='jul'))|((dendrol$year=='2016')&(dendrol$month=='may'))|((dendrol$year=='2017')&(dendrol$month=='may'))
                       ,dendrol$baselineinddbh)    #setting start columns
dendrol$oldBA  <- (3.14159*((dendrol$olddbh/2)^2))    #calculating oldBA from olddbh 
dendrol$newdbh <- (dendrol$growth+dendrol$olddbh)     #calculating newdbh
dendrol$newBA  <- (3.14159*((dendrol$newdbh/2)^2))    #calculating newBA from newdbh
dendrol$mardbh <- (dendrol$newdbh-dendrol$olddbh)     #calculating mardbh
dendrol$marba  <- (dendrol$newBA-dendrol$oldBA)       #calculating marba

#1-treeid, 2-spnum, 3-spname, 4-spcode, 5-site, 6-aspect, 7-sitequal,8-timbersale, 9-dominance, 10-baselinestandBA, 11-baselineinddbh, 12-tension, 13-year
#14-month, 15-growth, 16-rain, 17-temp, 18-olddbh, 19-oldBA, 20-newdbh, 21-newBA, 22-mardbh, 23-marba

for (treeid in 1:32){
  if it's later, olddbh=prevolddbh, else '
  add olddbh to newdbh
}
dendrol[560:576,11:23]

write.csv(dendrol,'F:/FIG/Dendrometer/Dendrometer Analyses/dendrol.csv')

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
# table(dendrodatl$spcode,dendrodatl$site) #all sp occur in only 1 or 2 sites
# table(dendrodatl$spcode,dendrodatl$sitequal) #all but PIST occur in only 1 or 2 site quals
# table(dendrodatl$site,dendrodatl$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
# table(dendrodatl$spcode,dendrodatl$timbersale) #at least one of each species in each level of timbersale
# table(dendrodatl$site,dendrodatl$timbersale) #exactly even number of trees in each timbersale level per plot
# table(dendrodatl$site,dendrodatl$aspect) #B06B and C03x: N; B10B: S
# table(dendrodatl$spcode,dendrodatl$dominance) #most trees are co-dominant
# table(dendrodatl$tension,dendrodatl$baselinedbh) #low tension dendrometers were used below 21.1 in DBH

#problem: some numbers are not integers, problematic with poisson and negbin distributions.
# dendrodatl$growth%%1==0 #integer check
#rounds up when there is a non-integer
###################2015
dendrodatl$growth <- ceiling(dendrodatl$growth)

###################2016
dendrodatl$jul16grow <- ceiling(dendrodatl$jul16grow)

###################2017
dendrodatl$jun17grow <- ceiling(dendrodatl$jun17grow); dendrodatl$jul17grow <- ceiling(dendrodatl$jul17grow)
dendrodatl$aug17grow <- ceiling(dendrodatl$aug17grow); dendrodatl$oct17grow <- ceiling(dendrodatl$oct17grow)
dendrodatl$tot17grow <- ceiling(dendrodatl$tot17grow); 

###################totals. these are done differently because not all are 0.5 (some should be rounded up, some down to get to nearest int)
dendrodatl$maygrow<-round(dendrodatl$maygrow,digits=1);dendrodatl$jungrow<-round(dendrodatl$jungrow,digits=1);dendrodatl$julgrow<-round(dendrodatl$julgrow,digits=1)
dendrodatl$auggrow<-round(dendrodatl$auggrow,digits=1);dendrodatl$sepgrow<-round(dendrodatl$sepgrow,digits=1);dendrodatl$octgrow<-round(dendrodatl$octgrow,digits=1)

dendrodatl2 <- dendrodatl #new dataframe
#rounding to nearest integer. ##########PROBLEM: 0.3 rounds down to 0.
dendrodatl2$maygrow <- ifelse(dendrodatl$maygrow%%1 >= 0.5,dendrodatl2$maygrow <- ceiling(dendrodatl$maygrow), dendrodatl2$maygrow <- floor(dendrodatl$maygrow))
dendrodatl2$jungrow <- ifelse(dendrodatl$jungrow%%1 >= 0.5,dendrodatl2$jungrow <- ceiling(dendrodatl$jungrow), dendrodatl2$jungrow <- floor(dendrodatl$jungrow))
dendrodatl2$julgrow <- ifelse(dendrodatl$julgrow%%1 >= 0.5,dendrodatl2$julgrow <- ceiling(dendrodatl$julgrow), dendrodatl2$julgrow <- floor(dendrodatl$julgrow))
dendrodatl2$auggrow <- ifelse(dendrodatl$auggrow%%1 >= 0.5,dendrodatl2$auggrow <- ceiling(dendrodatl$auggrow), dendrodatl2$auggrow <- floor(dendrodatl$auggrow))
dendrodatl2$sepgrow <- ifelse(dendrodatl$sepgrow%%1 >= 0.5,dendrodatl2$sepgrow <- ceiling(dendrodatl$sepgrow), dendrodatl2$sepgrow <- floor(dendrodatl$sepgrow))
dendrodatl2$octgrow <- ifelse(dendrodatl$octgrow%%1 >= 0.5,dendrodatl2$octgrow <- ceiling(dendrodatl$octgrow), dendrodatl2$octgrow <- floor(dendrodatl$octgrow))
dendrodatl$maygrowr <- dendrodatl2$maygrow; dendrodatl$jungrowr <- dendrodatl2$jungrow; dendrodatl$julgrowr <- dendrodatl2$julgrow; dendrodatl$auggrowr <- dendrodatl2$auggrow
dendrodatl$sepgrowr <- dendrodatl2$sepgrow; dendrodatl$octgrowr <- dendrodatl2$octgrow


################## visualizations ##################
#plot monthly average growth
# boxplot(dendrodatl$maygrow,dendrodatl$jungrow,dendrodatl$julgrow,dendrodatl$auggrow,dendrodatl$sepgrow,dendrodatl$octgrow,main="allmonths",ylab="Growth in mm", xlab="Month",las=2)
# axis(1,labels=c("May","Jun","Jul","Aug","Sep","Oct"),at=c(1:6),las=1)
#outliers present all but jun
# plot(dendrodatl$maygrow,main="maygrow");plot(dendrodatl$jungrow,main="jungrow");plot(dendrodatl$julgrow,main="julgrow");plot(dendrodatl$auggrow,main="auggrow")
# plot(dendrodatl$sepgrow,main="sepgrow");plot(dendrodatl$octgrow,main="octgrow")   
#q-q plots
# qqnorm(dendrodatl$maygrow,main="maygrow");qqline(dendrodatl$maygrow,main="maygrow")  #right skew
# qqnorm(dendrodatl$jungrow,main="jungrow");qqline(dendrodatl$jungrow,main="jungrow")  #light-tailed
# qqnorm(dendrodatl$julgrow,main="julgrow");qqline(dendrodatl$julgrow,main="julgrow")  #right skew
# qqnorm(dendrodatl$auggrow,main="auggrow");qqline(dendrodatl$auggrow,main="auggrow")  #right skew
# qqnorm(dendrodatl$sepgrow,main="sepgrow");qqline(dendrodatl$sepgrow,main="sepgrow")  #many 0's
# qqnorm(dendrodatl$octgrow,main="octgrow");qqline(dendrodatl$octgrow,main="octgrow")  #many 0's

#log transform--adding one to deal with 0's--0 transforms to 0
# dendrodatl$logmaygrow<-log(dendrodatl$maygrow+1);dendrodatl$logjungrow<-log(dendrodatl$jungrow+1);dendrodatl$logjulgrow<-log(dendrodatl$julgrow+1)
# dendrodatl$logauggrow<-log(dendrodatl$auggrow+1);dendrodatl$logsepgrow<-log(dendrodatl$sepgrow+1);dendrodatl$logoctgrow<-log(dendrodatl$octgrow+1)
#outliers present all but jun
# plot(dendrodatl$logmaygrow,main="logmaygrow");plot(dendrodatl$logjungrow,main="logjungrow");plot(dendrodatl$logjulgrow,main="logjulgrow")
# plot(dendrodatl$logauggrow,main="logauggrow");plot(dendrodatl$logsepgrow,main="logsepgrow");plot(dendrodatl$logoctgrow,main="logoctgrow")
#q-q plots
# qqnorm(dendrodatl$logmaygrow,main="logmaygrow");qqline(dendrodatl$logmaygrow,main="logmaygrow") #better than untransformed
# qqnorm(dendrodatl$logjungrow,main="logjungrow");qqline(dendrodatl$logjungrow,main="logjungrow") #worse than untransformed
# qqnorm(dendrodatl$logjulgrow,main="logjulgrow");qqline(dendrodatl$logjulgrow,main="logjulgrow") #better than untransformed  
# qqnorm(dendrodatl$logauggrow,main="logauggrow");qqline(dendrodatl$logauggrow,main="logauggrow") #better than untransformed
# qqnorm(dendrodatl$logsepgrow,main="logsepgrow");qqline(dendrodatl$logsepgrow,main="logsepgrow") #slightly better than untransformed
# qqnorm(dendrodatl$logoctgrow,main="logoctgrow");qqline(dendrodatl$logoctgrow,main="logoctgrow") #better than untransformed

##########sqrt transform--best transformations
dendrodatl$sqrtmaygrow<-sqrt(dendrodatl$maygrow);dendrodatl$sqrtjungrow<-sqrt(dendrodatl$jungrow);dendrodatl$sqrtjulgrow<-sqrt(dendrodatl$julgrow)
dendrodatl$sqrtauggrow<-sqrt(dendrodatl$auggrow);dendrodatl$sqrtsepgrow<-sqrt(dendrodatl$sepgrow);dendrodatl$sqrtoctgrow<-sqrt(dendrodatl$octgrow)
#outliers present all but jun
# plot(dendrodatl$sqrtmaygrow,main="sqrtmaygrow");plot(dendrodatl$sqrtjungrow,main="sqrtjungrow");plot(dendrodatl$sqrtjulgrow,main="sqrtjulgrow")
# plot(dendrodatl$sqrtauggrow,main="sqrtauggrow");plot(dendrodatl$sqrtsepgrow,main="sqrtsepgrow");plot(dendrodatl$sqrtoctgrow,main="sqrtoctgrow")
#q-q plots
# qqnorm(dendrodatl$sqrtmaygrow,main="sqrtmaygrow");qqline(dendrodatl$sqrtmaygrow,main="sqrtmaygrow") #better than log
# qqnorm(dendrodatl$sqrtjungrow,main="sqrtjungrow");qqline(dendrodatl$sqrtjungrow,main="sqrtjungrow") #different shape than untransformed, maybe slightly better
# qqnorm(dendrodatl$sqrtjulgrow,main="sqrtjulgrow");qqline(dendrodatl$sqrtjulgrow,main="sqrtjulgrow") #slightly better than log 
# qqnorm(dendrodatl$sqrtauggrow,main="sqrtauggrow");qqline(dendrodatl$sqrtauggrow,main="sqrtauggrow") #similar to log
# qqnorm(dendrodatl$sqrtsepgrow,main="sqrtsepgrow");qqline(dendrodatl$sqrtsepgrow,main="sqrtsepgrow") #better than log
# qqnorm(dendrodatl$sqrtoctgrow,main="sqrtoctgrow");qqline(dendrodatl$sqrtoctgrow,main="sqrtoctgrow") #better than log

#write to csv
# write.csv(dendrodatl, 'F:/FIG/Dendrometer/Dendrometer Analyses/dendrodatl.csv')

#separate datasets for each species
poplar <- dendrodatl[dendrodatl$spcode=='LITU',]
chestnutoak <- dendrodatl[dendrodatl$spcode=='QUMO',]
redoak <- dendrodatl[dendrodatl$spcode=='QURU',]
mockernut <- dendrodatl[dendrodatl$spcode=='CATO',]
whiteoak <- dendrodatl[dendrodatl$spcode=='QUAL',]
whitepine <- dendrodatl[dendrodatl$spcode=='PIST',]
redmaple <- dendrodatl[dendrodatl$spcode=='ACRU',]
blackbirch <- dendrodatl[dendrodatl$spcode=='BELE',]
hemlock <- dendrodatl[dendrodatl$spcode=='TCSA',]
aspen <- dendrodatl[dendrodatl$spcode=='POGR',]
