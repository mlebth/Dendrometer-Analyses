#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection)

#read-in and load packages
dendrodat <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrometer-data-corrected.csv')
#summary(dendrodat)

#making numeric vars factors as needed
dendrodat$treeid <- factor(dendrodat$treeid)
dendrodat$spnum <- factor(dendrodat$spnum)
dendrodat$sitequal <- factor(dendrodat$sitequal)
#factor(dendrodat$treeid)
#factor(dendrodat$spnum)
#factor(dendrodat$sitequal)

#taking out 99's
dendrodat[dendrodat==999] <- NA

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
#jun15mm-sep17mm [num]   = growth in mm of trees from previous month's measurement* 
#tot15mm-tot17mm [num]   = total growth in mm of each tree from April-October* 
#jun15rain-oct17rain [num] = total precipitation (inches) in previous month. If more than one month passed
#between measurements, the value was divided by the number of intervening
#months for a monthly average. Only in dataset when there is also a measurement.
#jun15temp-oct17temp	 = average high temperature (F) in the previous month(s). Only in dataset when there is also a measurement.
#tot15rain-tot17rain	 = total rainfall from initial band placement to last measurement
#tot15temp-tot17temp	 = average high temperature from initial band placement to last measurement
####  *for measurements: in dataset as '999' if value is missing

#exploratory
# table(dendrodat$spcode,dendrodat$site) #all sp occur in only 1 or 2 sites
# table(dendrodat$spcode,dendrodat$sitequal) #all but PIST occur in only 1 or 2 site quals
# table(dendrodat$site,dendrodat$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
# table(dendrodat$spcode,dendrodat$timbersale) #at least one of each species in each level of timbersale
# table(dendrodat$site,dendrodat$timbersale) #exactly even number of trees in each timbersale level per plot
# table(dendrodat$site,dendrodat$aspect) #B06B and C03x: N; B10B: S
# table(dendrodat$spcode,dendrodat$dominance) #most trees are co-dominant
# table(dendrodat$tension,dendrodat$baselinedbh) #low tension dendrometers were used below 21.1 in DBH

#growth rates by month only (mean of all years per individual)
dendrodat$maygrow <- rowMeans(subset(dendrodat,select=c(may16grow,may17grow)),na.rm=TRUE)
dendrodat$jungrow <- rowMeans(subset(dendrodat,select=c(jun15grow,jun16grow,jun17grow)),na.rm=TRUE)
dendrodat$julgrow <- rowMeans(subset(dendrodat,select=c(jul15grow,jul16grow,jul17grow)),na.rm=TRUE)
dendrodat$auggrow <- rowMeans(subset(dendrodat,select=c(aug15grow,aug16grow,aug17grow)),na.rm=TRUE)
dendrodat$sepgrow <- rowMeans(subset(dendrodat,select=c(sep15grow,sep16grow,sep17grow)),na.rm=TRUE)
dendrodat$octgrow <- rowMeans(subset(dendrodat,select=c(oct15grow,oct16grow,oct17grow)),na.rm=TRUE)

#rainfall by month only (mean of all years per individual)
dendrodat$mayrain <- rowMeans(subset(dendrodat,select=c(may16rain,may17rain)),na.rm=TRUE)
dendrodat$junrain <- rowMeans(subset(dendrodat,select=c(jun15rain,jun16rain,jun17rain)),na.rm=TRUE)
dendrodat$julrain <- rowMeans(subset(dendrodat,select=c(jul15rain,jul16rain,jul17rain)),na.rm=TRUE)
dendrodat$augrain <- rowMeans(subset(dendrodat,select=c(aug15rain,aug16rain,aug17rain)),na.rm=TRUE)
dendrodat$seprain <- rowMeans(subset(dendrodat,select=c(sep15rain,sep16rain,sep17rain)),na.rm=TRUE)
dendrodat$octrain <- rowMeans(subset(dendrodat,select=c(oct15rain,oct16rain,oct17rain)),na.rm=TRUE)

#temperature by month only (mean of all years per individual)
dendrodat$maytemp <- rowMeans(subset(dendrodat,select=c(may16temp,may17temp)),na.rm=TRUE)
dendrodat$juntemp <- rowMeans(subset(dendrodat,select=c(jun15temp,jun16temp,jun17temp)),na.rm=TRUE)
dendrodat$jultemp <- rowMeans(subset(dendrodat,select=c(jul15temp,jul16temp,jul17temp)),na.rm=TRUE)
dendrodat$augtemp <- rowMeans(subset(dendrodat,select=c(aug15temp,aug16temp,aug17temp)),na.rm=TRUE)
dendrodat$septemp <- rowMeans(subset(dendrodat,select=c(sep15temp,sep16temp,sep17temp)),na.rm=TRUE)
dendrodat$octtemp <- rowMeans(subset(dendrodat,select=c(oct15temp,oct16temp,oct17temp)),na.rm=TRUE)

#problem: some numbers are not integers, problematic with poisson and negbin distributions.
# dendrodat$jul15grow%%1==0 #integer check
#rounds up when there is a non-integer
###################2015
dendrodat$jul15grow <- ceiling(dendrodat$jul15grow); dendrodat$aug15grow <- ceiling(dendrodat$aug15grow)
dendrodat$sep15grow <- ceiling(dendrodat$sep15grow); dendrodat$oct15grow <- ceiling(dendrodat$oct15grow)

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
