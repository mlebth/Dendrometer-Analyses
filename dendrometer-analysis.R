#load packages
library(glm2)
library(lme4)
library(nlme)
library(ggplot2)
library(MASS)

#read-in and load packages
dendrodat <- read.csv('F:/FIG/Dendrometer/dendrometer-data-corrected.csv')
summary(dendrodat)

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
#treeid [char]            = individual tree id 
#spnum [char]             = forestry species code 
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
#jun15temp-oct17temp	= average high temperature (F) in the previous month(s). Only in dataset when there is also a measurement.
#tot15rain-tot17rain	= total rainfall from initial band placement to last measurement
#tot15temp-tot17temp	= average high temperature from initial band placement to last measurement
####  *for measurements: in dataset as '999' if value is missing

#exploratory
table(dendrodat$spcode,dendrodat$site) #all sp occur in only 1 or 2 sites
table(dendrodat$spcode,dendrodat$sitequal) #all but PIST occur in only 1 or 2 site quals
table(dendrodat$site,dendrodat$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
table(dendrodat$spcode,dendrodat$timbersale) #at least one of each species in each level of timbersale
table(dendrodat$site,dendrodat$timbersale) #exactly even number of trees in each timbersale level per plot
table(dendrodat$site,dendrodat$aspect) #B06B and C03x: N; B10B: S
table(dendrodat$spcode,dendrodat$dominance) #most trees are co-dominant
table(dendrodat$tension,dendrodat$baselinedbh) #low tension dendrometers were used below 21.1 in DBH

#growth rates by month only (mean of all years per individual)
dendrodat$maygrow <- rowMeans(subset(dendrodat,select=c(may16grow,may17grow)),na.rm=TRUE)
dendrodat$jungrow <- rowMeans(subset(dendrodat,select=c(jun15grow,jun16grow,jun17grow)),na.rm=TRUE)
dendrodat$julgrow <- rowMeans(subset(dendrodat,select=c(jul15grow,jul16grow,jul17grow)),na.rm=TRUE)
dendrodat$auggrow <- rowMeans(subset(dendrodat,select=c(aug15grow,aug16grow,aug17grow)),na.rm=TRUE)
dendrodat$sepgrow <- rowMeans(subset(dendrodat,select=c(sep15grow,sep16grow,sep17grow)),na.rm=TRUE)
dendrodat$octgrow <- rowMeans(subset(dendrodat,select=c(oct15grow,oct16grow,oct17grow)),na.rm=TRUE)

########################################
#modeling
#plot monthly average growth
boxplot(dendrodat$maygrow,dendrodat$jungrow,dendrodat$julgrow,dendrodat$auggrow,dendrodat$sepgrow,dendrodat$octgrow) 
#outliers present all but jun
plot(dendrodat$maygrow)
plot(dendrodat$jungrow)
plot(dendrodat$julgrow)
plot(dendrodat$auggrow)
plot(dendrodat$sepgrow)
plot(dendrodat$octgrow)   
#q-q plots
qqnorm(dendrodat$maygrow) #right skew
qqnorm(dendrodat$jungrow) #light-tailed
qqnorm(dendrodat$julgrow) #right skew
qqnorm(dendrodat$auggrow) #right skew
qqnorm(dendrodat$sepgrow) #many 0's
qqnorm(dendrodat$octgrow) #many 0's

#t-test for each month by timbersale
#these group growth rates of all species by month
mayttest <- t.test(maygrow ~ timbersale, data=dendrodat)
junttest <- t.test(jungrow ~ timbersale, data=dendrodat)
julttest <- t.test(julgrow ~ timbersale, data=dendrodat)
augttest <- t.test(auggrow ~ timbersale, data=dendrodat)
septtest <- t.test(sepgrow ~ timbersale, data=dendrodat)
octttest <- t.test(octgrow ~ timbersale, data=dendrodat)

#2-way ANOVA. Response: growth; predictors: timbersale, site, species
aov2<-aov(tot16mm~spcode+timbersale+site, data=dendrodat)
summary(aov2)

#linear model
basic.lm <- lm(tot16mm ~ timbersale, data=dendrodat)
summary(basic.lm)
plot(basic.lm, which = 1) 
plot(basic.lm, which = 2)
boxplot(tot16mm ~ site)

#generalized linear model
glm1 <- glm(tot16mm ~ timbersale + sitequal + timbersale*sitequal, data=dendrodat)
summary(glm1)

#mixed model
#treeid, spcode, site, aspect, sitequal, timbersale, dominance
mixed.lmer <- lmer(tot16mm ~ timbersale + spcode + sitequal + (1|site), data=dendrodat)
summary(mixed.lmer)
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) 