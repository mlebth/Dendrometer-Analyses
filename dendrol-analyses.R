#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc)

#read-in 
dendrol <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrol-2.csv')
#summary(dendrol)

#making numeric vars factors as needed
dendrol$treeid <- factor(dendrol$treeid)
dendrol$year <- factor(dendrol$year)
dendrol$spnum <- factor(dendrol$spnum)
dendrol$sitequal <- factor(dendrol$sitequal)

#data transformations
dendrol$sqmarba<-sqrt(dendrol$marba)
#log transform--adding one to deal with 0's--0 transforms to 0
dendrol$logmarba<-log(dendrol$marba+1)  ###0's---inf, not possible  

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
#baselinestandBA [num]   = stand basal area first installation (in inches) 
#baselineinddbh [num]    = DBH of individual at date of first installation (in inches) 
#tension [char]          = lo, hi or na -- densiometer tension. only available for trees 1-24 (24-32: na) 
#year [char]             = year 
#month [char]            = month 
#growth [num]            = growth (mm) for the month. Where more than one month elapsed between measurements, data were normalized to reflect monthly growth by dividing the number by months since last measurement
#rain                    = total rainfall from initial band placement to last measurement
#temp                    = average high temperature (F) in the previous month(s). Only in dataset when there is also a measurement.
#olddbh, oldBA           = previous dbh and basal area (individual)
#newdbh, newBA           = new dbh and basal area (individual)
#mardbh, marBA           = marginal growth in dbh and BA

#exploratory 
# predictor: spcode, site, aspect, sitequal, timbersale, dominance
# spvsite<-table(dendrol$spcode,dendrol$site) #all sp occur in only 1 or 2 sites
# spvaspect<-table(dendrol$spcode,dendrol$aspect) #at least one of each species in each level of aspect
# spvsitequal<-table(dendrol$spcode,dendrol$sitequal) #all but PIST occur in only 1 or 2 site quals (PIST=3)
# spvtimbersale<-table(dendrol$spcode,dendrol$timbersale) #at least one of each species in each level of timbersale
# spvdom<-table(dendrol$spcode,dendrol$dominance) #most trees are co-dominant
# sitevaspect<-table(dendrol$site,dendrol$aspect) #B06B and C03x: N; B10B: S
# sitevsitequal<-table(dendrol$site,dendrol$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
# sitevtimbersale<-table(dendrol$site,dendrol$timbersale) #exactly even number of trees in each timbersale level per plot
# sitevdom<-table(dendrol$site,dendrol$dominance) 
# qualvsale<-table(dendrol$sitequal,dendrol$timbersale)
# qualvdom<-table(dendrol$sitequal,dendrol$dominance)
# salevdom<-table(dendrol$timbersale,dendrol$dominance) #exactly even
#chi sq test of independence
# cspvsite<-chisq.test(spvsite) #p<0.0001             *
# cspvsitequal<-chisq.test(spvsitequal)#p<0.0001      *
# cspvtimbersale<-chisq.test(spvtimbersale) #p=1       #equal numbers in each cat
# cspvaspect<-chisq.test(spvaspect) #p<0.0001         *
# csitevsitequal<-chisq.test(sitevsitequal) #p<0.0001 *
# csitevtimbersale<-chisq.test(sitevtimbersale) #p=1   #equal numbers in each cat
# csitevaspect<-chisq.test(sitevaspect) #p<0.0001     *
# cqualvsale<-chisq.test(qualvsale) #p<0.0001         *
# csitevdom<-chisq.test(sitevdom) #p<0.0001           *
# cqualvdom<-chisq.test(qualvdom) #p<0.0001           *
# csalevdom<-chisq.test(salevdom) #p=1                 #equal numbers in each cat

# predictor: spcode, site, aspect, sitequal, timbersale, dominance
# bacode<-lm(baselinestandBA~spcode,data=dendrol) #p<0.0001       *
# basite<-lm(baselinestandBA~site,data=dendrol) #p=0.7
# baasp<-lm(baselinestandBA~aspect,data=dendrol) #p=0.4
# baqual<-lm(baselinestandBA~sitequal,data=dendrol) #p=0.01       *
# basale<-lm(baselinestandBA~timbersale,data=dendrol) #p<0.0001   *
# badom<-lm(baselinestandBA~dominance,data=dendrol) #p<0.0001     *

# barain<-lm(baselinestandBA~rain,data=dendrol) #p=0.9            
# batemp<-lm(baselinestandBA~temp,data=dendrol) #p=0.8            
# raintemp<-lm(rain~temp,data=dendrol) #p<0.0001                  *

# related:sp-site, sp-qual, sp-aspect, sp-ba, site-ba, site-qual, site-aspect,qual-sale, site-dom, qual-dom, ba-dom, ba-sale, rain-temp

################## visualizations ##################
#plot monthly average growth
plot(dendrol$sqmarba~dendrol$month,main="allmonths",ylab="Growth in mm", xlab="Month",las=2)
####SQRT transformation is best for normalization of data
plot(dendrol$marba,main="marba")
plot(dendrol$sqmarba,main="sqmarba")
plot(dendrol$logmarba,main="logmarba")
#q-q plots
qqnorm(dendrol$marba,main="marba");qqline(dendrol$marba,main="marba") 
qqnorm(dendrol$sqmarba,main="sqmarba");qqline(dendrol$sqmarba,main="sqmarba") 
qqnorm(dendrol$logmarba,main="logmarba");qqline(dendrol$logmarba,main="logmarba") #log is best

###########GLM
# predictor: spcode, site, aspect, sitequal, timbersale, dominance, baselinestandBA, year, month, rain, temp
# response:  growth, marBA

##sig corr: baselineBA-timbersale,site-aspect,site-sitequal,sp-aspect,sp-sitequal,sp-site
#gaussian--linear model
maylm <- lm(sqrtmaygrow ~ timbersale + sitequal, data=dendrol)
Anova(fullcont,type="III") #get: num df, F, P of each var. F and p are the contrasts for vars w/2 categorical levels
AICc(fullcont)
summary(fullcont) #from this, get: beta/se/p for continuous vars, R2
(sum(residuals(fullcont,type="pearson")^2))/347 # chisq over df--note that denominator is residual deviance df, will change for each model
lsmeans(fullcont,list(pairwise ~ timbersale, pairwise ~ sitequal))
#lsmeans: gives beta (lsmean), se, df, lovercl and upper cl (must manually back-transform)
#ignore pairwise, still need to figure that out (these are incorrect estimates)

plot(octlm, which = 1,main="residuals v fitted glm") 
qqnorm(resid(octlm));qqline(resid(octlm),main="q-q plot glm") 
plot(sqrtmaygrow ~ timbersale, data=dendrol,xlab="Timbersale",ylab="May growth",main="May growth~timbersale")
plot(sqrtmaygrow ~ sitequal, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")

#not using site as random for now
##########GLMM (mixed model)
#treeid, spcode, site, aspect, sitequal, timbersale, dominance
marba <- lmer(logmarba ~ timbersale + sitequal + month + year + spcode + aspect + baselineinddbh + baselinestandBA + rain + temp + baselineinddbh*rain + baselineinddbh*temp + baselineinddbh*baselinestandBA + rain*temp + (1|site), data=dendrol)
summary(marba)
(sum(residuals(marba,type="pearson")^2))/375 #chisq over df--note that denominator is residual deviance df, will change for each model
plot(marba,main="residuals v fitted mixed")
qqnorm(resid(marba));qqline(resid(marba),main="q-q plot mixed") 

