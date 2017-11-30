#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc);library(nlme)

#read-in 
dendrol <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrol-3.csv')
#summary(dendrol)

dendrol$month<-factor(dendrol$month, levels=c("may", "jun", "jul", "aug", "sep", "oct"))
#making numeric vars factors as needed
dendrol$treeid <- factor(dendrol$treeid)
dendrol$year <- factor(dendrol$year)
dendrol$spnum <- factor(dendrol$spnum)
dendrol$sitequal <- factor(dendrol$sitequal)

#data transformations
dendrol$sqmarba<-sqrt(dendrol$marba)
#log transform--adding one to deal with 0's--0 transforms to 0
dendrol$logmarba<-log(dendrol$marba+1)  ###0's---inf, not possible  

#centering and scaling continuous variables--rain/temp and BA are on very different scales
dendrol$baselinestandBAs<-scale(dendrol$baselinestandBA,center=TRUE, scale=TRUE)
dendrol$rains<-scale(dendrol$rain,center=TRUE, scale=TRUE)
dendrol$temps<-scale(dendrol$temp,center=TRUE, scale=TRUE)

#separate datasets for each species grouping
hwood <- subset(dendrol,spcode=='QUMO'|spcode=='QURU'|spcode=='QUAL'|spcode=='POGR'|spcode=='CATO'|spcode=='ACRU'|spcode=='BELE')
swood <- subset(dendrol,spcode=='PIST' | spcode=='TSCA')
poplar<- dendrol[dendrol$spcode=='LITU',]
###create var for group instead
dendrol$group <- ifelse((dendrol$spcode=='QUMO'|dendrol$spcode=='QURU'|dendrol$spcode=='QUAL'|dendrol$spcode=='POGR'|dendrol$spcode=='CATO'|
                         dendrol$spcode=='ACRU'|dendrol$spcode=='BELE'), 'hwood', 
                 ifelse((dendrol$spcode=='PIST'|dendrol$spcode=='TSCA'), 'swood', 
                 ifelse((dendrol$spcode=='LITU'                       ), 'pplar',
                        "NA")))
#seasonality variable
dendrol$season <- ifelse((dendrol$month=='may'|dendrol$month=='sep'|dendrol$month=='oct'),'cld',
                  ifelse((dendrol$month=='jun'|dendrol$month=='jul'|dendrol$month=='aug'),'hot',
                        "NA"))
#new dataset for summer months only
summer<-dendrol[dendrol$season=='hot',]

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
#rain                    = total rainfall from initial band placement to last measurement
#temp                    = average high temperature (F) in the previous month(s). Only in dataset when there is also a measurement. 
#growth [num]            = growth (mm) for the month. Where more than one month elapsed between measurements, data were normalized to reflect monthly growth by dividing the number by months since last measurement
#prevmeas                = dbh at previous measurement*
#olddbh, oldBA           = previous dbh and basal area (individual)*
#newdbh, newBA           = new dbh and basal area (individual)
#mardbh, marBA           = marginal growth in dbh and BA
#group [char]            = [hwood: oaks and other hardwoods, swood: pine and hemlock, poplar: tulip poplar]
#season                  = [cld: colder months--may, sep, oct. hot: hotter months--jun, jul, aug]
# * in 2015, some trees were measured 2 months apart. in these instances, 'growth' is first added to 'prevmeas' to obtain a more accurate estimate of 
#   old dbh and a more accurate marginal dbh.

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
plot(dendrol$marba~dendrol$month,xlab="month",ylab=" marginal BA",main="marginal growth (BA) in each month")
####SQRT transformation is best for normalization of data
plot(dendrol$marba,main="marba")
plot(dendrol$sqmarba,main="sqmarba")
plot(dendrol$logmarba,main="logmarba")
plot(dendrol$logmarba~dendrol$month,xlab="month",ylab="log marginal BA",main="log marginal growth (BA) in each month")
#q-q plots
qqnorm(dendrol$marba,main="marba");qqline(dendrol$marba,main="marba") 
qqnorm(dendrol$sqmarba,main="sqmarba");qqline(dendrol$sqmarba,main="sqmarba") 
qqnorm(dendrol$logmarba,main="logmarba");qqline(dendrol$logmarba,main="logmarba") #log is best

#########add figure: month by month and by timbersale

##########GLMM (mixed model)
# predictor: treeid,spcode, site, aspect, sitequal, timbersale, dominance, year, month, baselinestandBA, rain, temp, group, season
# response:  growth, marBA

# related:sp-site, sp-qual, sp-aspect, sp-ba, site-ba, site-qual, site-aspect,qual-sale, site-dom, qual-dom, ba-dom, ba-sale, rain-temp
  #modlme  <- lme (logmarba ~ spcode + aspect + sitequal + timbersale + dominance + month + year + baselineinddbh + baselinestandBA + rain + temp + baselinestandBA*rain*temp, random=list(~1|site/aspect/treeid), data=dendrol, na.action=na.exclude)
#Error in MEEM(object, conLin, control$niterEM) : NAs in foreign function call (arg 2)
#indicates numerical instability, consider re-scaling predictor vars. switching to lmer

#centering and scaling continuous variables--rain/temp and BA are on very different scales
dendrol$baselinestandBAs<-scale(dendrol$baselinestandBA,center=TRUE, scale=TRUE)
dendrol$rains<-scale(dendrol$rain,center=TRUE, scale=TRUE)
dendrol$temps<-scale(dendrol$temp,center=TRUE, scale=TRUE)

#####full model
modlmer <- lmer(logmarba ~ treeid + aspect + spcode + sitequal + timbersale + dominance + month + year + baselinestandBAs + rains + temps +
                  baselinestandBAs*rains*temps + (1|site), data=dendrol) 
#fixed-effect model matrix is rank deficient so dropping 15 columns / coefficients. AIC=1561

#####model selection
modlmer <- lmer(logmarba ~ aspect + spcode + sitequal + timbersale + month + year + rains + temps + (1|site/aspect/treeid), data=dendrol) 
AIC(modlmer)   ####AIC=1653. Best fit using original variables.
#best glm with dendrol below:
modlmer <- glm(logmarba ~ treeid + aspect + spcode + sitequal + timbersale + month + year + rains + temps , data=dendrol) 
AIC(modlmer)   ####AIC=1556 Best fit using original variables w/o random

modlmer <- lmer(logmarba ~ aspect + group + sitequal + timbersale + season + year + rains + temps + (1|site/aspect), data=dendrol) 
AIC(modlmer)   ####AIC=1676. Best fit using group and season.
modlmer <- glm(logmarba ~ treeid + aspect + group + sitequal + timbersale + season + year + rains + temps , data=dendrol) 
AIC(modlmer)   ####AIC=1566 Best fit using group and season w/o random

modlmer <- lmer(logmarba ~ treeid + aspect + group + sitequal + timbersale + (1|site), data=summer)   
AIC(modlmer)   ####Hessian non-PD with more variables
modlmer <- glm(logmarba ~ treeid + aspect + spcode + sitequal + timbersale + dominance + month + year + baselinestandBAs + rains, data=summer)  
AIC(modlmer)   ####AIC=801

###########best lmer and glm using dendrol or summer:

modlmer <- glm(logmarba ~ treeid + aspect + spcode + sitequal + timbersale + month + year + rains + temps , data=dendrol) 

Anova(modlmer,type="III") #get: num df, F, P of each var. F and p are the contrasts for vars w/2 categorical levels
summary(modlmer);AIC(modlmer) #from this, get: beta/se/p for continuous vars, R2
(sum(residuals(modlmer,type="pearson")^2))/375 #chisq over df--note that denominator is residual deviance df, will change for each model
lsmeans(modlmer,list(pairwise ~ timbersale, pairwise ~ sitequal))
#lsmeans: gives beta (lsmean), se, df, lovercl and upper cl (must manually back-transform)
#ignore pairwise, still need to figure that out (these are incorrect estimates)

qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
shapiro.test(resid(modlmer))

modlmer <- lmer(logmarba ~ aspect + spcode + sitequal + timbersale + month + year + rains + temps + (1|site/aspect), data=dendrol)
plot(logmarba ~ spcode, data=dendrol,xlab="Timbersale",ylab="May growth",main="May growth~timbersale")
plot(logmarba ~ timbersale, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ month, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ year, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ rain, data=dendrol,xlab="Rainfall",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")




