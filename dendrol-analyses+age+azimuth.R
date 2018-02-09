#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc);library(nlme)

#read-in 
dendrolreadin <- read.csv('F:/TU/FIG/Dendrometer/Dendrometer Analyses/dendrol-3.csv')
ageaz <- read.csv('F:/TU/FIG/Dendrometer/Dendrometer Tree Growth Data/Age+Azimuth.csv') 
#adds in 'age' (ring count), 'azimuth', and 'azadj' (adjusted azimuth--small values are more N)
dendrol<-join(dendrolreadin,ageaz,by=c('treeid','year'),type='left',match='all')
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
#January 2018--removing this, no longer necessary
#dendrol$baselinestandBAs<-scale(dendrol$baselinestandBA,center=TRUE, scale=TRUE)
#dendrol$rains<-scale(dendrol$rain,center=TRUE, scale=TRUE)
#dendrol$temps<-scale(dendrol$temp,center=TRUE, scale=TRUE)

#separate datasets for each species grouping
hwood <- subset(dendrol,spcode=='QUMO'|spcode=='QURU'|spcode=='QUAL'|spcode=='POGR'|spcode=='CATO'|spcode=='ACRU'|spcode=='BELE')
swood <- subset(dendrol,spcode=='PIST' | spcode=='TSCA')
poplar<- dendrol[dendrol$spcode=='LITU',]
###create var for group insteadblack birch
dendrol$group <- ifelse((dendrol$spcode=='QUMO'|dendrol$spcode=='QURU'|dendrol$spcode=='QUAL'|dendrol$spcode=='POGR'|dendrol$spcode=='CATO'|
                         dendrol$spcode=='ACRU'|dendrol$spcode=='BELE'), 'hwood', 
                 ifelse((dendrol$spcode=='PIST'|dendrol$spcode=='TSCA'), 'swood', 
                 ifelse((dendrol$spcode=='LITU'                       ), 'pplar',
                        "NA")))
#seasonality variable
dendrol$season <- ifelse((dendrol$month=='may'|dendrol$month=='sep'|dendrol$month=='oct'),'ngr',
                  ifelse((dendrol$month=='jun'|dendrol$month=='jul'|dendrol$month=='aug'),'gro',
                        "NA"))
#new dataset for summer months only
summer<-dendrol[dendrol$season=='gro',]

#vif function
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

#exploring relationship between age and initial size
dat2015 <- subset(dendrol,year == 2015); dat2016 <- subset(dendrol,year == 2016); dat2017 <- subset(dendrol,year == 2017)
xyplot(baselineinddbh~age, data=dat2015, groups=group, auto.key = TRUE)
modelaov<- aov(age ~ baselineinddbh, data=dendrol);summary(modelaov) #p<0.0001 -- age and size are positively related
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and marginal growth
modelaov<- aov(age ~ logmarba, data=dendrol);summary(modelaov) #p=0.3 -- no relationship observed between age and marginal growth
xyplot(logmarba~age, data=dat2015, groups=group, auto.key = TRUE)
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and size each year
modelaov<- aov(age ~ newdbh, data=dendrol);summary(modelaov) #p=0.002 -- positive relationship between age and size each year
xyplot(newdbh~age, data=dat2015, groups=group, auto.key = TRUE)
xyplot(baselineinddbh~age, data=dat2015, groups=group, auto.key = TRUE)
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

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

#####full model
#try with either age or newdbh
modlmer <- lmer(logmarba ~ aspect + spcode + sitequal + timbersale + dominance + month + year + baselinestandBAs + baselineinddbh + ages + azadjs 
                + rains + temps + near + baselinestandBAs*rains*temps*age*azadj + aspect*spcode*sitequal*timbersale*dominance + (1|site/treeid), data=dendrol) 
#doesn't even run--first, check collinearity with VIF;
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + newdbh + age + azadj 
                + rain + temp + near + (1|site/treeid), data=dendrol) 
vif.lme(modlmer) #cutoff = 5; round 1: month to season; #round 2: spcode to group

#####model selection
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + newdbh + age + azadj 
                + rain + temp + near + group*sitequal*timbersale*dominance*season*year + near*baselinestandBA*baselineinddbh*age*azadj*rain*temp + (1|site/treeid), data=dendrol) 
#failure to produce full column rank design matrix
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + azadj 
                + rain + temp + near + group*sitequal*timbersale*season*age + (1|site/treeid), data=dendrol) 
#dropped all interaction terms one by one until I got to a model that would run, ending with the same terms that interacted w/o age or azimuth
#######NOTE--ran with age OR newdbh, age produced a much better fit (AIC=1704 with dbh, 1526 with age)
AIC(modlmer) #AIC=1534.43
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + azadj 
                + rain + temp + group*sitequal*timbersale*season*age + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1526.18 (removed near)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + azadj 
                + rain + group*sitequal*timbersale*season*age + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1523.00 (removed temp) 
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect
                + rain + group*sitequal*timbersale*season*age + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1516.21 (removed azadj) --> 1514.20 (replaced with aspect)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect
                + group*sitequal*timbersale*season*age + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1510.97 (removed rain)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselineinddbh + age + aspect 
                + group*sitequal*timbersale*season*age*aspect + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1494.30 (removed baselinestandBA, added aspect in interaction). Best model.
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer); Anova(modlmer)


#lowest AIC/all interactions, some are inestimable--tested each interaction separated out (also did the same at the beginning of selection with
#the same results)

modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselineinddbh + age + aspect 
                + group:sitequal + group:timbersale + group:dominance + group:season + group:year + group:baselineinddbh 
                + group:sitequal:timbersale + group:sitequal:season + group:sitequal:year
                + (1|site/treeid), data=dendrol) 
AIC(modlmer) #AIC=1520.02 (removed 4-way and 3-way interactions which were inestimable)
#best model with all-estimable coefficients
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer); Anova(modlmer)

modlmersumm <- lmer(logmarba ~ group + sitequal + timbersale + dominance + year + age + rain + group:sitequal + group:timbersale + 
                  sitequal:timbersale + (1|site/treeid), data=summer) 
AIC(modlmersumm) #AIC=795.10 (removed 4-way and 3-way interactions which were inestimable)
summary(modlmersumm); Anova(modlmersumm)

modlmersumm <- lmer(logmarba ~ aspect + group + sitequal + timbersale + dominance + month + year + rains + (1|site/treeid), data=summer)   
AIC(modlmersumm)   ####AIC=836
modglmsumm <- glm(logmarba ~ aspect + group + sitequal + timbersale + dominance + month + year + baselinestandBAs + rains, data=summer)  
AIC(modglmsumm)   ####AIC=801

Anova(modlmersumm,type="III") #get: num df, F, P of each var. F and p are the contrasts for vars w/2 categorical levels
summary(modlmersumm)#from this, get: beta/se/p for continuous vars, R2
(sum(residuals(modlmersumm,type="pearson")^2))/195 #chisq over df--note that denominator is residual deviance df, will change for each model


###########best models using dendrol or summer:

modlmer <- lmer(logmarba ~ aspect + year + sitequal +    group + timbersale + dominance + season +  baselineinddbh + age  
                + group*sitequal*timbersale*season*age*aspect + (1|site/treeid), data=dendrol) 

modlmer <- lmer(logmarba ~ aspect + year + rain + temp + group + timbersale + age + group:timbersale + group:season + timbersale:season + 
                  sitequal*season + (1|site/treeid), data=dendrol)
AIC(modlmer)
#######best model
modlmer <- lmer(logmarba ~ aspect + year + rain + temp + group + timbersale + group:timbersale + group:season + timbersale:season + 
                 sitequal*season + (1|site/treeid), data=dendrol)
AIC(modlmer)
Anova(modlmer,type="III") #get: num df, F, P of each var. F and p are the contrasts for vars w/2 categorical levels
summary(modlmer) #from this, get: beta/se for continuous vars, R2
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 

lsmeans(modlmer,list(pairwise ~ year, pairwise ~ group:timbersale, pairwise ~ group:season, pairwise ~ timbersale:season, pairwise ~ season:sitequal))
