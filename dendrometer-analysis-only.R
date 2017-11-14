#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)

#read-in 
dendrodat <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrodat.csv')

#####DATASETS:
# all data in 'dendrodat'
# separate datasets for each species: 'poplar', 'chestnutoak', 'redoak', 'mockernut', 'whiteoak',
    # 'whitepine', 'redmaple', 'blackbirch', 'hemlock', 'aspen'

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
#jun15grow-sep17grow [num]   = growth in mm of trees from previous month's measurement* 
#tot15grow-tot17grow [num]   = total growth in mm of each tree from April-October* 
#jun15rain-oct17rain [num] = total precipitation (inches) in previous month. If more than one month passed
#between measurements, the value was divided by the number of intervening
#months for a monthly average. Only in dataset when there is also a measurement.
#jun15temp-oct17temp	= average high temperature (F) in the previous month(s). Only in dataset when there is also a measurement.
#tot15rain-tot17rain	= total rainfall from initial band placement to last measurement
#tot15temp-tot17temp	= average high temperature from initial band placement to last measurement
####  *for measurements: in dataset as '999' if value is missing. Also, note that the following values were rounded up from 0.5:
####  *jul-oct15grow, jul16grow, jun-aug17grow, oct17grow, tot17grow

################################### DENDRODAT #############################################

################## visualizations ##################
#plot monthly average growth
boxplot(dendrodat$maygrow,dendrodat$jungrow,dendrodat$julgrow,dendrodat$auggrow,dendrodat$sepgrow,dendrodat$octgrow,main="allmonths",ylab="Growth in mm", xlab="Month",las=2)
axis(1,labels=c("May","Jun","Jul","Aug","Sep","Oct"),at=c(1:6),las=1) #outliers present all but jun
####SQRT transformation is best for normalization of data
plot(dendrodat$sqrtmaygrow,main="sqrtmaygrow");plot(dendrodat$sqrtjungrow,main="sqrtjungrow");plot(dendrodat$sqrtjulgrow,main="sqrtjulgrow")
plot(dendrodat$sqrtauggrow,main="sqrtauggrow");plot(dendrodat$sqrtsepgrow,main="sqrtsepgrow");plot(dendrodat$sqrtoctgrow,main="sqrtoctgrow")
#q-q plots
qqnorm(dendrodat$sqrtmaygrow,main="sqrtmaygrow");qqline(dendrodat$sqrtmaygrow,main="sqrtmaygrow") #better than log
qqnorm(dendrodat$sqrtjungrow,main="sqrtjungrow");qqline(dendrodat$sqrtjungrow,main="sqrtjungrow") #different shape than untransformed, maybe slightly better
qqnorm(dendrodat$sqrtjulgrow,main="sqrtjulgrow");qqline(dendrodat$sqrtjulgrow,main="sqrtjulgrow") #slightly better than log 
qqnorm(dendrodat$sqrtauggrow,main="sqrtauggrow");qqline(dendrodat$sqrtauggrow,main="sqrtauggrow") #similar to log
qqnorm(dendrodat$sqrtsepgrow,main="sqrtsepgrow");qqline(dendrodat$sqrtsepgrow,main="sqrtsepgrow") #better than log
qqnorm(dendrodat$sqrtoctgrow,main="sqrtoctgrow");qqline(dendrodat$sqrtoctgrow,main="sqrtoctgrow") #better than log

#t-test for each month by timbersale
#these group growth rates of all species by month
t.test(sqrtmaygrow ~ timbersale, data=dendrodat);t.test(sqrtjungrow ~ timbersale, data=dendrodat);t.test(sqrtjulgrow ~ timbersale, data=dendrodat)
t.test(sqrtauggrow ~ timbersale, data=dendrodat);t.test(sqrtsepgrow ~ timbersale, data=dendrodat);t.test(sqrtoctgrow ~ timbersale, data=dendrodat)

###########GLM
#spcode, site, aspect, sitequal, timbersale, baselineBA, dominance, baselinedbh, rain, temp

#semi-full model: continuous interactions
fullcont<-lm(sqrtoctgrow ~ timbersale + sitequal + site + aspect + baselinedbh + baselineBA + octrain + octtemp + baselinedbh*octrain + baselinedbh*octtemp + baselinedbh*baselineBA + octrain*octtemp, data=dendrodat)
#semi-full model: categorical interactions
fullcat<-lm(sqrtmaygrow ~ timbersale + sitequal + site + aspect + baselinedbh + baselineBA + octrain + octtemp + timbersale*sitequal + timbersale*aspect + sitequal*aspect, data=dendrodat)
#more interactions per model--overspecified, not enough DF
summary(fullcont);AICc(fullcont);anova(fullcont)
summary(fullcat);AICc(fullcat);anova(fullcat)

#gaussian--linear model
maylm <- lm(sqrtmaygrow ~ timbersale + sitequal, data=dendrodat)
junlm <- lm(sqrtjungrow ~ timbersale + sitequal + baselinedbh + junrain + juntemp + baselinedbh*juntemp, data=dendrodat)
jullm <- lm(sqrtjulgrow ~ timbersale + sitequal + baselinedbh + julrain + baselinedbh*julrain, data=dendrodat)
auglm <- lm(sqrtauggrow ~ timbersale, data=dendrodat)
seplm <- lm(sqrtsepgrow ~ septemp, data=dendrodat)
octlm <- lm(sqrtoctgrow ~ timbersale +  baselinedbh + octrain + baselinedbh*octrain, data=dendrodat)

Anova(maylm,type="III") #get: num df, F, P of each var. F and p are the contrasts for vars w/2 categorical levels
AICc(maylm)
summary(junlm) #from this, get: beta/se/p for continuous vars, R2
(sum(residuals(junlm,type="pearson")^2))/27 # chisq over df--note that denominator is residual deviance df, will change for each model
lsmeans(junlm,list(pairwise ~ timbersale, pairwise ~ sitequal))
#lsmeans: gives beta (lsmean), se, df, lovercl and upper cl (must manually back-transform)
#ignore pairwise, still need to figure that out (these are incorrect estimates)

plot(octlm, which = 1,main="residuals v fitted glm") 
qqnorm(resid(octlm));qqline(resid(octlm),main="q-q plot glm") 
boxplot(sqrtoctgrow ~ timbersale, data=dendrodat,main="sqrtsepgrow~timbersale")

#not using site as random for now
##########GLMM (mixed model)
#treeid, spcode, site, aspect, sitequal, timbersale, dominance
maymixed <- lmer(sqrtmaygrow ~ timbersale + sitequal + (1|site), data=dendrodat)
summary(maymixed)
(sum(residuals(maymixed,type="pearson")^2))/25 #chisq over df--note that denominator is residual deviance df, will change for each model
plot(maymixed,main="residuals v fitted mixed")
qqnorm(resid(maymixed));qqline(resid(maymixed),main="q-q plot mixed") 


#################extraneous code snippets
#julglm <- glm(julgrowr ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat,family=poisson(link="log")) #poisson glm
#julglm <- glm.nb(julgrowr ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat)                         #negbin  glm
#plot(mayglm, which = 2,main="q-q plot")                                          #alternate way to plot q-q plots that identifies outliers
#hoslem.test(dendrodat$sqrtjungrow,fitted(junglm)) #goodness of fit (want p>0.05, no diff between model and observed data--not great for small sample sizes)