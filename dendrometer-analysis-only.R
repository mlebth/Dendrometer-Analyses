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
boxplot(dendrodat$maygrow,dendrodat$jungrow,dendrodat$julgrow,dendrodat$auggrow,dendrodat$sepgrow,dendrodat$octgrow,main="allmonths") 
#outliers present all but jun
plot(dendrodat$maygrow,main="maygrow");plot(dendrodat$jungrow,main="jungrow");plot(dendrodat$julgrow,main="julgrow");plot(dendrodat$auggrow,main="auggrow")
plot(dendrodat$sepgrow,main="sepgrow");plot(dendrodat$octgrow,main="octgrow")   
#q-q plots
qqnorm(dendrodat$maygrow,main="maygrow");qqline(dendrodat$maygrow,main="maygrow")  #right skew
qqnorm(dendrodat$jungrow,main="jungrow");qqline(dendrodat$jungrow,main="jungrow")  #light-tailed
qqnorm(dendrodat$julgrow,main="julgrow");qqline(dendrodat$julgrow,main="julgrow")  #right skew
qqnorm(dendrodat$auggrow,main="auggrow");qqline(dendrodat$auggrow,main="auggrow")  #right skew
qqnorm(dendrodat$sepgrow,main="sepgrow");qqline(dendrodat$sepgrow,main="sepgrow")  #many 0's
qqnorm(dendrodat$octgrow,main="octgrow");qqline(dendrodat$octgrow,main="octgrow")  #many 0's

#log transform--adding one to deal with 0's--0 transforms to 0
dendrodat$logmaygrow<-log(dendrodat$maygrow+1);dendrodat$logjungrow<-log(dendrodat$jungrow+1);dendrodat$logjulgrow<-log(dendrodat$julgrow+1)
dendrodat$logauggrow<-log(dendrodat$auggrow+1);dendrodat$logsepgrow<-log(dendrodat$sepgrow+1);dendrodat$logoctgrow<-log(dendrodat$octgrow+1)
#outliers present all but jun
plot(dendrodat$logmaygrow,main="logmaygrow");plot(dendrodat$logjungrow,main="logjungrow");plot(dendrodat$logjulgrow,main="logjulgrow")
plot(dendrodat$logauggrow,main="logauggrow");plot(dendrodat$logsepgrow,main="logsepgrow");plot(dendrodat$logoctgrow,main="logoctgrow")
#q-q plots
qqnorm(dendrodat$logmaygrow,main="logmaygrow");qqline(dendrodat$logmaygrow,main="logmaygrow") #better than untransformed
qqnorm(dendrodat$logjungrow,main="logjungrow");qqline(dendrodat$logjungrow,main="logjungrow") #worse than untransformed
qqnorm(dendrodat$logjulgrow,main="logjulgrow");qqline(dendrodat$logjulgrow,main="logjulgrow") #better than untransformed  
qqnorm(dendrodat$logauggrow,main="logauggrow");qqline(dendrodat$logauggrow,main="logauggrow") #better than untransformed
qqnorm(dendrodat$logsepgrow,main="logsepgrow");qqline(dendrodat$logsepgrow,main="logsepgrow") #slightly better than untransformed
qqnorm(dendrodat$logoctgrow,main="logoctgrow");qqline(dendrodat$logoctgrow,main="logoctgrow") #better than untransformed

##########sqrt transform--best transformations
dendrodat$sqrtmaygrow<-sqrt(dendrodat$maygrow);dendrodat$sqrtjungrow<-sqrt(dendrodat$jungrow);dendrodat$sqrtjulgrow<-sqrt(dendrodat$julgrow)
dendrodat$sqrtauggrow<-sqrt(dendrodat$auggrow);dendrodat$sqrtsepgrow<-sqrt(dendrodat$sepgrow);dendrodat$sqrtoctgrow<-sqrt(dendrodat$octgrow)
#outliers present all but jun
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
#spcode, site, aspect, sitequal, timbersale, dominance, baselinedbh, rain, temp

junglm <- glm(sqrtjungrow ~ timbersale + sitequal + aspect + baselinedbh + junrain + juntemp + baselinedbh*junrain + baselinedbh*juntemp + junrain*juntemp, data=dendrodat)

#gaussian
junglm <- glm(sqrtjungrow ~ timbersale + sitequal + aspect + baselinedbh + junrain + juntemp + baselinedbh*juntemp , data=dendrodat)
summary(junglm)
plot(junglm, which = 1,main="residuals v fitted glm") 
qqnorm(resid(junglm));qqline(resid(junglm),main="q-q plot glm") 
boxplot(sqrtjungrow ~ site, data=dendrodat,main="sqrtjungrow~site")


##########GLMM (mixed model)
#treeid, spcode, site, aspect, sitequal, timbersale, dominance
maymixed <- lmer(sqrtmaygrow ~ timbersale + sitequal + mayrain + maytemp + (1|site), data=dendrodat)
summary(maymixed)
(sum(residuals(maymixed,type="pearson")^2))/25 #chisq over df--note that denominator is residual deviance df, will change for each model
plot(maymixed,main="residuals v fitted mixed")
qqnorm(resid(maymixed));qqline(resid(maymixed),main="q-q plot mixed") 




#################extraneous code snippets
#julglm <- glm(julgrowr ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat,family=poisson(link="log")) #poisson glm
#julglm <- glm.nb(julgrowr ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat)                         #negbin  glm
#plot(mayglm, which = 2,main="q-q plot")                                          #alternate way to plot q-q plots that identifies outliers
#hoslem.test(dendrodat$sqrtjungrow,fitted(junglm)) #goodness of fit (want p>0.05, no diff between model and observed data--not great for small sample sizes)