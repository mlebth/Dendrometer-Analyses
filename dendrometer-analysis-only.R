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

################## visualizations
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

###################models
#t-test for each month by timbersale
#these group growth rates of all species by month
mayttest <- t.test(maygrow ~ timbersale, data=dendrodat)
junttest <- t.test(jungrow ~ timbersale, data=dendrodat)
julttest <- t.test(julgrow ~ timbersale, data=dendrodat)
augttest <- t.test(auggrow ~ timbersale, data=dendrodat)
septtest <- t.test(sepgrow ~ timbersale, data=dendrodat)
octttest <- t.test(octgrow ~ timbersale, data=dendrodat)

#GLM
#family=poisson
julglm <- glm(jul15growint ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat,family=poisson(link="log"))
mayglm <- glm(may15grow ~ timbersale + sitequal + baselinedbh + mayrain + maytemp, data=dendrodat,family=poisson(link="log"))
#negative binomial
mayglm <- glm.nb(jun16grow ~ timbersale +  aspect , data=dendrodat)
summary(julglm )
(sum(residuals(julglm ,type="pearson")^2))/6 #chisq over df--note that denominator is residual deviance df, will change for each model
plot(julglm , which = 1) #residuals v fitted
plot(julglm , which = 2) #q-q plot
boxplot(jul15growint ~ site, data=dendrodat)

#GLMM (mixed model)
#treeid, spcode, site, aspect, sitequal, timbersale, dominance
julmixed <- lmer(julgrow ~ timbersale + spcode + sitequal + (1|site), data=dendrodat)
summary(julmixed)
plot(julmixed)
qqnorm(resid(julmixed))
qqline(resid(julmixed)) 

