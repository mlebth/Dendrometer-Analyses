<<<<<<< HEAD
#note 11/28/2018--2018 data were mostly fine, but i divided the May growth by 2.
#this is because bands were zeroed on 4/10 and measured again on 5/31, which is closer to 2 months than 1.

# all measurements have been translated to cm
=======
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc

library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc);library(nlme);library(Rmisc)

#read-in 
<<<<<<< HEAD
dendrol <- read.csv('G:/Postdoc/FIG/Dendrometer/Dendrometer Analyses/dendrol-5.csv')
=======
dendrol <- read.csv('D:/Postdoc/FIG/Dendrometer/Dendrometer Analyses/dendrol-5.csv')
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc

#making month a factor variable
dendrol$month<-factor(dendrol$month, levels=c("may", "jun", "jul", "aug", "sep", "oct"))
#making numeric vars factors as needed
dendrol$treeid <- factor(dendrol$treeid)
dendrol$year <- factor(dendrol$year)
dendrol$spnum <- factor(dendrol$spnum)
dendrol$sitequal <- factor(dendrol$sitequal)

#log transform--adding one to deal with 0's--0 transforms to 0
dendrol$logmarba<-log(dendrol$marba+1)  ###0's---inf, not possible
#qqnorm(dendrol$logmarba,main="logmarba");qqline(dendrol$logmarba,main="logmarba") 

#hardwoods v softwoods
dendrol$group <- ifelse((dendrol$spcode=='QUMO'|dendrol$spcode=='QURU'|dendrol$spcode=='QUAL'|dendrol$spcode=='POGR'|dendrol$spcode=='CATO'|
                           dendrol$spcode=='ACRU'|dendrol$spcode=='BELE'|dendrol$spcode=='LITU'), 'hwood', 
                        ifelse((dendrol$spcode=='PIST'|dendrol$spcode=='TSCA'), 'swood',"NA"))
#seasonality variable
dendrol$season <- ifelse((dendrol$month=='may'|dendrol$month=='sep'|dendrol$month=='oct'),'Non-growing',
                         ifelse((dendrol$month=='jun'|dendrol$month=='jul'|dendrol$month=='aug'),'Growing',
                                "NA"))
<<<<<<< HEAD
#1/3/2019--making a new seasonality var because with new data, avg may growth is non-zero.
dendrol$season2 <- ifelse((dendrol$month=='sep'|dendrol$month=='oct'),'Non-growing',
                         ifelse((dendrol$month=='may'| dendrol$month=='jun'|dendrol$month=='jul'|dendrol$month=='aug'),'Growing',
                                "NA"))

dendrol18 <- dendrol[dendrol$year == 2018,]
=======
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc

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

modelaov <- aov(age ~ dominance, data=dendrol); summary(modelaov)

#exploring relationship between rainfall and season
modellm<- lm(rain ~ season, data=dendrol);summary(modellm) #p<0.0001 -- rainfall and season are positively related
boxplot(rain ~ season, data=dendrol,xlab="season",ylab="precip",main="precip-season")
ols_coll_diag(modellm)

#exploring relationship between temp and season
modellm<- lm(temp ~ season, data=dendrol);summary(modellm) #p<0.0001 -- temp and season are positively related
<<<<<<< HEAD
boxplot(temp ~ season, data=dendrol,xlab="season",ylab="temp",main="temp-season")

#exploring relationship between temp and rainfall
modellm<- lm(rain ~ temp, data=dendrol);summary(modellm) #p<0.0001 -- temp and season are positively related
boxplot(rain ~ temp, data=dendrol,xlab="temp",ylab="precip",main="precip-season")
=======
boxplot(temp ~ season, data=dendrol,xlab="season",ylab="precip",main="precip-season")

#exploring relationship between temp and rainfall
modellm<- lm(rain ~ temp, data=dendrol);summary(modellm) #p<0.0001 -- temp and season are positively related
boxplot(rain ~ temp, data=dendrol,xlab="rain",ylab="precip",main="precip-season")
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc

#exploring relationship between rainfall and month
modellm<- lm(rain ~ month, data=dendrol);summary(modellm);Anova(modellm) #p<0.0001 -- mostly in the growing season


#exploring relationship between age and initial size
dat2015 <- subset(dendrol,year == 2015); dat2016 <- subset(dendrol,year == 2016); dat2017 <- subset(dendrol,year == 2017)
<<<<<<< HEAD
xyplot(baselineinddbhcm~age, data=dat2015, groups=group, auto.key = TRUE)
modellm<- aov(baselineinddbhcm ~ age, data=dendrol);summary(modellm) #p<0.0001 -- age and size are positively related
=======
xyplot(baselineinddbh~age, data=dat2015, groups=group, auto.key = TRUE)
modellm<- aov(baselineinddbh ~ age, data=dendrol);summary(modellm) #p<0.0001 -- age and size are positively related
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
qqnorm(resid(modellm));qqline(resid(modellm)) 

#relationship between age and marginal growth
modellm<- aov(logmarba~age, data=dendrol);summary(modellm) #p=0.11 -- no relationship observed between age and marginal growth
xyplot(logmarba~age, data=dendrol, groups=group, auto.key = TRUE)
qqnorm(resid(modellm));qqline(resid(modellm)) 

#relationship between age and size each year
modellm<- lm(newdbh~age, data=dendrol);summary(modellm) #p=0.65 -- no relationship observed between age and size
xyplot(newdbh~age, data=dendrol, groups=group, auto.key = TRUE)
qqnorm(resid(modellm));qqline(resid(modellm)) 

#relationship size and month yeach year
dat2015 <- subset(dendrol,year == 2015); dat2016 <- subset(dendrol,year == 2016); dat2017 <- subset(dendrol,year == 2017)
xyplot(prevmeasdbh~month, data=dat2015, groups=group, auto.key = TRUE)
boxplot(prevmeasdbh~month, data=dat2015, groups=group, auto.key = TRUE)
modellm<- lm(prevmeasdbh~month + group, data=dendrol);summary(modellm); Anova(modellm)
qqnorm(resid(modellm));qqline(resid(modellm))

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
<<<<<<< HEAD
#baselinestandBAsqcm [num]   = stand basal area first installation (in inches) 
#baselineinddbhcm [num]    = DBH of individual at date of first installation (in inches) 
=======
#baselinestandBA [num]   = stand basal area first installation (in inches) 
#baselineinddbh [num]    = DBH of individual at date of first installation (in inches) 
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
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
#near                    = distance to treatment boundary
#azadj                   = adjusted azimuth (using Lisa Powers' formula)
#age                     = tree age (from cores)


########### all variables (without interactions)--testing for multicollinearity using VIF
<<<<<<< HEAD
modlmer <- lmer(logmarba ~ spcode + sitequal + timbersale + dominance + month + year + baselinestandBAsqcm + baselineinddbhcm + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=dendrol) #all variables
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBAsqcm + baselineinddbhcm + age + aspect + near + (1|site/treeid), 
                data=dendrol) #refined variables


modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBAsqcm + baselineinddbhcm + age + aspect + azadj 
=======
modlmer <- lmer(logmarba ~ spcode + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=dendrol) #all variables
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect + near + (1|site/treeid), 
                data=dendrol) #refined variables


modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect + azadj 
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
                + rain + temp + near + (1|site/treeid), data=dendrol) #all variables
vif.lme(modlmer) #cutoff = 5; round 1: month to season; #round 2: spcode to group

########### model selection (dendrol)
#try with either age or newdbh; and with azadj or aspect
<<<<<<< HEAD
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBAsqcm + baselineinddbhcm + age + aspect 
=======
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect 
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
                + rain + temp + near + (1|site/treeid), data=dendrol) 
AICc(modlmer) #age much better than newdbh, aspect slightly better than azadj

#######################
#to keep number of parameters/models down, using only interactions that might be biologically relevant (with terms directly related to the individuals,
#meaning 'group' and 'age':
#  group - sitequal, timbersale, season, year. not using group:aspect because there are not enough combinations in data to produce estimates
#  age   - sitequal, timbersale, season, year, aspect
#not testing 3-way interactions 
#######################
#global model
<<<<<<< HEAD
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
=======
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + baselineinddbh + baselinestandBA
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
                + group:sitequal + group:timbersale + group:year + group:season + group:near 
                + age:sitequal   + age:timbersale   + age:year   + age:season   + age:near   + age:aspect
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1686

modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect 
               + group:sitequal + group:timbersale + group:season
               + age:season  
               + (1|site/treeid), data=dendrol) 
AICc(mnull)
modellist<-list(mnull,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
modnames<-c("mnull","m1","m2","m3","m4","m5","m6","m7","m8","m9","m10")
aictab(modellist,modnames,second.ord=T)

<<<<<<< HEAD
###this is the one--final model! [pre-2018. see below for update]
=======
###this is the one--final model!
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect 
                + group:sitequal + group:timbersale + group:season 
                + age:season
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1682.46
#age-aspect(1674.45),age-near(1660.66),age-year(1616.57),age-timbersale(1609.40),age-sitequal(1597.38).
#group-near(1593.75), group-year(1583.24),
<<<<<<< HEAD
#baselinestandBAsqcm(1573.82), near(1561.41)
qqnorm(resid(modlmer));qqline(resid(modlmer)) 
summary(modlmer);Anova(modlmer,test.statistic="F")

########################2018 re-do with new data:
#global model (using orig season variable)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:year + group:season + group:near 
                + age:sitequal   + age:timbersale   + age:year   + age:season   + age:near   + age:aspect
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:2055.46

#using original seasonality variable
modlmer1 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect   
                 + group:sitequal + group:timbersale + group:season 
                 + age:season
                 + (1|site/treeid), data=dendrol) 
AICc(modlmer1) #original:2055.46
#age-aspect(2045.81),age-near(2028.64),age-year(1998.60),age-timbersale(1989.24),age-sitequal(1986.68).
#group-near(1977.51), group-year(1971.63),
#baselinestandBAsqcm(1947.43), baselineinddbhcm (1944.14), near(1933.9)
qqnorm(resid(modlmer1));qqline(resid(modlmer1)) 
summary(modlmer1);Anova(modlmer1,test.statistic="F")

#using new seasonality variable 
####KEEP THIS ONE, FINAL MODEL W/ 2018 DATA
modlmer2 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + year + age + aspect   
                 + group:sitequal + group:timbersale + group:season2
                 + (1|site/treeid), data=dendrol) 
AICc(modlmer2) #original:2064.72
#age-aspect(2055.09),age-near(2037.99),age-season2(2028.71), age-year(1999.17),age-timbersale(1989.82),age-sitequal(1986.93).
#group-near(1977.78), group-year(1972.23),
#baselinestandBAsqcm(1948.13), baselineinddbhcm (1944.50), near(1934.38)
qqnorm(resid(modlmer2));qqline(resid(modlmer2)) 
summary(modlmer2);Anova(modlmer2,test.statistic="F")

#to generate AIC table:
m11 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + year + age + aspect   
                + group:sitequal + group:timbersale + group:season2 
                + (1|site/treeid), data=dendrol) 
AICc(mnull)
modellist<-list(mnull,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)
modnames<-c("mnull","m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11")
aictab(modellist,modnames,second.ord=F)

#lsmeans for all categorical interactions
year <- emmeans(modlmer2, list(pairwise ~ year))
groupsitequal <- emmeans(modlmer2, list(pairwise ~ group|sitequal, pairwise~sitequal|group))
grouptimber   <- emmeans(modlmer2, list(pairwise ~ group|timbersale, pairwise~timbersale|group))
groupseason   <- emmeans(modlmer2, list(pairwise ~ group|season, pairwise~season|group))
dominance <- emmeans(modlmer2, list(pairwise ~ dominance))
age <- emmeans(modlmer2, list(pairwise ~ age))
aspect <- emmeans(modlmer2, list(pairwise ~ aspect))
=======
#baselinestandBA(1573.82), near(1561.41)
qqnorm(resid(modlmer));qqline(resid(modlmer)) 
summary(modlmer);Anova(modlmer,test.statistic="F")

#lsmeans for all categorical interactions
year <- emmeans(modlmer, list(pairwise ~ year))
groupsitequal <- emmeans(modlmer, list(pairwise ~ group|sitequal, pairwise~sitequal|group))
grouptimber   <- emmeans(modlmer, list(pairwise ~ group|timbersale, pairwise~timbersale|group))
groupseason   <- emmeans(modlmer, list(pairwise ~ group|season, pairwise~season|group))
dominance <- emmeans(modlmer, list(pairwise ~ dominance))
>>>>>>> 051ce002615bfd61497f1e21ef7372c3d1bd1cdc

#tables
with(dendrol, table(timbersale, group))
with(dendrol, table(season, group))
with(dendrol, table(year, group))

######visualizations of raw data--see 'dendrol-graphics-2.R' for final figures
#simple plots for visualization
boxplot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")
plot(logmarba ~ month, data=dendrol,xlab="Month",ylab="Marginal growth of log basal area",main="Month")
plot(logmarba ~ year, data=dendrol,xlab="Year",ylab="Marginal growth of log basal area",main="Year")
plot(logmarba ~ spcode, data=dendrol,xlab="Timbersale",ylab="May growth",main="May growth~timbersale")
plot(logmarba ~ timbersale, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ month, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ year, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ rain, data=dendrol,xlab="Rainfall",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")

#sig. interactions: group and: timbersale, season, year; season:age
#age and season (scatterplot)
ggplot(dendrol, aes(x=age, y=logmarba, color=season)) + 
  geom_point(size=3, alpha=0.75) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_color_manual(values=c("forestgreen", "gray70"),name="Season",breaks = c("gro", "ngr"), labels=c("Growing", "Non-growing")) +
  labs(x = "Age", y="Marginal growth of log basal area", title="Age*Season")

#timbersale and group
ggplot(dendrol, aes(x=timbersale, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_x_discrete(labels=c("Unthinned", "Thinned")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),name="Group",breaks = c("hwood", "pplar", "swood"), labels=c("Hardwood", "Poplar", "Softwood")) +
  labs(x = "Timbersale", y="Marginal growth of log basal area", title="Group*Timbersale") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  

#season and group
ggplot(dendrol, aes(x=season, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_x_discrete(labels=c("Growing season", "Non-growing season")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),name="Group",breaks = c("hwood", "pplar", "swood"), labels=c("Hardwood", "Poplar", "Softwood")) +
  labs(x = "Season", y="Marginal growth of log basal area", title="Group*Season") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  

#year and group
ggplot(dendrol, aes(x=year, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),name="Group",breaks = c("hwood", "pplar", "swood"), labels=c("Hardwood", "Poplar", "Softwood")) +
  labs(x = "Year", y="Marginal growth of log basal area", title="Group*Year") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge   