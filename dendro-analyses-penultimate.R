
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc);library(nlme);library(Rmisc)

#read-in 
dendrol <- read.csv('F:/TU/FIG/Dendrometer/Dendrometer Analyses/dendrol-4.csv')

#making month a factor variable
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
modelaov<- aov(baselineinddbh ~ age, data=dendrol);summary(modelaov) #p<0.0001 -- age and size are positively related
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and marginal growth
modelaov<- aov(logmarba~age, data=dendrol);summary(modelaov) #p=0.25 -- no relationship observed between age and marginal growth
xyplot(logmarba~age, data=dendrol, groups=group, auto.key = TRUE)
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and size each year
modelaov<- aov(newdbh~age, data=dendrol);summary(modelaov) #p=0.001 -- positive relationship between age and size each year
xyplot(newdbh~age, data=dendrol, groups=group, auto.key = TRUE)
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
#near                    = distance to treatment boundary
#azadj                   = adjusted azimuth (using Lisa Powers' formula)
#age                     = tree age (from cores)


#########add figure: month by month and by timbersale

########### all variables (without interactions)--testing for multicollinearity using VIF
modlmer <- lmer(logmarba ~ spcode + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=dendrol) #all variables
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=dendrol) #refined variables
vif.lme(modlmer) #cutoff = 5; round 1: month to season; #round 2: spcode to group

########### model selection (dendrol)
#try with either age or newdbh; and with azadj or aspect
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + baselinestandBA + baselineinddbh + age + aspect 
                + rain + temp + near + (1|site/treeid), data=dendrol) 
AICc(modlmer) #age much better than newdbh, aspect slightly better than azadj

#######################
#to keep number of parameters/models down, using only interactions that might be biologically relevant (with terms directly related to the individuals,
#meaning 'group' and 'age':
#  group - sitequal, timbersale, season, year. not using group:aspect because there are not enough combinations in data to produce estimates
#  age   - sitequal, timbersale, season, year, aspect
#not testing 3-way interactions 
#######################
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group
                + age:season
                + group:sitequal + group:timbersale + group:season + group:year + group:temp + group:near + group:rain
                + age:sitequal + age:timbersale + age:year + age:aspect + age:temp + age:near + age:rain
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1611

#let the selection process begin:
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect 
                + group:sitequal + group:timbersale + group:season + group:year + age:season 
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) 
#-age:rain (1593), -age:near (1583), -age:temp (1569), -age:aspect (1564), -age:year (1545), -age:timbersale (1537), -age:sitequal (1527)
#-group:rain (1520), -group:temp (1507), -age:group (1502), -group:near (1499)
#-baselinestandBA (1490), -baselineinddbh (1486), -rain (1484), -temp (1479), -near (1472)
#NOTE I did try using newdbh (size) instead of age--resulted in poorer fits

qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#sig. interactions: group and: timbersale, season, year; season:age
ggplot(dendrol, aes(x=age, y=logmarba, fill=season)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) 


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
                

#summer models

########### all variables (without interactions)--testing for multicollinearity using VIF
modlmer <- lmer(logmarba ~ spcode + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=summer) #all variables
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=summer) #refined variables
vif.lme(modlmer) #cutoff = 5; round 1: spcode to group


modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group
                + age:month
                + group:sitequal + group:timbersale + group:year + group:temp + group:near + group:rain
                + age:sitequal + age:timbersale + age:year + age:aspect + age:temp + age:near + age:rain
                + (1|site/treeid), data=summer) 
AICc(modlmer) #original:920.15

#let the selection process begin:
summermod <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + age + aspect 
                + group:sitequal + group:timbersale + group:year 
                + (1|site/treeid), data=summer) 
AICc(summermod) 
#-age:rain (769), -age:near (875), -age:temp (866), -age:aspect (861), -age:year (842), -age:timbersale (835), -age:sitequal (825)
#-group:rain (818), -group:temp (810), -age:group (804), -group:near (801)
#-baselinestandBA (792), -baselineinddbh (787), -temp (780), -near (769), -rain (767)

qqnorm(resid(summermod));qqline(resid(summermod),main="q-q plot mixed") 
summary(summermod);Anova(summermod)

#sig. interactions: group and: timbersale, year
ggplot(summer, aes(x=timbersale, y=logmarba, fill=group)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) 

ggplot(summer, aes(x=year, y=logmarba, fill=group)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) 

#summary of results for summer and dendro:
#in dendrol:
#   positive with sitequal, timbersale, season, and year (all but sitequal in interactions)
#   interaction between group and timbersale, group and year, group and season
#   interaction between season and age
#in summer:
#   positive with sitequal, timbersale, month, year, and age
#   interaction between group and timbersale, group and year

#summary for both:
#   positive with sitequal. interactions between group and timbersale and group and year.
#   positive with age *only* in summer.
#   within summer, positive with month.





########################using new age

modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group
                + age:season
                + group:sitequal + group:timbersale + group:season + group:year + group:temp + group:near + group:rain
                + age:sitequal + age:timbersale + age:year + age:aspect + age:temp + age:near + age:rain
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1713

#let the selection process begin:
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + rain
                + age:season
                + group:sitequal + group:timbersale + group:season + group:year 
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) 
#-age:rain (1700), -age:near (1685), -age:temp (1670), -age:aspect (1667), -age:year (1648), -age:timbersale (1641), -age:sitequal (1631)
#-group:rain (1624), -group:temp (1612), -age:group (1606), -group:near (1602)
#-baselinestandBA (1593), -baselineinddbh (1590), -temp (1586), -near (1577)
#NOTE I did try using newdbh (size) instead of age--resulted in poorer fits

#summer
summermod <- lmer(logmarba ~ spcode + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=summer) #all variables
summermod <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + baselinestandBA + baselineinddbh + newdbh + age + aspect + azadj 
                + rain + temp + near + (1|site/treeid), data=summer) #refined variables
vif.lme(summermod) #cutoff = 5; round 1: spcode to group

summermod <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group
                + age:month
                + group:sitequal + group:timbersale + group:year + group:temp + group:near + group:rain
                + age:sitequal + age:timbersale + age:year + age:aspect + age:temp + age:near + age:rain
                + (1|site/treeid), data=summer) 
AICc(summermod) #original:971

#let the selection process begin:
summermod <- lmer(logmarba ~ group + sitequal + timbersale + dominance + month + year + age + aspect +  rain 
                + group:sitequal + group:timbersale + group:year 
                + (1|site/treeid), data=summer) 
AICc(summermod) 
#-age:rain (958), -age:near (942), -age:temp (929), -age:aspect (925), -age:year (905), -age:timbersale (897), -age:sitequal (887)
#-group:rain (881), -group:temp (871), -age:group (866), -group:near (862), -age:month (850)
#-baselinestandBA (841), -baselineinddbh (836), -temp (830), -near (822)