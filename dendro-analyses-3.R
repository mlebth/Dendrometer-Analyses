
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)
library(PerformanceAnalytics);library(Hmisc);library(nlme);library(Rmisc)

#read-in 
dendrol <- read.csv('F:/TU/FIG/Dendrometer/Dendrometer Analyses/dendrol-5.csv')

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

#exploring relationship between rainfall and season
modellm<- lm(rain ~ season, data=dendrol);summary(modellm) #p<0.0001 -- rainfall and season are positively related

#exploring relationship between rainfall and month
modellm<- lm(rain ~ month, data=dendrol);summary(modellm) #p<0.0001 -- mostly in the growting season


#exploring relationship between age and initial size
dat2015 <- subset(dendrol,year == 2015); dat2016 <- subset(dendrol,year == 2016); dat2017 <- subset(dendrol,year == 2017)
xyplot(baselineinddbh~age, data=dat2015, groups=group, auto.key = TRUE)
modelaov<- aov(baselineinddbh ~ age, data=dendrol);summary(modelaov) #p<0.0001 -- age and size are positively related
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and marginal growth
modelaov<- aov(logmarba~age, data=dendrol);summary(modelaov) #p=0.11 -- no relationship observed between age and marginal growth
xyplot(logmarba~age, data=dendrol, groups=group, auto.key = TRUE)
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

#relationship between age and size each year
modelaov<- aov(newdbh~age, data=dendrol);summary(modelaov) #p=0.65 -- no relationship between age and size
xyplot(newdbh~age, data=dendrol, groups=group, auto.key = TRUE)
qqnorm(resid(modelaov));qqline(resid(modelaov)) 

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
                + group:sitequal + group:timbersale + group:year + group:season + group:temp + group:rain + group:near 
                + age:sitequal   + age:timbersale   + age:year   + age:season   + age:temp   + age:rain   + age:near   + age:aspect
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
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#lsmeans fpr all categorical interactions
grouptimber <- lsmeans(modlmer, list(pairwise ~ group|timbersale))
groupseason <- lsmeans(modlmer, list(pairwise ~ group|season))
groupyear   <- lsmeans(modlmer, list(pairwise ~ group|year))

#tables
with(dendrol, table(timbersale, group))
with(dendrol, table(season, group))
with(dendrol, table(year, group))


#sig. interactions: group and: timbersale, season, year; season:age
#age and season
ggplot(dendrol, aes(x=age, y=logmarba, fill=season)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) 
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


#summary of results for summer and dendro:
# positive with sitequal and rain (also with timbersale, season and year, but those are all in interactions)
# interactions:
#   season:age (younger trees grow faster in summer)
#   group:timbersale (softwood grow fastest in thinned areas)
#   group:season (poplar grow fastest in growing season)
#   group:year (hardwood grow most in 2015, poplar in 2015 and 2016, softwood in 2015 and 2016)