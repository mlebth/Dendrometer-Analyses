#grouping litu in with hardwoods.
dendrol$group <- ifelse((dendrol$spcode=='QUMO'|dendrol$spcode=='QURU'|dendrol$spcode=='QUAL'|dendrol$spcode=='POGR'|dendrol$spcode=='CATO'|
                           dendrol$spcode=='ACRU'|dendrol$spcode=='BELE'|dendrol$spcode=='LITU'), 'hwood', 
                        ifelse((dendrol$spcode=='PIST'|dendrol$spcode=='TSCA'), 'swood',"NA"))

modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near  + rain  
                + age:group
                + group:sitequal + group:timbersale + group:season + group:near 
                + age:season  
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) 
#age-aspect(1709),age-near(1694),age-rain(1682),age-temp(1667),age-year(1648),age-timbersale(1641),age-sitequal(1631).
#group-rain(1624),group-temp(1612),group-year(1610),
#baselinestandBA(1602),baselineinddbh(1599),temp(1595)
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#lsmeans fpr all categorical interactions
sitequal <- lsmeans(modlmer, list(pairwise ~ sitequal))
grouptimber <- lsmeans(modlmer, list(pairwise ~ group|timbersale))
groupseason <- lsmeans(modlmer, list(pairwise ~ group|season))
groupyear   <- lsmeans(modlmer, list(pairwise ~ group|year))


###this is the one--final model!
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + rain 
                + group:sitequal + group:timbersale + group:season 
                + age:season 
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1647
#age-aspect(1714),age-near(1699),age-rain(1686),age-temp(1672),age-year(1654),age-timbersale(1641),age-sitequal(1636).
#group-rain(1633),group-temp(1627),group-year(1623),
#baselinestandBA(1614),baselineinddbh(1612),temp(1608)
#removing more interactions: age-group(1606) [removed because it doesn't make sense], group-near(1603), near (1594)

qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#lsmeans fpr all categorical interactions
year <- lsmeans(modlmer, list(pairwise ~ year))
groupsitequal <- emmeans(modlmer, list(pairwise ~ group|sitequal, pairwise~sitequal|group))
grouptimber   <- lsmeans(modlmer, list(pairwise ~ group|timbersale, pairwise~timbersale|group))
groupseason   <- lsmeans(modlmer, list(pairwise ~ group|season, pairwise~season|group))



#lsmeans fpr all categorical interactions
timbersale <- lsmeans(modlmer, list(pairwise ~ timbersale))
season   <- lsmeans(modlmer, list(pairwise ~ season))
year   <- lsmeans(modlmer, list(pairwise ~ year))


#rainfall and season

### 85% confidence intervals instead of 95%
#### overwinter change/total annual change -- either total annual, or october-april--include (annual) precipitation


library(jtools)
#plotting continuous v categorical interactions painlessly:
interact_plot(modlmer,pred="age",modx="season",plot.points=TRUE,
              x.label="Age", y.label="Marginal growth of log basal area",
              main.title="Age x Season", legend.main="Season") +
              theme_minimal() + 
              theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) 


interact_plot(modlmer,pred="rain",modx="group",plot.points=TRUE,
              x.label="rain", y.label="Marginal growth of log basal area",
              main.title="rain x group", legend.main="group") +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) 

#not working: scale_colour_discrete(name="Season",breaks=c("gro","ngr"),labels=c("Growing", "Non-Growing")) 

boxplot(rain ~ season, data=dendrol,xlab="Season",ylab="Total precipitation (cm)",main="Season-rain")
boxplot(rain ~ month, data=dendrol,xlab="Month",ylab="Total precipitation (cm)",main="Month-rain")

cat_plot(modlmer, pred = timbersale, modx = group, interval = FALSE, plot.points = TRUE)
cat_plot(modlmer, pred = timbersale, modx = group)
#sig. interactions: group and: timbersale, season

#timbersale and group
ggplot(dendrol, aes(x=timbersale, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_x_discrete(labels=c("Unthinned", "Thinned")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood")) +
  labs(x = "Timbersale", y="Marginal growth of log basal area", title="Group*Timbersale") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  

pd=position_dodge(0.4)
#######group x timbersale
mean<-c(2.301423,-0.375254,2.975134,6.296589);cldown<-c(-0.2581612,-3.9584525,0.5501371,2.5181204)
clup<-c(4.861008,3.207944,5.400132,10.075058)
timber<-c("Unthinned","Unthinned","Thinned","Thinned");group<-c("Hardwood","Softwood","Hardwood","Softwood")
timbergroup.dat <- data.frame(timber,group,mean,cldown,clup)

ggplot(timbergroup.dat, aes(x=timber, y=mean, color=group)) +  
  geom_point(shape=15, size=3, position=pd) +  
  geom_errorbar(width=.2, size=1,aes(ymin=cldown, ymax=clup),position=pd) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + 
  labs(x="Timbersale",y="Log of marginal basal area",title="Thin X Group") +
  scale_color_manual(values = c("#999999", "#56B4E9"),name="Group")

#######group x season
mean<-c(3.902353,3.346368,1.374205,2.574967);cldown<-c(1.0896177,-0.1568609,-1.4476591,-0.9016096)
clup<-c(6.715089,6.849597,4.196068,6.051544)
season<-c("Growing","Growing","Non-growing","Non-growing");group<-c("Hardwood","Softwood","Hardwood","Softwood")
seasongroup.dat <- data.frame(season,group,mean,cldown,clup)

ggplot(seasongroup.dat, aes(x=season, y=mean, color=group)) +  
  geom_point(shape=15, size=3, position=pd) +  
  geom_errorbar(width=.2, size=1,aes(ymin=cldown, ymax=clup),position=pd) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + 
  labs(x="Season",y="Log of marginal basal area",title="Season X Group") +
  scale_color_manual(values = c("#999999", "#56B4E9"),name="Group")


#######group x sitequal #not done
mean<-c(2.8872994
,3.8858533
,3.3017107
,2.574967);cldown<-c(0.01214853,0.24086871
,-0.25306614
,-0.9016096)
clup<-c(5.76245
,7.530838
,6.856488
,6.051544)
sitequal<-c("1","1","2","2","3","3");group<-c("hwood","swood","hwood","swood","hwood","swood")
sitequalgroup.dat <- data.frame(sitequal,group,mean,cldown,clup)

ggplot(seasongroup.dat, aes(x=sitequal, y=mean, color=group)) +  
  geom_point(shape=21, size=3, position=pd) +  
  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup),position=pd) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) + 
  labs(x="Sitequal",y="Log of marginal basal area",title="Sitequal X Group")

#season and group
ggplot(dendrol, aes(x=season, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_x_discrete(labels=c("Growing season", "Non-growing season")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood")) +
  labs(x = "Season", y="Marginal growth of log basal area", title="Group*Season") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  

#site quality and group
ggplot(dendrol, aes(x=sitequal, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_x_discrete(labels=c("1", "2", "3")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood")) +
  labs(x = "Site Quality", y="Marginal growth of log basal area", title="Group*Site quality") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  

#age and season (scatterplot)
ggplot(dendrol, aes(x=age, y=logmarba, color=season)) + 
  geom_point(size=3, alpha=0.75) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_color_manual(values=c("forestgreen", "gray70"),name="Season",breaks = c("gro", "ngr"), labels=c("Growing", "Non-growing")) +
  labs(x = "Age", y="Marginal growth of log basal area", title="Age*Season")

#rain (scatterplot)
ggplot(dendrol, aes(x=rain, y=logmarba, color=rain)) + 
  geom_point(size=3, alpha=0.75) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5))  +
  labs(x = "Monthly Precipitation", y="Marginal growth of log basal area", title="Effect of rainfall")

#year
ggplot(dendrol, aes(x=year, y=logmarba, fill=year)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  labs(x = "Year", y="Marginal growth of log basal area", title="Effect of year") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge  