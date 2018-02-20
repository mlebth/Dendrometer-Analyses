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
groupsitequal <- lsmeans(modlmer, list(pairwise ~ group|sitequal, pairwise~sitequal|group))
grouptimber   <- lsmeans(modlmer, list(pairwise ~ group|timbersale, pairwise~timbersale|group))
groupseason   <- lsmeans(modlmer, list(pairwise ~ group|season, pairwise~season|group))



#lsmeans fpr all categorical interactions
timbersale <- lsmeans(modlmer, list(pairwise ~ timbersale))
season   <- lsmeans(modlmer, list(pairwise ~ season))
year   <- lsmeans(modlmer, list(pairwise ~ year))


#rainfall and season


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