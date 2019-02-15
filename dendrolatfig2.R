
#rainfall and season

### 85% confidence intervals instead of 95%
#### overwinter change/total annual change -- either total annual, or october-april--include (annual) precipitation
###########!!!!!!!!!!!!!!!!!!!!don't have og basal area each may! this throws everything off!?!?!?

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
mean<-c(2.3726684,-0.3866388,2.9567032,6.2449292);cldown<-c(-0.2436976,-3.9643272,0.4601439,2.4725502)
clup<-c(4.989034,3.191050,5.453263,10.017308)
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
mean<-c(3.930775,3.337417,1.398597,2.520874);cldown<-c(1.0371065,-0.2201744,-1.5042022,-1.0088933)
clup<-c(6.824443,6.895008,4.301396,6.050641)
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
mean<-c(2.8065526,3.7922418,3.3375581,5.4536631,1.8499467,-0.4584693);cldown<-c(-0.18309242,0.08996833,-0.27653444,-1.59609025,-1.90363590,-6.17243974)
clup<-c(5.796198,7.494515,6.951651,12.503416,5.603529,5.255501)
sitequal<-c("1","1","2","2","3","3");group<-c("hwood","swood","hwood","swood","hwood","swood")
sitequalgroup.dat <- data.frame(sitequal,group,mean,cldown,clup)

ggplot(sitequalgroup.dat, aes(x=sitequal, y=mean, color=group))  +  
  geom_point(shape=15, size=3, position=pd) +  
  geom_errorbar(width=.2, size=1,aes(ymin=cldown, ymax=clup),position=pd) +
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