#rainfall and season

### 85% confidence intervals instead of 95%
#### overwinter change/total annual change -- either total annual, or october-april--include (annual) precipitation
#how to test whether individual slopes differ from 0?

library(jtools) #plotting continuous v categorical interactions painlessly:
#age and season
interact_plot(modlmer,pred="age",modx="season",plot.points=TRUE,x.label="Tree age", y.label="Marginal growth of log basal area",
              main.title="Age x Season", legend.main="Season") +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid"),plot.title = element_text(size=18)) 

#rainfall
ggplot(dendrol, aes(x=rain, y=logmarba)) + 
  geom_point(shape=16, size=2, color="blue3") +
  geom_abline(aes(intercept=5.510796,slope=-0.159328)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-0.5,8.5)) + 
  labs(x = "Total monthly precipitation", y="Marginal growth of log basal area", title="Precipitation")

library(multcompView)
#timbersale x group
marginal = emmeans(modlmer, ~ timbersale:group)
CLD = cld(marginal, alpha=0.05, Letters=letters)
### Order the levels for printing
CLD$timbersale = factor(CLD$timbersale, levels=c("N", "Y"))
CLD$group = factor(CLD$group, levels=c("hwood", "swood")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#timbersale x group
ggplot(CLD, aes(x=timbersale, y=emmean, color=group, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18)) +
  scale_x_discrete(breaks=c("N","Y"),labels=c("Unthinned", "Thinned")) +
  labs(x="Treatment",y="Log of marginal basal area",title="Treatment X Group") +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, 0.1), nudge_y = c(4.1, 3.15, 3, 4.25), color = "black") 

#######group x season
marginal = emmeans(modlmer, ~ season:group)
CLD = cld(marginal, alpha=0.05, Letters=letters)
### Order the levels for printing
CLD$season = factor(CLD$season, levels=c("Non-growing", "Growing"))
CLD$group = factor(CLD$group, levels=c("hwood", "swood")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#season x group
ggplot(CLD, aes(x=season, y=emmean, color=group, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18))  + 
  labs(x="Season",y="Log of marginal basal area",title="Season X Group") +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, 0.1), nudge_y = c(4.95, 2.1, 3.8, 3.3), color = "black") 

#######group x sitequal
marginal = emmeans(modlmer, ~ sitequal:group)
CLD = cld(marginal, alpha=0.05, Letters=letters)
### Order the levels for printing
CLD$sitequal = factor(CLD$sitequal, levels=c("1", "2", "3"))
CLD$group = factor(CLD$group, levels=c("hwood", "swood")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#sitequal x group
ggplot(CLD, aes(x=sitequal, y=emmean, color=group, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18))  + 
  labs(x="Site Quality",y="Log of marginal basal area",title="Site Quality X Group") +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Hardwood", "Softwood"))

#year
marginal = emmeans(modlmer, ~ year)
CLD = cld(marginal, alpha=0.05, Letters=letters)
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group);CLD
pd=position_dodge(0.4)
#sitequal x group
ggplot(CLD, aes(x=year, y=emmean, label=.group)) +  
  geom_point(shape=15, size=4, position=pd, color="forestgreen") +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd, color="forestgreen") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18))  + 
  labs(x="Year",y="Log of marginal basal area",title="Year") +
  geom_text(nudge_y = c(3.2, 3.3, 3.15), color = "black") 

#######boxplots
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

#year
ggplot(dendrol, aes(x=year, y=logmarba, fill=year)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  labs(x = "Year", y="Marginal growth of log basal area", title="Effect of year") +
  geom_point(position = position_jitterdodge(jitter.width=.0035, dodge.width=0.75))  #jitter and dodge 

#rain (scatterplot)
ggplot(dendrol, aes(x=rain, y=logmarba, color=rain)) + 
  geom_point(size=3, alpha=0.75) +
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5))  +
  labs(x = "Monthly Precipitation", y="Marginal growth of log basal area", title="Effect of rainfall")
 
#stripchart
plot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")
stripchart(logmarba ~ sitequal, vertical=TRUE, data=dendrol, method="jitter", jitter=0.175, add=TRUE, pch=20, col="blue", xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")

#plot of random effect coefficients (treeid)
growth_subject <- fixef(modlmer) + ranef(modlmer)$treeid
growth_subject$treeid<-rownames(growth_subject)
names(growth_subject)[1]<-"Intercept"
growth_subject <- growth_subject[,c(2,1)]
ggplot(growth_subject,aes(x=treeid,y=Intercept))+geom_point()