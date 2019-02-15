#rainfall and season

### 85% confidence intervals instead of 95%
#### overwinter change/total annual change -- either total annual, or october-april--include (annual) precipitation
#how to test whether individual slopes differ from 0?

#age and season
ggplot(dendrol, aes(x=age, y=logmarba, color=season)) + 
  geom_point(shape=19, size=2.3,position=position_jitter(w=0.1, h=0.1)) +
  geom_abline(aes(intercept=4.15568,slope=-0.02622,color="Growing"),size=1,linetype=1) +
  geom_abline(aes(intercept=-0.1167,slope=0.00273,color="Non-growing"),size=1,linetype=2) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18), text = element_text(size=16)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-0.5,6.1)) + 
  labs(x = "Tree age", y="Marginal growth of log basal area", title="Age x Season") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  scale_color_manual(values=c("green4", "navajowhite3"),name="Season",breaks = c("Growing", "Non-growing"), labels=c("Growing", "Non-growing")) 


#age
ggplot(dendrol, aes(x=age, y=logmarba)) + 
  geom_point(shape=16, size=2, color="blue3") +
  geom_abline(aes(intercept=2.079877,slope=-0.007308)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-0.5,8.5)) + 
  labs(x = "Age", y="Marginal growth of log basal area", title="Precipitation")


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
marginal = emmeans(modlmer2, ~ timbersale:group)
CLD = cld(marginal, alpha=0.05, Letters=letters, level=0.85)
### Order the levels for printing
CLD$timbersale = factor(CLD$timbersale, levels=c("N", "Y"))
CLD$group = factor(CLD$group, levels=c("hwood", "swood")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#for back-transformation--not using
# CLD$backmean <- (exp(1)^CLD$emmean);CLD$backdown <- (exp(1)^CLD$lower.CL);CLD$backup <- (exp(1)^CLD$upper.CL)
#timbersale x group
ggplot(CLD, aes(x=timbersale, y=emmean, color=group, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18), text = element_text(size=16)) +
  scale_x_discrete(breaks=c("N","Y"),labels=c("Unthinned", "Thinned")) +
  labs(x="Treatment",y="Log of marginal basal area",title="Treatment X Group") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Broadleaf", "Conifer")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, 0.1), nudge_y = c(2.5, 2, 1.9, 2.55), color = "black") 

  
#######group x season
marginal = emmeans(modlmer2, ~ season2:group)
CLD = cld(marginal, alpha=0.05, Letters=letters, level=0.85)
### Order the levels for printing
CLD$season2 = factor(CLD$season2, levels=c("Non-growing", "Growing"))
CLD$group = factor(CLD$group, levels=c("hwood", "swood")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#season x group
ggplot(CLD, aes(x=season2, y=emmean, color=group, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18), text = element_text(size=16))  + 
  labs(x="Season",y="Log of marginal basal area",title="Season X Group") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Broadleaf", "Conifer")) +
  geom_text(nudge_x = c(-0.1,0.1, 0.1, -0.1), nudge_y = c(1.75, 2.15, 2.1, 1.8), color = "black") 

#######group x sitequal

###work on this

marginal = emmeans(modlmer2, ~ sitequal:group)
CLD = cld(marginal, alpha=0.05, Letters=letters, level=0.85)
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
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Broadleaf", "Conifer")) +
  geom_text(color="black")
  scale_x_discrete(limits=c("3","2","1"), labels=c("3"="Low","2"="Moderate","1"="High")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, -0.1, 0.1, 0.1), nudge_y = c(3.8, .8, 2.75, 1.8, 0.8, 3.1), color = "black") 

#year
marginal = emmeans(modlmer2, ~ year)
CLD = cld(marginal, alpha=0.05, Letters=letters, level = 0.85)
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group);CLD
pd=position_dodge(0.4)
#for back-transformation
#CLD$backmean <- (exp(1)^CLD$emmean);CLD$backdown <- (exp(1)^CLD$lower.CL);CLD$backup <- (exp(1)^CLD$upper.CL)
#sitequal x group
ggplot(CLD, aes(x=year, y=emmean, label=.group)) +  
  geom_point(shape=15, size=4, position=pd, color="forestgreen") +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd, color="forestgreen") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18))  + 
  labs(x="Year",y="Log of marginal basal area",title="Year") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  geom_text(nudge_y = c(1.7, 1.8, 1.7), color = "black") 

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