library(multcompView); library(sjPlot); library(sjmisc)
#crown class
marginal = emmeans(modlmertreeid, ~ dominance)
CLD = cld(marginal, alpha=0.05, Letters=letters, level=0.85)
### Order the levels for printing
CLD$dominance = factor(CLD$dominance, levels=c("C", "D", "I", "O")); CLD
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)
pd=position_dodge(0.4)
#for back-transformation--not using
# CLD$backmean <- (exp(1)^CLD$emmean);CLD$backdown <- (exp(1)^CLD$lower.CL);CLD$backup <- (exp(1)^CLD$upper.CL)
#timbersale x group
ggplot(CLD, aes(x=dominance, y=emmean, label=.group)) +  
  geom_point(shape=15, size=4, position=pd) +  
  geom_errorbar(width=.2, size=1.5,aes(ymin=lower.CL, ymax=upper.CL),position=pd) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18), text = element_text(size=16)) +
  scale_x_discrete(breaks=c("D", "C", "I", "O"),labels=c("Codominant", "Dominant", "Intermediate", "Overtopped")) +
  labs(x="Crown Class",y="Log of marginal basal area",title="Crown Class") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) +
  scale_color_manual(values=c("#999999", "#56B4E9"),name="Group",breaks = c("hwood", "swood"), labels=c("Broadleaf", "Conifer")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, 0.1), nudge_y = c(2.5, 2, 1.9, 2.55), color = "black") 

#timbersale x group
marginal = emmeans(modlmertreeid, ~ timbersale:group)
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
marginal = emmeans(modlmertreeid, ~ season2:group)
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
marginal = emmeans(modlmertreeid, ~ sitequal:group)
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
  geom_text(color="black") +
scale_x_discrete(limits=c("3","2","1"), labels=c("3"="Low","2"="Moderate","1"="High")) +
  geom_text(nudge_x = c(0.1, -0.1, -0.1, -0.1, 0.1, 0.1), nudge_y = c(3.8, .8, 2.75, 1.8, 0.8, 3.1), color = "black") 


#age and site quality
ggplot(dendrol18, aes(x=age, y=logmarba)) + 
  geom_point(shape=19, size=2.3,position=position_jitter(w=0.1, h=0.1)) +
  geom_abline(aes(intercept=1.456143 ,slope=-0.02622,color="Low Quality"),size=1,linetype=1) +
  geom_abline(aes(intercept=1.456143 ,slope=0.242590,color="Medium Quality"),size=1,linetype=2) +
  geom_abline(aes(intercept=1.456143 ,slope=2.140423,color="High Quality"),size=1,linetype=2) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line=element_line(colour="black", size=0.1, linetype = "solid"),
        plot.title = element_text(size=18), text = element_text(size=16)) +
  scale_y_continuous(expand = c(0, 0), limits=c(-0.5,6.1)) + 
  labs(x = "Tree age", y="Marginal growth of log basal area", title="Age x Site Quality") +
  ylab(bquote(~Log[e]~(marginal~basal~area))) 
  #scale_color_manual(values=c("green4", "navajowhite3", "blue"),name="Site Quality",breaks = c("Low Quality", "Medium Quality", "High Quality"), labels=c("Low Quality", "Medium Quality", "High Quality")) 

Plot.HandPick<-ggplot(data=dendrol18, aes(x=age, y=logmarba, group=sitequal))+
  geom_line(size=2, aes(color=sitequal))+
  ylim(0,4)+
  ylab("logmarba")+
  xlab("tree age")+
  ggtitle("Hand Picked Plot")
