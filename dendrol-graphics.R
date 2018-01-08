#function for sample size, to be used in boxplots
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

#grouped boxplots
ggplot(dendrol, aes(x=sitequal, y=logmarba, fill=group)) + 
  geom_boxplot() +  
  stat_boxplot(geom ='errorbar') + 
  stat_summary(fun.data = give.n, geom = "text", position = position_dodge(width = 0.75), size = 4) +
  geom_text(aes(label=posthoc.l, y=live+(live.se/1.8)), vjust=-1.5) + #????
  theme_minimal() + 
  theme(axis.line=element_line(colour="black", size=0.1, linetype = "solid")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,8.5)) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),name="Group",breaks = c("hwood", "pplar", "swood"), labels=c("Hardwood", "Poplar", "Softwood")) +
  labs(x = "Site quality", y="Marginal growth of log basal area", title="Site quality") +
  geom_point(position = position_jitterdodge(jitter.width=0.035, dodge.width=0.75))  #jitter and dodge 

text(1:length(plot$n), plot$stats[5,]+1, paste("n=", plot$n))

#option to just dodge
  geom_point(position = position_dodge(width=0.75)) 

#with jitter overlay
plot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")
stripchart(logmarba ~ sitequal, vertical=TRUE, data=dendrol, method="jitter", jitter=0.175, add=TRUE, pch=20, col="blue", xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")

#lsmeans plots
#year
lsyear<-data.frame(year=c('2015','2016','2017'),cldown=c(-1.769739,-2.329213,-1.054521),mean=c(2.399255,1.598122,2.750009),clup=c(6.568250,5.525458,6.554539))
yearplot <- ggplot(lsyear, aes(x=year, y=mean, group=1)) +  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup)) +
  geom_point(shape=21, size=3, fill="white") +  ylim(-3, 8)
yearplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Year",y="Log of marginal basal area",title="Growth by year") +
  annotate("text", x=.75, y=8, label= "p<0.0001") 

#softwood-timbersale
lsgroupsale<-data.frame(groupsale=c('Not thinned','Thinned'),cldown=c(-4.839794169,0.001728341),mean=c(0.257078 ,5.004519),clup=c(5.353950,10.007310))
groupsaleplot <- ggplot(lsgroupsale, aes(x=groupsale, y=mean, group=1)) +  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup)) +
  geom_point(shape=21, size=3, fill="white") +  ylim(-7, 11)
groupsaleplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Timbersale",y="Log of marginal basal area",title="Softwood growth by timbersale") +
  annotate("text", x=.85, y=11, label= "Chi.Sq.= 14.01, p=0.0009") 

#hardwood v softwood in timbersale areas
lsgroupsale<-data.frame(groupsale=c('Hardwood','Softwood'),cldown=c(-1.188124785,0.001728341),mean=c(2.152185 ,5.004519),clup=c(5.492495,10.007310))
groupsaleplot <- ggplot(lsgroupsale, aes(x=groupsale, y=mean, group=1)) +  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup)) +
  geom_point(shape=21, size=3, fill="white") +  ylim(-2, 11)
groupsaleplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Group",y="Log of marginal basal area",title="Hardwood v softwood growth in thinned areas") +
  annotate("text", x=.85, y=11, label= "Chi.Sq.= 14.01, p=0.0009") 

#hardwood by season
lsgroupsale<-data.frame(groupsale=c('Jun-Jul-Aug','May-Sep-Oct'),cldown=c(-0.8240716,-3.8422250),mean=c(3.2875933 ,0.2627907),clup=c(7.399258,4.367806))
groupsaleplot <- ggplot(lsgroupsale, aes(x=groupsale, y=mean, group=1)) +  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup)) +
  geom_point(shape=21, size=3, fill="white") +  ylim(-5, 9)
groupsaleplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Growing season",y="Log of marginal basal area",title="Hardwood growth by growing season") +
  annotate("text", x=.85, y=9, label= "Chi.Sq.= 6.74, p=0.03") 

#poplar by season
lsgroupsale<-data.frame(groupsale=c('Jun-Jul-Aug','May-Sep-Oct'),cldown=c(-1.5350448,-5.5139278),mean=c(4.2681425 ,0.4146491),clup=c(10.071330,6.343226))
groupsaleplot <- ggplot(lsgroupsale, aes(x=groupsale, y=mean, group=1)) +  geom_errorbar(width=.1, aes(ymin=cldown, ymax=clup)) +
  geom_point(shape=21, size=3, fill="white") +  ylim(-7, 13)
groupsaleplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Growing season",y="Log of marginal basal area",title="Poplar growth by growing season") +
  annotate("text", x=.85, y=12.5, label= "Chi.Sq.= 6.74, p=0.03") 

#plot of random effect coefficients (treeid)
growth_subject <- fixef(modlmer) + ranef(modlmer)$treeid
growth_subject$treeid<-rownames(growth_subject)
names(growth_subject)[1]<-"Intercept"
growth_subject <- growth_subject[,c(2,1)]
ggplot(growth_subject,aes(x=treeid,y=Intercept))+geom_point()


boxplot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="Marginal growth of log basal area",main="Site quality")
plot(logmarba ~ month, data=dendrol,xlab="Month",ylab="Marginal growth of log basal area",main="Month")
plot(logmarba ~ year, data=dendrol,xlab="Year",ylab="Marginal growth of log basal area",main="Year")


plot(logmarba ~ spcode, data=dendrol,xlab="Timbersale",ylab="May growth",main="May growth~timbersale")
plot(logmarba ~ timbersale, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ month, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ year, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ rain, data=dendrol,xlab="Rainfall",ylab="May growth",main="May growth~site quality")
plot(logmarba ~ sitequal, data=dendrol,xlab="Site quality",ylab="May growth",main="May growth~site quality")
