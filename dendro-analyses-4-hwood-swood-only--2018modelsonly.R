#global model (using orig season variable)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:season2 + group:near 
                + age:sitequal   + age:timbersale   + age:season2   + age:near   + age:aspect
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:2055.46\

#testing for multicollinearity
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + (1|site/treeid), data=dendrol) 
vif.lme(modlmer) 

#using new seasonality variable
modlmer2 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + year + age + aspect   
                + group:sitequal + group:timbersale + group:season2
                + (1|site/treeid), data=dendrol18) 
AICc(modlmer2) #will not run properly with nested random term


modlmertreeid <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect  
                + group:sitequal + group:timbersale + group:season2
                + (1|treeid), data=dendrol) 
AICc(modlmertreeid)

#global model using treeid only--FINAL MODEL FOR 2018 ONLY
####3-4-19--trying with and without age:sitequal
modlmertreeid <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age  
                + group:sitequal + group:timbersale +  group:season2 
                + age:sitequal
                + (1|treeid), data=dendrol18) 
AICc(modlmertreeid) #original:688.84
##### orig model had site/treeid as random, but was 'singular fit'. removed random.
#age-aspect(814.49),age-near(796.26),age-season2(785.77), age-timbersale(776.17)
#group-near(771.75),
#baselinestandBAsqcm(747.37), baselineinddbhcm (744.74), near(739.75), aspect (735.24)
qqnorm(resid(modlmertreeid));qqline(resid(modlmertreeid)) 
summary(modlmertreeid);Anova(modlmertreeid,test.statistic="F")

season2 <- emmeans(modlmer2, list(pairwise ~ season2))
timbersale <- emmeans(modlmer2, list(pairwise ~ timbersale))

modlmertreeid <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:season2 + group:near 
                + age:sitequal   + age:timbersale   + age:season2   + age:near   + age:aspect
                + (1|treeid), data=dendrol18) 


m8 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age   + aspect 
                + group:sitequal + group:timbersale + group:season2 
                + age:sitequal  
                + (1|treeid), data=dendrol18) 
vif.lme(modlmer) 
AICc(m8)
modellist<-list(mnull,m1,m2,m3,m4,m5,m6,m7,m8,m9)
modnames<-c("mnull","m1","m2","m3","m4","m5","m6","m7","m8","m9")
aictab(modellist,modnames,second.ord=F)

#global model--site ID as random only (disregard)
modlmersite <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:season2
                + age:sitequal  
                + (1|site), data=dendrol18) 
AICc(modlmersite) 
qqnorm(resid(modlmersite));qqline(resid(modlmersite)) 
summary(modlmersite);Anova(modlmersite,test.statistic="F")

#using no random (disregard)
modlmer18 <- lm(logmarba ~ group + sitequal + timbersale + dominance + season2 + age    
                + group:sitequal + group:timbersale + group:season2
                + age:sitequal, data=dendrol18) 
AICc(modlmer18)
qqnorm(resid(modlmer18));qqline(resid(modlmer18)) 
summary(modlmer18);Anova(modlmer18,test.statistic="F")