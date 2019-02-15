#global model (using orig season variable)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:year + group:season + group:near 
                + age:sitequal   + age:timbersale   + age:year   + age:season   + age:near   + age:aspect
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:2055.46

#using original seasonality variable
modlmer1 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + age + aspect   
                + group:sitequal + group:timbersale + group:season 
                + age:season
                + (1|site/treeid), data=dendrol) 
AICc(modlmer1) #original:2055.46
#age-aspect(2045.81),age-near(2028.64),age-year(1998.60),age-timbersale(1989.24),age-sitequal(1986.68).
#group-near(1977.51), group-year(1971.63),
#baselinestandBAsqcm(1947.43), baselineinddbhcm (1944.14), near(1933.9)
qqnorm(resid(modlmer1));qqline(resid(modlmer1)) 
summary(modlmer1);Anova(modlmer1,test.statistic="F")

#using new seasonality variable
modlmer2 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + year + age + aspect   
                + group:sitequal + group:timbersale + group:season2
                + (1|site/treeid), data=dendrol18) 
AICc(modlmer2) #original:2064.72
#age-aspect(2055.09),age-near(2037.99),age-season2(2028.71), age-year(1999.17),age-timbersale(1989.82),age-sitequal(1986.93).
#group-near(1977.78), group-year(1972.23),
#baselinestandBAsqcm(1948.13), baselineinddbhcm (1944.50), near(1934.38)
qqnorm(resid(modlmer2));qqline(resid(modlmer2)) 
summary(modlmer2);Anova(modlmer2,test.statistic="F")

#global model 2018 only
modlmertreeid <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect 
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

modlmertreeid <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:season2 + group:near 
                + age:sitequal   + age:timbersale   + age:season2   + age:near   + age:aspect
                + (1|treeid), data=dendrol18) 



m8 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age   + aspect 
                + group:sitequal + group:timbersale + group:season2 
                + age:sitequal  
                + (1|treeid), data=dendrol18) 
AICc(m8)
modellist<-list(mnull,m1,m2,m3,m4,m5,m6,m7,m8,m9)
modnames<-c("mnull","m1","m2","m3","m4","m5","m6","m7","m8","m9")
aictab(modellist,modnames,second.ord=F)





#global model (using orig season variable)
modlmersite <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age + aspect + near + baselineinddbhcm + baselinestandBAsqcm
                + group:sitequal + group:timbersale + group:season2
                + age:sitequal  
                + (1|site), data=dendrol18) 
AICc(modlmersite) #original:2055.46
qqnorm(resid(modlmersite));qqline(resid(modlmersite)) 
summary(modlmersite);Anova(modlmersite,test.statistic="F")




modlmer18 <- lm(logmarba ~ group + sitequal + timbersale + dominance + season2 + age    
                + group:sitequal + group:timbersale + group:season2
                + age:sitequal  , data=dendrol18) 
AICc(modlmer18) #original:821.50
####AICs are wrong here
#age-aspect(814.49),age-near(796.26),age-season2(785.77), age-timbersale(776.17)
#group-near(771.75),
#baselinestandBAsqcm(747.37), baselineinddbhcm (744.74), near(739.75), aspect (737.62)
qqnorm(resid(modlmer18));qqline(resid(modlmer18)) 
summary(modlmer18);Anova(modlmer18,test.statistic="F")

modlmer18 <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season2 + age    
                + group:sitequal + group:timbersale + group:season2
                + age:sitequal  
                + (1|site|treeid), data=dendrol18) 
AICc(modlmer18) #original:821.50
####AICs are wrong here
#age-aspect(814.49),age-near(796.26),age-season2(785.77), age-timbersale(776.17)
#group-near(771.75),
#baselinestandBAsqcm(747.37), baselineinddbhcm (744.74), near(739.75), aspect (737.62)
qqnorm(resid(modlmer18));qqline(resid(modlmer18)) 
summary(modlmer18);Anova(modlmer18,test.statistic="F")
sig: groupxtrmt, groupxsitequal, groupxseason, sitequalxage