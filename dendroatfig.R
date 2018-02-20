#trying with year as random--did not work, discard. (couldn't work because one group only measured in 2017)
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group 
                + group:sitequal + group:timbersale + group:season + group:aspect + group:temp + group:rain + group:near 
                + age:sitequal   + age:timbersale   + age:season   + age:aspect   + age:temp   + age:rain   + age:near
                + (1|site/treeid) + (1|year), data=dendrol) 
AICc(modlmer) #original:1713. making year random and removing other terms with year: 1657. 

#let the selection process begin:
modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
                + age:group 
                + group:sitequal + group:timbersale + group:season + group:aspect + group:near + group:temp + group:rain 
                + age:sitequal   + age:timbersale   + age:season   + age:aspect   + age:near   + age:temp   + age:rain  
                + (1|site/treeid) + (1|year), data=dendrol) 
AICc(modlmer) 
#-age:rain (1687), -age:near (1671), -age:temp (1657), -age:aspect (1654), -age:timbersale (1647), -age:sitequal (1636)
#-group:rain (1632), -group:temp (1626), 
-age:group (1606), -group:near (1602)
#-baselinestandBA (1593), -baselineinddbh (1590), -temp (1586), -near (1577)


modlmer <- lmer(logmarba ~ group + sitequal + timbersale + dominance + season + year + aspect + rain + aspect
                +  group:timbersale + group:year + group:season         
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) 
#age:aspect (1709), age:near (1694), age:rain(1682),age:temp(1667),age:season(1665),age:year(1647),age:timbersale(1640),age:sitequal(1630),
#age:group(1624), group:near(1620),group:rain(1613),group:temp(1601),
#baselinestandBA(1599),baselineinddbh(1594),temp(1590),near (1582),age(1574)
#group:aspect removed bc inestimable
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#lsmeans fpr all categorical predictors
sitequal    <- lsmeans(modlmer, list(pairwise ~ sitequal))
grouptimber <- lsmeans(modlmer, list(pairwise ~ group|timbersale))
groupseason <- lsmeans(modlmer, list(pairwise ~ group|season))
groupyear   <- lsmeans(modlmer, list(pairwise ~ group|year))

