#eliminating group
modlmer <- lmer(logmarba ~ sitequal + timbersale + dominance + season + year + age + aspect + near + temp + rain + baselineinddbh + baselinestandBA
               + age:sitequal   + age:timbersale   + age:year   + age:season   + age:temp   + age:rain   + age:near   + age:aspect
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1721


modlmer <- lmer(logmarba ~ sitequal + timbersale + dominance + season + year + age + aspect   + rain  
                + age:season     
                + (1|site/treeid), data=dendrol) 
AICc(modlmer) #original:1721
#age-aspect(1717), age-near(1706),age-rain(1694),age-temp(1680), age-year(1662),age-timbersale(1655),
#age-sitequal(1642),baselinestandBA(1632),baselineinddbh(1629),temp(1624),near(1618)
qqnorm(resid(modlmer));qqline(resid(modlmer),main="q-q plot mixed") 
summary(modlmer);Anova(modlmer)

#lsmeans fpr all categorical interactions
year <- lsmeans(modlmer, list(pairwise ~ year))
timbersale <- lsmeans(modlmer, list(pairwise ~ timbersale))
#season   <- lsmeans(modlmer, list(pairwise ~ season))

