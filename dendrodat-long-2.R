#load packages
library(glm2);library(lme4);library(ggplot2);library(MASS);library(ResourceSelection);library(plyr);library(car);library(emmeans);library(AICcmodavg)

#read-in 
dendrodatl <- read.csv('F:/FIG/Dendrometer/Dendrometer Analyses/dendrodat-long.csv')
#summary(dendrodatl)

#making numeric vars factors as needed
dendrodatl$treeid   <- factor(dendrodatl$treeid)
dendrodatl$spnum    <- factor(dendrodatl$spnum)
dendrodatl$sitequal <- factor(dendrodatl$sitequal)
dendrodatl$year     <- factor(dendrodatl$year)

dendrol<-dendrodatl 

dendrol$olddbh<-ifelse(((dendrol$year=='2015')&(dendrol$month=='may'))|((dendrol$year=='2015')&(dendrol$month=='jun'))|((dendrol$year=='2015')
                       &(dendrol$month=='jul'))|((dendrol$year=='2016')&(dendrol$month=='may'))|((dendrol$year=='2017')&(dendrol$month=='may'))
                       ,dendrol$baselineinddbh)    #setting start columns


dendrol$olddbh<-ifelse(((dendrol$year=='2015')&(dendrol$month=='jun')&(dendrol$treeid=c(13:22)))|((dendrol$year=='2015')&(dendrol$month=='jul')&
                      (dendrol$treeid=c(2:9))| (dendrol$treeid=c(11:12))| (dendrol$treeid=c(23-24))),dendrol$baselineinddbh)    #setting start columns

dendrol$oldBA  <- (3.14159*((dendrol$olddbh/2)^2))    #calculating oldBA from olddbh 
dendrol$newdbh <- (dendrol$growth+dendrol$olddbh)     #calculating newdbh
dendrol$newBA  <- (3.14159*((dendrol$newdbh/2)^2))    #calculating newBA from newdbh
dendrol$mardbh <- (dendrol$newdbh-dendrol$olddbh)     #calculating mardbh
dendrol$marba  <- (dendrol$newBA-dendrol$oldBA)       #calculating marba

#1-treeid, 2-spnum, 3-spname, 4-spcode, 5-site, 6-aspect, 7-sitequal,8-timbersale, 9-dominance, 10-baselinestandBA, 11-baselineinddbh, 12-tension, 13-year
#14-month, 15-growth, 16-rain, 17-temp, 18-olddbh, 19-oldBA, 20-newdbh, 21-newBA, 22-mardbh, 23-marba

for (treeid in 1:32){
  if it's later, olddbh=prevolddbh, else '
  add olddbh to newdbh
}
dendrol[560:576,11:23]

write.csv(dendrol,'F:/FIG/Dendrometer/Dendrometer Analyses/dendrol.csv')

###############################Variables:
#treeid [char]           = individual tree id 
#spnum [char]            = forestry species code 
#spname [char]           = tree common name 
#spcode [char]           = 4-letter USDA species code 
#site [char]             = FTIG site code (B6B, B10B, C03x [x added for consistency of variable length]) 
#aspect [char]           = south (S) or north (N) 
#sitequal [num]          = site quality (1: , 2: , 3: ) 
#timbersale [char]       = Y (thinned ) or N (unthinned) 
#dominance [char]        = dominant (D), co-dominant (C), or intermediate (I) 
#baselinedbh [num]       = DBH at date of first installation (in inches) 
#tension [char]          = lo, hi or na -- densiometer tension. only available for trees 1-24 (24-32: na) 
#year, month [num]       = year and month of measurement
#growth                  = growth in mm from previous measurement
#rain                    = total rainfall since last measurement
#temp                    = mean max temperature since last measurement

#exploratory
# table(dendrodatl$spcode,dendrodatl$site) #all sp occur in only 1 or 2 sites
# table(dendrodatl$spcode,dendrodatl$sitequal) #all but PIST occur in only 1 or 2 site quals
# table(dendrodatl$site,dendrodatl$sitequal) #B06B: sitequal 1 only; B10B and C03x: each site qual represented
# table(dendrodatl$spcode,dendrodatl$timbersale) #at least one of each species in each level of timbersale
# table(dendrodatl$site,dendrodatl$timbersale) #exactly even number of trees in each timbersale level per plot
# table(dendrodatl$site,dendrodatl$aspect) #B06B and C03x: N; B10B: S
# table(dendrodatl$spcode,dendrodatl$dominance) #most trees are co-dominant
# table(dendrodatl$tension,dendrodatl$baselinedbh) #low tension dendrometers were used below 21.1 in DBH

