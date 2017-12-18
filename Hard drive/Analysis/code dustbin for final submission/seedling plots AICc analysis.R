#######################
# Analysis of Ungulate Exclosure Experiment without Rota
# Author: Ann Gawel
# last updated Aug 23, 2017 by AMG 
########################

library(lme4)
library(ggplot2)
library(AICcmodavg)
library(tidyr)
library(lubridate)
library(dplyr)

#import dataset 
ungulate <- read.csv("seedlingplots.csv")

#summarize data
summary(ungulate)

#subset for Guam
ungulate_gu <- subset(ungulate, island == "guam")
ungulate<-ungulate_gu
str(ungulate)

ungulate$island<-factor(ungulate$island)
ungulate$site<-factor(ungulate$site)
ungulate$length.exposure<-factor(ungulate$length.exposure)
ungulate$length.exposure <- as.integer(ungulate$length.exposure)
##keep working with "ungulate" which now only includes
##guam##


#create variable for survival
survival<-cbind(ungulate$alive,ungulate$dead)


#####FULL#####
full <- glmer(survival~species+trt+species:trt+length.exposure+length.exposure:trt+(1|site), family=binomial, data = ungulate)
notimetrtintxn <- glmer(survival~species+trt+species:trt+length.exposure+(1|site), family=binomial, data = ungulate)
nosptrtintxn <- glmer(survival~species+trt+length.exposure+length.exposure:trt+(1|site), family=binomial, data = ungulate)
notime <-glmer(survival~species*trt+(1|site), family=binomial, data = ungulate)
nospecies <- glmer(survival~trt*length.exposure+(1|site), family=binomial, data = ungulate)
notrt <- glmer(survival~species*length.exposure+(1|site), family=binomial, data = ungulate)
species<-glmer(survival~species+(1|site), family=binomial, data=ungulate)
trt<-glmer(survival~trt+(1|site), family=binomial, data=ungulate)
time <- glmer(survival~length.exposure+(1|site), family=binomial, data=ungulate)

aictab(list(full, notimetrtintxn, nosptrtintxn, notime, nospecies, notrt, species, trt, time), modnames=c("full", "notimeintxn", "nospintxn", "notime", "nospecies", "notrt", "species", "trt", "time")) 
#twoway is best fitting model
confint(notimetrtintxn,method="Wald")

#Species seems to matter more than treatment, so will analyze separately by species now. 

#######AGLAIA#########
aglaia<-subset(ungulate,species=="aglaia")
survivalag<-cbind(aglaia$alive,aglaia$dead)

#candidate models
trtag<-glmer(survivalag~trt+(1|site),family=binomial,data=aglaia)
nullag<-glmer(survivalag~1+(1|site),family=binomial,data=aglaia)

#find best model
aictab(list(trtag, nullag), modnames=c("trtag", "nullag")) 

##barely any difference between trt and null, only 0.5 AICc difference##
confint(nullag,method="Wald")

#######NEISO########
##***Neisosperma is now called Ochrosia, so will appear as "Ochrosia oppositifolia" in figures and text
neiso<-subset(ungulate,species=="neiso")
survivalne<-cbind(neiso$alive,neiso$dead)

#candidate models
trtne<-glmer(survivalne~trt+(1|site),family=binomial,data=neiso)
nullne<-glmer(survivalne~1+(1|site),family=binomial,data=neiso)

#find best model
aictab(list(trtne, nullne), modnames=c("trtne", "nullne"))

#nullne is best model, 2.72 less AICc###
confint(nullne,method="Wald")

#####CARICA########
carica<-subset(ungulate,species=="carica")
survivalca<-cbind(carica$alive,carica$dead)

#candidate models
trtca<-glmer(survivalca~trt+(1|site),family=binomial,data=carica)
nullca<-glmer(survivalca~1+(1|site),family=binomial,data=carica)

#find best model
aictab(list(trtca, nullca), modnames=c("trtca", "nullca"))

#trtca is best model by 34.68, fence trt was better fit than null##
confint(trtca,method="Wald")

########MORINDA###############

morinda<-subset(ungulate,species=="morinda")
survivalmo<-cbind(morinda$alive,morinda$dead)

#candidate models
trtmo<-glmer(survivalmo~trt+(1|site),family=binomial,data=morinda)
nullmo<-glmer(survivalmo~1+(1|site),family=binomial,data=morinda)

#find best model
aictab(list(trtmo,nullmo), modnames=c("trtmo", "nullmo"))

#model with treatment trtmo is best fit by 8.27 AICc points## 

confint(trtmo,method="Wald")

######PREMNA##########
premna<-subset(ungulate,species=="premna")
survivalpr<-cbind(premna$alive,premna$dead)

#candidate models
trtpr<-glmer(survivalpr~trt+(1|site),family=binomial,data=premna)
nullpr<-glmer(survivalpr~1+(1|site),family=binomial,data=premna)

#find best model
aictab(list(trtpr,nullpr), modnames=c("trtpr", "nullpr"))

#model with trt is best fit by 7.77 AIC points. 

confint(trtpr,method="Wald")

########PSYCHOTRIA############

psychotria<-subset(ungulate,species=="psychotria")
survivalps<-cbind(psychotria$alive,psychotria$dead)

#candidate models
trtps<-glmer(survivalps~trt+(1|site),family=binomial,data=psychotria)
nullps<-glmer(survivalps~1+(1|site),family=binomial,data=psychotria)

#find best model
aictab(list(trtps,nullps), modnames=c("trtps", "nullps"))

#full model is best fit by 22.12 AICc points.

confint(trtps,method="Wald")
