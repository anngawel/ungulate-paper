#######################
# Analysis & Graphs of Ungulate Exclosure Experiment
# Author: Ann Gawel
# last updated March 29 2015 by HSR 
########################

library(lme4)
library(ggplot2)
library(AICcmodavg)

setwd("~/Dropbox/Ann & Haldre- Ungulates/Data & scripts 2015")

ungulate<-read.csv("ungulate2.csv", header=T) #it looks like ungulate2 is the same as ungulate. 

#summarize data
summary(ungulate)

#create variable for survival
survival<-cbind(ungulate$alive,ungulate$dead)

#####FULL#####
full3way<-glmer(survival~island*species*trt+(1|site),family=binomial,data=ungulate)
full_no3way<-update(full3way, form = ~ island*species+species*trt+island*trt + (1|site))
full<-glmer(survival~island*trt+species*trt+(1|site),family=binomial,data=ungulate) #no island*species interaction
noislandtrtx<-glmer(survival~island+species*trt+(1|site),family=binomial,data=ungulate)
noisland<-glmer(survival~species*trt+(1|site),family=binomial,data=ungulate)
species<-glmer(survival~species+(1|site), family=binomial, data=ungulate)
trt<-glmer(survival~trt+(1|site), family=binomial, data=ungulate)

aictab(list(full3way, full_no3way, full, noislandtrtx, noisland, species, trt), modnames=c("full3way", "full_no3way", "full", "noislandtrtx", "noisland", "species", "trt")) 
#full_no3way is best fitting model by a long shot
confint(full_no3way,method="Wald")

#have some complex interactions, such that the effect of island and ungulates is important for some species but not others. Will analyze separately by species now. 

#######AGLAIA#########
aglaia<-subset(ungulate,species=="aglaia")
survivalag<-cbind(aglaia$alive,aglaia$dead)

#candidate models
fullag<-glmer(survivalag~island*trt+(1|site),family=binomial,data=aglaia)
noislandtrtxag<-glmer(survivalag~island+trt+(1|site),family=binomial,data=aglaia)
noislandag<-glmer(survivalag~trt+(1|site),family=binomial,data=aglaia)
notrtag<-glmer(survivalag~island+(1|site),family=binomial,data=aglaia)
nullag<-glmer(survivalag~1+(1|site),family=binomial,data=aglaia)

#find best model
aictab(list(fullag, noislandtrtxag, noislandag, notrtag, nullag), modnames=c("fullag", "noislandtrtxag", "noislandag", "notrtag", "nullag")) 

#not a clear winner. All models are within 4 AIC points of each other. nullag is best, with notrtag a point behind. Summary- trt isn't important for Aglaia. Nor is island. 

confint(notrtag,method="Wald")

#######NEISO########
neiso<-subset(ungulate,species=="neiso")
survivalne<-cbind(neiso$alive,neiso$dead)

#candidate models
fullne<-glmer(survivalne~island+trt+trt:island+(1|site),family=binomial,data=neiso)
noislandtrtxne<-glmer(survivalne~island+trt+(1|site),family=binomial,data=neiso)
noislandne<-glmer(survivalne~trt+(1|site),family=binomial,data=neiso)
notrtne<-glmer(survivalne~island+(1|site),family=binomial,data=neiso)
nullne<-glmer(survivalne~1+(1|site),family=binomial,data=neiso)

#find best model
aictab(list(fullne, noislandtrtxne, noislandne, notrtne, nullne), modnames=c("fullne", "noislandtrtxne", "noislandne", "notrtne", "nullne")) 

#notrtne is best model, but only by 1.96 AIC points. followed by model without an island by trt interaction. But trt isn't significant by a Wald test in this second model. Summary is that trt isn't important for Neiso, but island is. 

confint(notrtne,method="Wald")

#####CARICA########
carica<-subset(ungulate,species=="carica")
survivalca<-cbind(carica$alive,carica$dead)

#candidate models
fullca<-glmer(survivalca~island+trt+trt:island+(1|site),family=binomial,data=carica)
noislandtrtxca<-glmer(survivalca~island+trt+(1|site),family=binomial,data=carica)
noislandca<-glmer(survivalca~trt+(1|site),family=binomial,data=carica)
notrtca<-glmer(survivalca~island+(1|site),family=binomial,data=carica)
nullca<-glmer(survivalca~1+(1|site),family=binomial,data=carica)

#find best model
aictab(list(fullca, noislandtrtxca, noislandca, notrtca, nullca), modnames=c("fullca", "noislandtrtxca", "noislandca", "notrtca", "nullca")) 

#model with only trt is best, with the model including island+trt as second best (only 1.38 AIC points behind). Island less important, trt is important. 

confint(noislandca,method="Wald")

########MORINDA###############

morinda<-subset(ungulate,species=="morinda")
survivalmo<-cbind(morinda$alive,morinda$dead)

#candidate models
fullmo<-glmer(survivalmo~island+trt+trt:island+(1|site),family=binomial,data=morinda)
noislandtrtxmo<-glmer(survivalmo~island+trt+(1|site),family=binomial,data=morinda)
noislandmo<-glmer(survivalmo~trt+(1|site),family=binomial,data=morinda)
notrtmo<-glmer(survivalmo~island+(1|site),family=binomial,data=morinda)
nullmo<-glmer(survivalmo~1+(1|site),family=binomial,data=morinda)

#find best model
aictab(list(fullmo, noislandtrtxmo, noislandmo, notrtmo, nullmo), modnames=c("fullmo", "noislandtrtxmo", "noislandmo", "notrtmo", "nullmo")) 

#model with island + trt is best fit by 2.37 AIC points. 

confint(noislandtrtxmo,method="Wald")

######PREMNA##########
premna<-subset(ungulate,species=="premna")
survivalpr<-cbind(premna$alive,premna$dead)

#candidate models
fullpr<-glmer(survivalpr~island+trt+trt:island+(1|site),family=binomial,data=premna)
noislandtrtxpr<-glmer(survivalpr~island+trt+(1|site),family=binomial,data=premna)
noislandpr<-glmer(survivalpr~trt+(1|site),family=binomial,data=premna)
notrtpr<-glmer(survivalpr~island+(1|site),family=binomial,data=premna)
nullpr<-glmer(survivalpr~1+(1|site),family=binomial,data=premna)

#find best model
aictab(list(fullpr, noislandtrtxpr, noislandpr, notrtpr, nullpr), modnames=c("fullpr", "noislandtrtxpr", "noislandpr", "notrtpr", "nullpr")) 

#model with just trt is best fit by 2.65 AIC points. 

confint(noislandpr,method="Wald")

########PSYCHOTRIA############

psychotria<-subset(ungulate,species=="psychotria")
survivalps<-cbind(psychotria$alive,psychotria$dead)

#candidate models
fullps<-glmer(survivalps~island+trt+trt:island+(1|site),family=binomial,data=psychotria)
noislandtrtxps<-glmer(survivalps~island+trt+(1|site),family=binomial,data=psychotria)
noislandps<-glmer(survivalps~trt+(1|site),family=binomial,data=psychotria)
notrtps<-glmer(survivalps~island+(1|site),family=binomial,data=psychotria)
nullps<-glmer(survivalps~1+(1|site),family=binomial,data=psychotria)

#find best model
aictab(list(fullps, noislandtrtxps, noislandps, notrtps, nullps), modnames=c("fullps", "noislandtrtxps", "noislandps", "notrtps", "nullps")) 

#full model is best fit by 12.56 AIC points.

confint(fullps,method="Wald")

########################################
##########GRAPHS##########
########################################
ggplot(ungulate, aes(x=trt, y=propalive))+
  geom_boxplot()+
  facet_grid(island~species)

ggplot(ungulate, aes(x=trt, y=propalive))+
  geom_boxplot()+
  facet_grid(species~island)

ggplot(ungulate, aes(trt, propalive, color=island))+
  geom_boxplot()+
  facet_grid(.~species)

#############
# calculate binomial confidence intervals on raw data
library(binom)
library(plyr)
binom.confint(, n, conf.level = 0.95, methods = "all") #not working
prop.test(sum(ungulate$alive),sum(ungulate$numplant))$conf.int[1]

#create dataframe with binomial confidence intervals
newdf<-ddply(ungulate,.(island, trt, species),summarise,
              prop=sum(alive)/sum(numplant),
              low=prop.test(sum(alive),sum(numplant))$conf.int[1],
              upper=prop.test(sum(alive),sum(numplant))$conf.int[2])

#newdf<-ddply(ungulate,.(island, trt, species),summarise,
#              prop=binom.confint(alive, numplant, conf.level=0.95, 
#                                 methods="agresti-coull")$mean[1], 
#              low=binom.confint(alive, numplant, conf.level=0.95, 
#                                methods="agresti-coull")$lower[1], 
#              upper=binom.confint(alive, numplant, conf.level=0.95, 
#                                  methods="agresti-coull")$upper[1])

#Plot data

#make list of species names for the final graph
species_names<-list("aglaia"="Aglaia", "carica"="Carica", "morinda"="Morinda", "neiso"="Ochrosia", "premna"="Premna", "psychotria"="Psychotria")

species_labeller <- function(variable,value){
  return(species_names[value])
}

#Make graph
g <- gridExtra::borderGrob(type=9, colour="black", lwd=2)

ggplot(newdf,aes(island, prop, ymin=low, ymax=upper, fill=trt))+
  #geom_point(position=position_dodge(width=0.5))+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.7))+
  geom_errorbar(width=0.2, position=position_dodge(width=0.7))+
  facet_grid(species~., labeller=species_labeller)+
  ylab("Proportion seedling survival")+
  ylim(0,1.1)+
  scale_x_discrete("Island", labels=c("Guam", "Rota"))+
  scale_fill_manual(values=c("lightgrey", "darkgrey"), breaks=c("fenced","ungulate"), labels=c("No ungulates", "Ungulates"))+
  theme_minimal()+
  theme(axis.title.y=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #panel.border = element_rect(colour="black", fill=NA),
        #axis.line.x=element_line(colour="black"), 
        #axis.line.y=element_line(colour="black"), 
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        strip.text=element_text(size=10, face="italic"),
        strip.background = element_rect(colour = "white", fill="white"))+
        annotation_custom(g)

#To do:
#Change spacing, so can fit space for labels saying what is significant in each graph

#change order, so species with significant ungulate effects are on top, without sig ungulate effects are on bottom. 


