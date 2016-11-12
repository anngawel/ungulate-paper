#######################
# Analysis & Graphs of Ungulate Exclosure Experiment without Rota
# Author: Ann Gawel
# last updated Sep 22, 2016 by AMG 
########################

library(lme4)
library(ggplot2)
library(AICcmodavg)

#import dataset 
ungulate <- read.csv("~/Ungulate paper/stats and figures/ungulate2.csv")

#summarize data
summary(ungulate)

#subset for Guam
ungulate_gu <- ungulate[ which(ungulate$island=='guam'),]


ungulate<-ungulate_gu
##keep working with "ungulate" to use old commands, but now only includes
##guam##

#create variable for survival
survival<-cbind(ungulate$alive,ungulate$dead)

#####FULL#####

full2way<-glmer(survival~species*trt+(1|site),family=binomial,data=ungulate)
species<-glmer(survival~species+(1|site), family=binomial, data=ungulate)
trt<-glmer(survival~trt+(1|site), family=binomial, data=ungulate)

aictab(list(full2way, species, trt), modnames=c("full2way", "species", "trt")) 
#full2way is best fitting model
confint(full2way,method="Wald")

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

########################################
##########GRAPHS##########
########################################
ggplot(ungulate, aes(x=trt, y=propalive))+
  geom_boxplot()+
  facet_grid(.~species)


#############
# calculate binomial confidence intervals on raw data
library(binom)
library(plyr)
binom.confint(, n, conf.level = 0.95, methods = "all") #not working
prop.test(sum(ungulate$alive),sum(ungulate$numplant))$conf.int[1]

#create dataframe with binomial confidence intervals
newdf<-ddply(ungulate,.(trt, species),summarise,
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

##order x-axis by biggest difference between treatments to least##
##Order from most to least significant: CAPA, PSMA, MOCI, PROB, AGMA, OCOP##
newdf$species <- factor(newdf$species, levels = c("carica","psychotria","morinda","premna","aglaia","neiso"))
###add asterisks above first four that have significant difference between treatments by##
###adding data frame with coordinates###
label.df <- data.frame(species = c("carica", "psychotria","morinda","premna"),
                       prop = c(0.9, 0.9,0.9,0.9))

###PROBLEM: ggplot works if I don't attempt to add the data.frame to label the significant
###bars, but as soon as I do, it doesn't recognize values in newdf####

#Make graph

g <- gridExtra::borderGrob(type=9, colour="black", lwd=2) ##notworking - do we need this?

ggplot(newdf,aes(species,prop, ymin=low, ymax=upper, fill=trt))+
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.7))+
  geom_errorbar(width=0.2, position=position_dodge(width=0.7))+
  ylab("Proportion seedling survival")+
  ylim(0,1.1)+
  scale_x_discrete("Species", labels=c("Carica", "Psychotria","Morinda","Premna","Aglaia","Ochrosia"))+
  scale_fill_manual(values=c("lightgrey", "darkgrey"), breaks=c("fenced","ungulate"), labels=c("No ungulates", "Ungulates"))+
  theme_minimal()+
  theme(axis.title.y=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        axis.line.x=element_line(colour="black"), 
        axis.line.y=element_line(colour="black"), 
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        strip.text=element_text(size=10, face="italic"),
        strip.background = element_rect(colour = "white", fill="white"))
    
