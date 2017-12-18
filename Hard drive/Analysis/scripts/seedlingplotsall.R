summary(ungulate)

######using ALL######
survival<-cbind(ungulate$alive,ungulate$dead)
full<-lmer(survival~species+trt+deer.abundance+trt:species+trt:deer.abundance+(1|island/site),family=binomial,data=ungulate)
summary(full)
notrt<-lmer(survival~species+deer.abundance+(1|island/site),family=binomial,data=ungulate)
summary(notrt)
anova(full,notrt)

##by species##

############AGLAIA##############
aglaia<-subset(ungulate,species=="aglaia")
survivalag<-cbind(aglaia$alive,aglaia$dead)
fullag<-lmer(survivalag~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=aglaia)
summary(fullag)
notrtag<-lmer(survivalag~deer.abundance+(1|island/site),family=binomial,data=aglaia)
summary(notrtag)
anova(fullag,notrtag)

##########CARICA#################
carica<-subset(ungulate,species=="carica")
survivalca<-cbind(carica$alive,carica$dead)
fullca<-lmer(survivalca~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=carica)
summary(fullca)
notrtca<-lmer(survivalca~deer.abundance+(1|island/site),family=binomial,data=carica)
summary(notrtca)
anova(fullca,notrtca)

################MORINDA###############
morinda<-subset(ungulate,species=="morinda")
survivalmo<-cbind(morinda$alive,morinda$dead)
fullmo<-lmer(survivalmo~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=morinda)
summary(fullmo)
notrtmo<-lmer(survivalmo~deer.abundance+(1|island/site),family=binomial,data=morinda)
summary(notrtmo)
anova(fullmo,notrtmo)

############NEISO##############
neiso<-subset(ungulate,species=="neiso")
survivalne<-cbind(neiso$alive,neiso$dead)
fullne<-lmer(survivalne~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=neiso)
summary(fullne)
notrtne<-lmer(survivalne~deer.abundance+(1|island/site),family=binomial,data=neiso)
summary(notrtne)
anova(fullne,notrtne)

##############PREMNA#################
premna<-subset(ungulate,species=="premna")
survivalpr<-cbind(premna$alive,premna$dead)
fullpr<-lmer(survivalpr~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=premna)
summary(fullpr)
notrtpr<-lmer(survivalpr~deer.abundance+(1|island/site),family=binomial,data=premna)
summary(notrtpr)
anova(fullpr,notrtpr)

##################PSYCHOTRIA#################

psychotria<-subset(ungulate,species=="psychotria")
survivalps<-cbind(psychotria$alive,psychotria$dead)
fullps<-lmer(survivalps~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=psychotria)
summary(fullps)
notrtps<-lmer(survivalps~deer.abundance+(1|island/site),family=binomial,data=psychotria)
summary(notrtps)
anova(fullps,notrtps)
##indep not nested random effects##
fullps<-lmer(survivalps~trt+deer.abundance+trt:deer.abundance+(1|island)+(1|site),family=binomial,data=psychotria)
summary(fullps)
notrtps<-lmer(survivalps~deer.abundance+(1|island)+(1|site),family=binomial,data=psychotria)
summary(notrtps)
anova(fullps,notrtps)

#######GRAPHS#########
bargraph.CI(island,survival,data=ungulate)

######try no cbind#####
psurvivalag<-(aglaia$propalive)
pfullag<-lmer(psurvivalag~trt+deer.abundance+trt:deer.abundance+(1|island/site),family=binomial,data=aglaia)
