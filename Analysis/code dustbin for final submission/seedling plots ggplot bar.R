
########################################
##########GRAPHS
#################################
# Graphs of Ungulate Exclosure Experiment without Rota
# Author: Ann Gawel (AMG)
# last updated Sep 24, 2017 by AMG
########################

########################################
###load data###
#import dataset
ungulate <- read.csv("seedlingplots.csv")
ungulate<-subset(ungulate, island == "guam")
ungulate$island<-factor(ungulate$island)
ungulate$site<-factor(ungulate$site)
summary(ungulate)

########################################
ggplot(ungulate, aes(x=trt, y=propalive))+
  geom_boxplot()+
  facet_grid(.~species)


#############
# calculate binomial confidence intervals on raw data
library(binom)
library(plyr)
library(ggplot2)
library(ggsignif)
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

p <- ggplot(newdf,aes(species,prop, ymin=low, ymax=upper, shape=trt))+
  geom_point(stat="identity", position=position_dodge(width=0.3), size=3)+
  geom_errorbar(width=0.2, position=position_dodge(width=0.3))+
  ylab("Proportion seedling survival")+
  ylim(0,1.1)+
  scale_x_discrete("Species", labels=c("Carica", "Psychotria","Morinda","Premna","Aglaia","Ochrosia"))+
  scale_shape_manual(values=c(17,19), breaks=c("fenced","ungulate"), labels=c("No ungulates", "Ungulates"))+
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

###annotate to add asterisks and NS above bars###
p +annotate("text",x=1, y=0.9,label="*") +annotate("text",x=2,y=0.93,label="*") +annotate("text",x=3,y=0.94,label="*") +annotate("text",x=4,y=0.5,label="*")+annotate("text",x=5,y=0.93,label="NS")+annotate("text",x=6,y=0.97,label="NS")




