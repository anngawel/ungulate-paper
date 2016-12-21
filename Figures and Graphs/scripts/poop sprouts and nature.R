####################################################
#####Figures for poop gerimination comparing species
#####abundance in poop vs. species abundance in nature
#####Last edited by AMG 2016NOV13####################
#####################################################

###load packages
library(ggplot2)
library(labeling)
library(tidyr)
library(reshape2)
library(dplyr)

####make a bar graph for each of these things###
###same x axis: plant species, different y axes:
###1. in pigs scats 2. in deer scats 3. in nature
#################################################
####combined vegportions.csv (nature), pigpooportions.csv (pig scat), and 
####deerpooportions.csv (deer scat)#############


###see if ggplot geom_bar is working###
vegportions<-read.csv("vegportions.csv")

ggplot(vegportions, aes(plant_spp, prop)) +
  geom_bar(stat = "identity") +
  theme_bw()+
  coord_flip()

####load merged data###
bardata<-read.csv("pooportions_nature_ungulate.csv")
str(bardata)

####first need to decide how many species should stay on graph
####there are too many (39) from nature, take top 5, and any other native species that
####appear in poop, which are Morinda and Ficus, can keep all exotic spp###

####second need to subset 
subbardata <- subset(bardata, prop_nature > 0.02 | plant_spp =="Morinda citrifolia" | 
                  plant_spp == "Ficus prolixa" | native_exotic =="exotic", 
                          select=c(plant_spp:prop_deer))

str(subbardata)
###removed rows still appearing in subset!###
levels(subbardata$plant_spp)
subbardata$plant_spp<-factor(subbardata$plant_spp)

###need to make subset data long###
datalong <- gather(subbardata, treatment, proportion, prop_nature, prop_pig, prop_deer, 
                   factor_key=TRUE)

###subset into native and exotic###
native<-subset(datalong, native_exotic == "native")
str(native)
levels(native$native_exotic)
native$native_exotic<-factor(native$native_exotic)
##reorder
native$plant_spp <- factor(native$plant_spp, 
                 levels = native$plant_spp[order(native$proportion)])

exotic<-subset(datalong, native_exotic == "exotic")
str(exotic)
levels(exotic$native_exotic)
exotic$native_exotic<-factor(exotic$native_exotic)
##reorder
exotic$plant_spp <- factor(exotic$plant_spp, 
                           levels = exotic$plant_spp[order(exotic$proportion)])


###how do I get native species to stop appearing in the exotic half and 
###exotic species to stop appearing the native half?
###resolved above by subsetting into native and exotic

##can I change facet labels?###
facet_names <- c(
  `prop_nature` = "Nature",
  `prop_pig` = "Pig scat",
  `prop_deer` = "Deer scat")

###now to graph###
plot1<-ggplot(native,aes(plant_spp,proportion))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  facet_grid(. ~ treatment,labeller = as_labeller(facet_names)) +
  xlab("Species")+
  ylab("Proportional abundance")+
  theme_minimal()+
  theme(axis.title.x=element_text(size=10, face="bold"))+
  theme(axis.title.y=element_text(size=10, face="bold"))+
  ggtitle("Native plants")
 

plot2<-ggplot(exotic,aes(plant_spp,proportion))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  facet_grid(. ~ treatment,labeller = as_labeller(facet_names)) +
  xlab("Species")+
  ylab("Proportional abundance")+
  theme_minimal()+
  theme(axis.title.x=element_text(size=10, face="bold"))+
  theme(axis.title.y=element_text(size=10, face="bold"))+
  ggtitle("Exotic plants")


multiplot <- function(..., plotlist = NULL, file, cols=1, rows=1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(plot1,plot2,rows=2, cols=1)


