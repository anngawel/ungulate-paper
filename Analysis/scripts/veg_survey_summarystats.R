#Summary veg data. 
#Starting with tidy vegetation survey dataset, calculate summary statistics and graphs. 

#Goal: get total # of seedlings from each species from seedling surveys to see what proportion of seedlings neiso & aglaia comprise. 

###load packages###
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(dplyr)

setwd("~/ungulate-paper/Analysis/data/working data")
tidy_veg<-read.csv("tidy_veg_gu.csv")

summary(tidy_veg)
#subset out trees
trees_gu<-tidy_veg[tidy_veg$morphology=="tree",]
str(trees_gu)
trees_gu$age<-factor(trees_gu$age)
#subset out seedlings
treeseeds<-trees_gu[trees_gu$age=="seedling",]
str(treeseeds)
summary(treeseeds)

#Summarize to figure out how many seeds of each spp and what proportion of seedlings each species comprises
#seedsumsite<-ddply(treeseeds, .(site, spp), summarize, totalseed=sum(numseed))

treeseedtotals<-ddply(treeseeds, .(spp), summarize, totalseed=sum(numseed))
sum(treeseedtotals$totalseed)
#total tree seedlings 3203
treeseedtotals$prop<-(treeseedtotals$totalseed/3203)
str(treeseedtotals)

#do the same for totals including adult and seedling trees
?

# sort by prop
sort_treesdls <- treeseedtotals[order(-treeseedtotals$prop),]

###trees only - totalseeds = 3203 seedlings total###

ggplot(treeseedtotals, aes(spp, prop))+
 geom_point()

