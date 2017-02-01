#Summary veg data. 
#Starting with tidy vegetation survey dataset, calculate summary statistics and graphs. 

#Goal: get total # of seedlings from each species from seedling surveys to see what proportion of seedlings neiso & aglaia comprise. 

###load packages###
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(dplyr)

setwd("Analysis/data/working data")
tidy_veg<-read.csv("tidy_veg_gu.csv")

#subset out seedlings
seedling<-tidy_veg[tidy_veg$age=="seedling",]
seedling$age<-factor(seedling$age)

#Summarize to figure out how many seeds of each spp and what proportion of seedlings each species comprises
seedsumsite<-ddply(seedling, .(site, spp), summarize, totalseed=sum(numseed))
seedsum<-ddply(seedling, .(spp), summarize, totalseed=sum(numseed), prop=totalseed/5673)

sum(seedsum$totalseed) #5673 seedlings total

seedsum$spp<- factor(seedsum$spp, levels = seedsum[order(seedsum$prop, decreasing = TRUE), 1])

ggplot(seedsum, aes(spp, prop))+
  geom_point()
