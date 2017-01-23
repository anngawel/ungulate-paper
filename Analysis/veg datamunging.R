###DATA-MUNGING FOR VEG DATA####
###COMPARISONS TO SEEDLING PLOT DATA###
###AMG 1/22/2017###

###load packages###
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)

###load csv###
proportions<-read.csv("~/ungulate-paper/Analysis/data/working data/pooportions_nature_ungulate.csv")
veg<-read.csv("~/ungulate-paper/Analysis/data/working data/veg_guam_working.csv")
veg_spp<-read.csv("~/ungulate-paper/Analysis/data/working data/veg totals by spp.csv")

###summaries###
summary(veg_spp)


###use tidyr to convert data to long form###
veglong <- gather(veg_spp, adultorsdl, count, adult, seedlings, 
                   factor_key=TRUE)

str(veglong)

###sub for guam only###
veglonggu <- subset(veglong, island == "guam")

ggplot(veglonggu,aes(species,count))+
  geom_bar(position="dodge",stat="identity")

ggplot(veglonggu,aes(deer,count))+
  geom_point()

ggplot(veglonggu,aes(site,count, fill=species))+
  geom_bar(stat="identity", position=position_dodge())

ggplot(veglonggu,aes(species,count, fill=adultorsdl))+
  geom_bar(stat="identity", position=position_dodge())

ggplot(veglong,aes(species,count, fill=island))+
  geom_bar(stat="identity", position=position_dodge())





