#########################################################
########POOP SPROUTS data exploration###################
#######last edited by A. Gawel 2016NOV011################
########################################################

#load libraries
library(lsmeans)
library(ggplot2)
library(dplyr)
library(tidyr)

###load data###
poop<-read.csv("raw poop sprouts.csv")
veg<-read.csv("veg_guam_working.csv")

#####Research Questions: 1. Do pigs or deer disperse seeds? 2. Do they disperse more
####native or exotic plant seeds?##################################

####Notes about the study: Scats were collected from several sites in Northern Guam
####in limestone karst forests in 2010-2011
###They were then planted at a nursery and seedling sprouts were monitored and
###identified.
###response variable is ?????

###summaries for poop data###
str(poop)
summary(poop)

with(poop, ftable(ung_spp, vegsite_match))
####looks like a balanced set between deer and pigs at Anao area and deer and pigs
####at Ritidian area, a couple deer and pigs from sblas, and a couple pigs racetrack

summary(poop$plant_spp)
with(poop, ftable(ung_spp, plant_spp))
 plot(poop$native_exotic, poop$last_count)
plot(poop$ung_spp, poop$last_count)

ggplot(poop, aes(last_count, plant_spp, color=ung_spp))+geom_bar()
spp_count_scat<-table(poop$plant_spp)

tapply(poop$last_count, poop$plant_spp, mean)

###WHAT DO I NEED?###
### 2 different responses: last_count per plant_spp per ung_spp; proportion of scat
### that each plant_spp appears in ###

###help from Brittany###
poopy<-with(poop, table(plant_spp, scat_id))
poopy<-data.frame(with(poop, table(plant_spp, scat_id)))
poopy2<-aggregate(poopy$Freq, by = list(poopy$plant_spp), FUN="sum")
poopy2$prop_scat<-poopy2$x/56
colnames(poopy2)[1]<-"plant_spp"
###this created a separate dataset that has summary counts for each plant species that
###germinated, and dividing by our count of scats (56), we get proportion of scats that
###each spp of plant appeared in

###now to merge###
pooportions<-merge(poop, poopy2, by="plant_spp")
###export###
write.csv(pooportions, "pooportions.csv", row.names=F)

###veg data (loaded above)###
str(veg)
###rename some columns###
colnames(veg)[5]<-"plant_spp"
colnames(veg)[2]<-"site"
colnames(veg)[6]<-"height"
colnames(veg)[8]<-"seedlings"
summary(veg$plant_spp)
veg$site<-as.factor(tolower(veg$site))

###clean data###
veg$plant_spp <- gsub("Asplenium niduse", "Asplenium nidus", veg$plant_spp)
veg$plant_spp <- gsub("guma", "GUMA", veg$plant_spp)
veg$plant_spp <- gsub("Pteris tripartita", "Pteris tripartata", veg$plant_spp)
veg$plant_spp <- gsub("ASNI", "Asplenium nidus", veg$plant_spp)
veg$plant_spp <- gsub("ASNI", "Asplenium nidus", veg$plant_spp)
write.csv(veg, "veg_output.csv", row.names=F)

###remove unnecessary rows and non-fruiting trees###
veggy <- veg[-grep("young Pteris?",veg$plant_spp),]
veggy <- veggy[-grep("Asplenium nidus",veggy$plant_spp),]
veggy <- veggy[-grep("Asplenium polyodon",veggy$plant_spp),]
veggy <- veggy[-grep("ASPO",veggy$plant_spp),]
veggy <- veggy[-grep("Nephrolepis",veggy$plant_spp),]
veggy <- veggy[-grep("PIMI",veggy$plant_spp),]
veggy <- veggy[-grep("Pteris tripartata",veggy$plant_spp),]
veggy <- veggy[-grep("Pyrrosia",veggy$plant_spp),]
###count only adults, since only adults have height value, keep only those###
veggy[veggy==""] <- NA
veggy <- veggy[!is.na(veggy$height), ]
##that worked, now try this for counts and proportions###
spcount<-count(veggy, 'plant_spp')
summary(spcount)
spcount$prop<-spcount$freq/1751
write.csv("vegspcount.csv")










