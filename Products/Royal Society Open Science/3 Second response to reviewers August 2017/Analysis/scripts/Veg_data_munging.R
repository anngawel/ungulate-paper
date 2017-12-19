#Munging script for Ann's vegetation surveys on Guam and Rota
#Goal: take raw vegetation data, clean it up into a tidy dataset. 
#Do not include any summarization and analysis 

#Things left to do: 
#Make sure morphology is completely filled out. Might do this by creating another dataframe with each species and it's morphology
#Fix dates so in correct format
#clean up species names. 

###load packages###
library(tidyr)
library(dplyr)
library(lubridate)

#veg dataset is the full dataset from the vegetation surveys but doesn't have the Rota data. Should start with the entire dataset instead of this one. 
veg<-read.csv("Analysis/data/working data/veg_guam_working.csv") 

#clean up column names
veg<-dplyr::rename(veg, spp = Spp., ht= Height..m., numseed=Seedling., date=Date, site=Site)

#add a column for age (adult or seedling), so we can sort on that column later. 
veg$age <- ifelse(is.na(veg$numseed), "adult", "seedling")

#make sure date reads in consistently as a date
#veg$date<-mdy(veg$date) #warning -888 failed to parse

setwd("Analysis/data/working data")
write.csv(veg, "tidy_veg_gu.csv")
