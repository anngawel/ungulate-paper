# Data wrangling for ungulate food web exclosure project ###
# creates two files, one with all data, and one with Guam data.

library(tidyverse)
library(lubridate)

##import dataset
ungulate <- read.csv("data/raw data/seedlingplots.csv", stringsAsFactors=F)
#note - there is also a dataset called ungulate2, which appears to have exactly the same data as seedlingplots.csv. date is in a different format, but is identical.

#assign class of each column
factor_cols <- c("island","site", "species", "trt", "deer.abundance", "pig.abundance")
numeric_cols <- c("numplant", "alive", "dead")
ungulate[factor_cols] <- lapply(ungulate[factor_cols], as.factor)
ungulate[numeric_cols] <- lapply(ungulate[numeric_cols], as.numeric)
#change format of dates
ungulate$plantdt <- mdy(ungulate$plantdt)
ungulate$checkdt <- mdy(ungulate$checkdt)

#create growtime factors for analysis
ungulate$growtime <- ungulate$checkdt - ungulate$plantdt
#standardize time - may help with convergence
ungulate$growtime_st <- scale(ungulate$growtime)

#create response variables
ungulate$survival <- cbind(ungulate$alive,ungulate$dead)
ungulate$propalive <- ungulate$alive/ungulate$numplant

#look at data
str(ungulate)
with(ungulate, table(species, growtime))

#write full dataset
write.csv(ungulate,"data/tidy data/ung_seedlingplot_tidy.csv")

#write dataset just with Guam data
ung_seedlingplot_guam_tidy <- ungulate[ungulate$island=="guam",]

write.csv(ung_seedlingplot_guam_tidy,"data/tidy data/ung_seedlingplot_guam_tidy.csv")

