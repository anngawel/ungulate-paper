################################################
###comparing seedling plot treatment sites######
###response to 2nd round review RSOS############
###by AMG last mod 2017AUG23####################
################################################

########load data###
trees<-read.csv("Analysis/data/working data/seedling plot adult trees.csv")
avg <-read.csv("Analysis/data/working data/seedling plot averages.csv")


###load packages###
library(ggplot2)
library(dplyr)
library(plyr)
library(lme4)
library(lsmeans)
library(multcompView)



summary(trees)
str(trees)
trees$DBH<-as.numeric(trees$DBH)
summary(avg)
str(avg)



avg <- avg[order(avg$site, avg$trt), ]
###lsmeans for #adult trees and canopy cover instead of 

#############################
#does number of adult trees differ by site?
# Paired t-test
t.test(adult.trees ~ trt, avg, paired = TRUE)

###results
#data:  adult.trees by trt
#t = 1.3439, df = 7, p-value = 0.2209
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.234195  4.484195
#sample estimates:
#  mean of the differences
#1.625

##number of adult trees between treatments are not significantly different

#linear model
ad.tree.m1<-lmer(adult.trees ~ trt + (1|site), data=avg)
summary(ad.tree.m1)
anova(ad.tree.m1) #get all the same values, but no p-value
adtree.grid1 <- ref.grid(ad.tree.m1)
adtree.trt <- lsmeans(adtree.grid1, "trt")
pairs(adtree.trt) #p-value is 0.2209, same as t-test

##################################
# Paired t-test

t.test(canopy.cover ~ trt, avg, paired = TRUE)

#data:  canopy.cover by trt
#t = 0.10533, df = 7, p-value = 0.9191
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.02681324  0.02931324
#sample estimates:
#  mean of the differences
#0.00125

#linear model
cc.m1<-lmer(canopy.cover ~ trt + (1|site), data=avg)
summary(cc.m1)
anova(cc.m1)
cc.grid1 <- ref.grid(cc.m1)
cc.trt <- lsmeans(cc.grid1, "trt")
pairs(cc.trt) #p-value is 0.9191, same as t-test

##canopy cover between treatments are not significantly different

########################################
#explore dbh and height data

cellN <- with(trees, table(Site, Trt))
cellN

cellMeanDBH <- with(trees, tapply(DBH, list(Site, Trt), mean))
cellMeanDBH

###DBH###
#                     Fenced  Ungulate
#Anao North         10.000000 19.000000
#Anao South         12.000000  6.454545
#North Blas          8.230769  4.750000
#Racetrack           4.928571  4.400000
#Racetrack Fragment  4.000000  4.000000
#Ritidian Gate      14.571429 11.200000
#Ritidian Grid      14.666667 16.200000
#South Blas          6.125000  6.666667

cellMeanHeight <- with(trees, tapply(Height, list(Site, Trt), mean))
cellMeanHeight

###Height###
#                    Fenced  Ungulate
#Anao North         9.666667 11.062500
#Anao South         9.428571  8.636364
#North Blas         7.615385  7.750000
#Racetrack          6.214286  6.650000
#Racetrack Fragment 7.458333  6.666667
#Ritidian Gate      8.714286  7.600000
#Ritidian Grid      9.166667  8.300000
#South Blas         6.875000  7.666667

#########
#test whether dbh varies by trt
dbh.lm1 <- lmer(DBH ~ Trt +(1|Site), data=trees)
dbh.grid1 <- ref.grid(dbh.lm1)
#  gives cell means for each combination of factor levels

# computing lsmeans
# tests of differences:
# use pairs() after saving the lsmeans result
dbh.trt <- lsmeans(dbh.grid1, "Trt")
pairs(dbh.trt) #p-value is 0.5737, dbh is not different

###########################

ht.lm1 <- lmer(Height ~ Trt + (1|Site), data=trees)
ht.grid1 <- ref.grid(ht.lm1) #  gives cell means for each combination of factor levels
# tests of differences:
# use pairs() after saving the lsmeans result
ht.trt <- lsmeans(ht.grid1, "Trt")
pairs(ht.trt) #p-value is 0.98 (height is not different)
