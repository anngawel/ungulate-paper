################################################
###comparing seedling plot treatment sites######
###response to 2nd round review RSOS############
###by AMG last mod 2017AUG23####################
################################################

########load data###
trees<-read.csv("Analysis/Data/seedling plot adult trees.csv")
avg <-read.csv("Analysis/Data/seedling plot averages.csv")


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

##number of adult trees between treatments are not significantly different
#try lsmeans for dbh and height
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

dbh.lm1 <- lm(DBH ~ Trt + Site + Trt:Site, data=trees)
dbh.grid1 <- ref.grid(dbh.lm1)
#  gives cell means for each combination of factor levels

--------------
  # computing lsmeans
  
lsmeans(dbh.grid1, "Site")
lsmeans(dbh.grid1, "Trt")
# Specify the grid and the desired means
#   get estimates, with se and ci's
# The note about interactions is to remind you that simple effects may not equal the 
#   main effect

# tests of differences:
# use pairs() after saving the lsmeans result
dbh.trt <- lsmeans(dbh.grid1, "Trt")
pairs(dbh.trt)

ht.lm1 <- lm(Height ~ Trt + Site + Trt:Site, data=trees)
ht.grid1 <- ref.grid(ht.lm1)
#  gives cell means for each combination of factor levels

--------------
  # computing lsmeans
  
lsmeans(ht.grid1, "Site")
lsmeans(ht.grid1, "Trt")
# Specify the grid and the desired means
#   get estimates, with se and ci's
# The note about interactions is to remind you that simple effects may not equal the 
#   main effect

# tests of differences:
# use pairs() after saving the lsmeans result
ht.trt <- lsmeans(ht.grid1, "Trt")
pairs(ht.trt)

##can we use boxplots to compare parameters between sites?

###DBH###
ggplot(trees, aes(x=Trt, y=DBH))+
  geom_boxplot()+
  facet_grid(.~Site)+
  xlab("Treatment")+
  theme_bw()

ggplot(trees, aes(x=Trt, y=Height))+
  geom_boxplot()+
  facet_grid(.~Site)+
  xlab("Treatment")+
  theme_bw()


  

