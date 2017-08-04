#################################################
#####HURDLE MODELS FOR PIGS SCAT DATA#################
#######LAST MODIFIED BY AMG 19JUN2017################
#################################################

###load package###
library(pscl)

###load dataframe###
poop<-read.csv("pooportions.csv")
veg<-read.csv("vegportions.csv")

str(poop)
str(veg)
veg$prop_veg<-veg$prop

all <- read.csv("pooportions_nature_ungulate.csv")
str(all)
all[is.na(all)] <- 0
all$freq_pig <- as.integer(all$freq_pig)
all$freq_deer<- as.integer(all$freq_deer)
all$freq_nature <- as.integer(all$freq_nature)

hurdle(freq_pig~freq_nature, all, dist = c("negbin"))

