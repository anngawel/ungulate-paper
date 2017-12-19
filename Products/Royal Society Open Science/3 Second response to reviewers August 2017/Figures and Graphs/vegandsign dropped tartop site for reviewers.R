################################
####ggplot multiplot for########
####ungulate abundance##########
####vs plant communities########
####WITH 'TARTOP' SITE DROPPED##
###last  modified by AMG########
####Jun 02, 2017################

###load library###
library(dplyr)
library(ggplot2)

###upload dataset###
vegandsign<-read.csv("~/ungulate-paper/Analysis/data/working data/vegandsign.csv")
###upload reformatted csv where native/exotic is long-form column instead of
###two columns#####
vegandscat_reform<-read.csv("~/ungulate-paper/Analysis/data/working data/vegandscat_reform.csv")

#try analyzing dataset with site "Tartop" removed
vegandsign_drop <- vegandsign[-c(14), ]
vegandscat_reform_drop <- vegandscat_reform[-c(14,28),]

summary(vegandsign_drop)
str(vegandsign_drop)
summary(vegandscat_reform_drop)
str(vegandscat_reform_drop)


###r-quared values for input###
summary(lm(totalsdl~pig, vegandsign_drop))$r.squared
## 0.004396184
summary(lm(nativsdl~pig, vegandsign_drop))$r.squared
## 0.006434192
summary(lm(nonnativesdl~pig, vegandsign_drop))$r.squared
##0.01817523
summary(lm(vines~pig, vegandsign_drop))$r.squared
##0.0001815729

###PIG PLOTS, LEFT-HAND PANELS###

###PIG and TOTAL SEEDLINGS###
p1<- ggplot(data=vegandsign_drop, aes(y=totalsdl, x=pig))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~x)+
  geom_point()+
  xlab("Pig scats per 100m2")+
  ylab("Total seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 0.75, y = 800, label = "r2 = 0.00")


####PIG AND TWO TRENDLINES NATIVE EXOTIC SEEDLINGS###
p2<-ggplot(vegandscat_reform_drop, aes(y=sdls, x=pig, color= native_exotic))+
  geom_smooth(se=FALSE, method="lm",formula=y~x)+
  geom_point()+
  xlab("Pig scats per 100m2")+
  ylab("Seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+ 
  scale_colour_manual(name = "Nat/Exo",
                      labels = c("Exotic", "Native"),
                      values = c("gray", "black")) +   
  scale_shape_manual(name = "Nat/Exo",
                     labels = c("Exotic", "Native"),
                     values = c(19, 17))+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 0.75, y = 700, label = "native r2 = 0.01")+
  annotate("text", x = 0.7, y = 500, label = "non-native r2 = 0.02")

###PIGS AND VINES####
p3<-ggplot(data=vegandsign_drop, aes(y=vines, x=pig))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~x)+
  geom_point()+
  xlab("Pig scats per 100m2")+
  ylab("Vine abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 0.75, y = 200, label = "r2 = 0.00")



####DEER PLOTS - RIGHT-HAND PANELS#################
###r-squared values for input###
summary(lm(totalsdl~log(deer), vegandsign_drop))$r.squared
###0.6877756
summary(lm(nativsdl~log(deer), vegandsign_drop))$r.squared
###0.6185654
summary(lm(nonnativesdl~log(deer), vegandsign_drop))$r.squared
###0.7806632
summary(lm(vines~log(deer), vegandsign_drop))$r.squared
###0.777756

###deer and total seedlings####
p4<- ggplot(data=vegandsign_drop, aes(y=totalsdl, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab("Deer scats per 100m2")+
  ylab("Total seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 4.5, y = 800, label = "r2 = 0.69")

####X vs 2 Y's same plot####
###I USED THIS INSTEAD OF THE INDIVIDUAL NATIVE AND EXOTIC PLOTS####
p5<-ggplot(vegandscat_reform_drop, aes(y=sdls, x=deer, color= native_exotic))+
  geom_smooth(se=FALSE, method="lm",formula=y~log(x))+
  geom_point()+
  xlab("Deer scats per 100m2")+
  ylab("Seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+ 
  scale_colour_manual(name = "Nat/Exo",
                      labels = c("Exotic", "Native"),
                      values = c("gray", "black")) +   
  scale_shape_manual(name = "Nat/Exo",
                     labels = c("Exotic", "Native"),
                     values = c(19, 17))+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 4.5, y = 750, label = "native r2 = 0.62")+
  annotate("text", x = 4.5, y = 600, label = "non-native r2 = 0.78")


###DEER and VINES###
p6<- ggplot(data=vegandsign_drop, aes(y=vines, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab("Deer scats per 100m2")+
  ylab("Vine abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 4.5, y = 220, label = "r2 = 0.78")


###now that you have individual plots, combine into multiplot using function below###

####MULTI-PANEL####
###create multiplot function###
multiplot <- function(..., plotlist = NULL, file, cols = 2, layout = NULL) {
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

###CAN'T REMEMBER WHY I PUT THIS IN, MIGHT NOT BE NEEDED###
par(mfrow=c(2,2))

###MULTIPLOT###
multiplot(p1,p2,p3,p4,p5,p6,cols=2)


