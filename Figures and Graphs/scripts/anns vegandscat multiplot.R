################################
####ggplot multiplot for########
####ungulate abundance##########
####vs plant communities########
###last  modified by AMG########
####Jan 02, 2017################

###load library###
library(dplyr)
library(ggplot2)

###upload dataset###
vegandsign<-read.csv("~/ungulate-paper/Analysis/data/working data/vegandsign.csv")
###upload reformatted csv where native/exotic is long-form column instead of
###two columns#####
vegandscat_reform<-read.csv("~/ungulate-paper/Analysis/data/working data/vegandscat_reform.csv")

summary(vegandsign)
str(vegandsign)
summary(vegandscat_reform)
str(vegandscat_reform)


###PIG PLOTS, LEFT-HAND PANELS####

###PIG and TOTAL SEEDLINGS###
p1<- ggplot(data=vegandsign, aes(y=totalsdl, x=pig))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~x)+
  geom_point()+
  xlab("Pig scats per 100m2")+
  ylab("Total seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")
  


####PIG AND TWO TRENDLINES NATIVE EXOTIC SEEDLINGS###
p2<-ggplot(vegandscat_reform, aes(y=sdls, x=pig, color= native_exotic))+
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
                     values = c(19, 17))

###PIGS AND VINES####
p3<-ggplot(data=vegandsign, aes(y=vines, x=pig))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~x)+
  geom_point()+
  xlab("Pig scats per 100m2")+
  ylab("Vine abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")
  
  

####DEER PLOTS - RIGHT-HAND PANELS#################

###deer and total seedlings####
p4<- ggplot(data=vegandsign, aes(y=totalsdl, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab("Deer scats per 100m2")+
  ylab("Total seedling abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")

####X vs 2 Y's same plot####
###I USED THIS INSTEAD OF THE INDIVIDUAL NATIVE AND EXOTIC PLOTS####
p5<-ggplot(vegandscat_reform, aes(y=sdls, x=deer, color= native_exotic))+
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
                     values = c(19, 17))
  

###DEER and VINES###
p6<- ggplot(data=vegandsign, aes(y=vines, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab("Deer scats per 100m2")+
  ylab("Vine abundance")+
  theme_bw() + 
  theme(axis.line = element_line())+ 
  theme (panel.background = element_rect(fill = "transparent", colour = NA), 
         panel.grid.minor = element_line(colour = NA), 
         panel.grid.major = element_line(colour = NA), legend.position="none")
  

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










####FOR UNGULATE ABUNDANCE####
###DID NOT USE THIS#####
rm(guamsite)
summary(ungulateabundance)
ungulateabundancebar <- ddply(ungulateabundance, .(animal, island), summarise, N= length(scatcount),scatcount = mean(scatcount),sd = sd(scatcount),se= sd(scatcount) / sqrt(length(scatcount)) )
summary(ungulateabundancebar)
ggplot(data=ungulateabundancebar, aes(x=animal,y=scatcount,fill=island)) + geom_bar(position=position_dodge())+geom_errorbar(aes(ymin=scatcount-se,ymax=scatcount+se),size=.3,width=.2,position=position_dodge(.9))+xlab("Ungulate")+ylab("Scat abundance")+opts(panel.background = theme_rect(fill = "transparent", colour = NA), panel.grid.minor = theme_line(colour = NA), panel.grid.major = theme_line(colour = NA)) + scale_fill_manual(values=c("black","gray"))

###NATIVE SEEDLINGS ONLY###
###DID NOT USE THIS, COMBINED WITH EXOTIC FOR ONE PANEL####
pX<-qplot(deer,nativesdls,data=guamveg, geom=c("point","smooth"), se=FALSE, method="lm", formula=y~log(x), xlab="Deer scats per 100m2", ylab="Seedling abundance") + theme (panel.background = element_rect(fill = "transparent", colour = NA), panel.grid.minor = element_line(colour = NA), panel.grid.major = element_line(colour = NA))
pX<-pX + layer(geom = "point") + theme(axis.line = element_line())
pX<-pX + geom_line(aes(y=nativesdls), colour="black")  + geom_line(aes(y=exoticsdls), colour="gray")

###EXOTIC SEEDLINGS###
###DID NOT USE THIS, COMBINED WITH EXOTIC FOR ONE PANEL####
pX<-qplot(deer,exoticsdls,data=guamveg, geom=c("point","smooth"), se=FALSE, method="lm", formula=y~log(x), xlab="Deer scats per 100m",ylab="Exotic seedling abundance") + theme (panel.background = element_rect(fill = "transparent", colour = NA), panel.grid.minor = element_line(colour = NA), panel.grid.major = element_line(colour = NA))
pX<-pX + layer(geom = "point") + theme(axis.line = element_line())

###SHANNON DIVERSITY###
#####DID NOT USE SHANNON
pX<-qplot(deer, shannon, data=guamveg, geom = c("point","smooth"), se = FALSE, method = "lm", formula = y~poly(x,2), xlab="Deer scats per 100m", ylab = "Shannon diversity (H)") + theme (panel.background = element_rect(fill = "transparent", colour = NA), panel.grid.minor = element_line(colour = NA), panel.grid.major = element_line(colour = NA))
pX<-pX + layer(geom = "point") + theme(axis.line = element_line())
