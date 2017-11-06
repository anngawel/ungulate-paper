################################
####ggplot multiplot for########
####ungulate abundance##########
####vs plant communities########
###last  modified by AMG########
####Jun 03, 2017################

###load library###
library(dplyr)
library(ggplot2)
library(lme4)
library(AICcmodavg)
library(MuMIn)

###upload dataset###
vegandsign<-read.csv("vegandsign.csv")
###upload reformatted csv where native/exotic is long-form column instead of
###two columns#####
vegandscat_reform<-read.csv("vegandscat_reform.csv")

summary(vegandsign)
str(vegandsign)
summary(vegandscat_reform)
str(vegandscat_reform)

vegandsign2<-vegandsign[vegandsign$pig<1,] #without high pig site
vegandsign3<-vegandsign[vegandsign$deer<8,] #without high pig site
vegandscat_reform2<-vegandscat_reform[vegandscat_reform$pig<1,] #without high pig site
vegandscat_reform3<-vegandscat_reform[vegandscat_reform$deer<8,] #without high deer site

###### Just pigs ##############
mpigtot2<-lm(totalsdl~pig, vegandsign2)
mpigltot2<-lm(totalsdl~log(pig), vegandsign2)
summary(mpigtot2)
summary(mpigltot2) #none are significant

mpignat2<-lm(nativsdl~(pig), vegandsign2)
mpiglnat2<-lm(nativsdl~log(pig), vegandsign2)
summary(mpignat2)
summary(mpiglnat2)

mpignnat2<-lm(nonnativesdl~pig, vegandsign2)
mpiglnnat2<-lm(nonnativesdl~log(pig), vegandsign2)
summary(mpignnat)
summary(mpignnat2)

mpigvin2<-lm(vines~pig, vegandsign2)
mpiglvin2<-lm(vines~log(pig), vegandsign2)
summary(mpigvin2)
summary(mpiglvin2) # 0.07... close!

###r-squared values for input###
summary(lm(totalsdl~pig, vegandsign2))$r.squared
##[1] 0.2054512
summary(lm(nativsdl~pig, vegandsign2))$r.squared
##[1] 0.2143603
summary(lm(nonnativesdl~pig, vegandsign2))$r.squared
##[1] 0.09219646
summary(lm(vines~pig, vegandsign2))$r.squared
##[1] 0.2549879


###PIG PLOTS, LEFT-HAND PANELS###

###PIG and TOTAL SEEDLINGS###  bquote('x axis'~(Å^2))
p1<- ggplot(data=vegandsign2, aes(y=totalsdl, x=pig))+
  geom_point()+
  xlab(expression(paste("Pig scats per 100", m^2)))+
  ylab("Total seedling abundance")+
  theme_bw() +
  theme(axis.line = element_line())+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_line(colour = NA),
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 0.5, y = 800, label = "r^2==0.21", parse=T)


####PIG AND TWO TRENDLINES NATIVE EXOTIC SEEDLINGS###
p2<-ggplot(vegandscat_reform2, aes(y=sdls, x=pig, color= native_exotic))+
  geom_point()+
  xlab(expression(paste("Pig scats per 100", m^2)))+
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
  annotate("text", x = 0.4, y = 700, label = "native~r^2==0.21", parse=T)+
  annotate("text", x = 0.4, y = 550, label = "nonnative~r^2==0.09", parse=T)

###PIGS AND VINES####
p3<-ggplot(data=vegandsign2, aes(y=vines, x=pig))+
  geom_point()+
  xlab(expression(paste("Pig scats per 100", m^2)))+
  ylab("Vine abundance")+
  theme_bw() +
  theme(axis.line = element_line())+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_line(colour = NA),
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 0.5, y = 200, label = "r^2==0.25", parse=T)



####DEER PLOTS - RIGHT-HAND PANELS#################

###### Just deer ##############
mdeertot3<-lm(totalsdl~deer, vegandsign3)
mdeerltot3<-lm(totalsdl~log(deer), vegandsign3)
summary(mdeertot3)
summary(mdeerltot3)#better fit

mdeernat3<-lm(nativsdl~deer, vegandsign3)
mdeerlnat3<-lm(nativsdl~log(deer), vegandsign3)
summary(mdeernat3)
summary(mdeerlnat3)

mdeernnat3<-lm(nonnativesdl~deer, vegandsign3)
mdeerlnnat3<-lm(nonnativesdl~log(deer), vegandsign3)
summary(mdeernnat3)
summary(mdeerlnnat3)

mdeervin3<-lm(vines~deer, vegandsign3)
mdeerlvin3<-lm(vines~log(deer), vegandsign3)
summary(mdeervin3)
summary(mdeerlvin3)
#loglinear fit is best. all are significant.

###r-squared values for input###
summary(lm(totalsdl~log(deer), vegandsign3))$r.squared
###0.6877756
summary(lm(nativsdl~log(deer), vegandsign3))$r.squared
###0.6185654
summary(lm(nonnativesdl~log(deer), vegandsign3))$r.squared
###0.7806632
summary(lm(vines~log(deer), vegandsign3))$r.squared
###0.777756

###deer and total seedlings####
p4<- ggplot(data=vegandsign3, aes(y=totalsdl, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab(expression(paste("Deer scats per 100", m^2)))+
  ylab("Total seedling abundance")+
  theme_bw() +
  theme(axis.line = element_line())+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_line(colour = NA),
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 5, y = 800, label = "r^2==0.69", parse=T)

####X vs 2 Y's same plot####
###I USED THIS INSTEAD OF THE INDIVIDUAL NATIVE AND EXOTIC PLOTS####
p5<-ggplot(vegandscat_reform3, aes(y=sdls, x=deer, color= native_exotic))+
  geom_smooth(se=FALSE, method="lm",formula=y~log(x))+
  geom_point()+
  xlab(expression(paste("Deer scats per 100", m^2)))+
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
  annotate("text", x = 5, y = 700, label = "native~r^2==0.62", parse=T)+
  annotate("text", x = 5, y = 550, label = "nonnative~r^2==0.78", parse=T)


###DEER and VINES###
p6<- ggplot(data=vegandsign3, aes(y=vines, x=deer))+
  geom_smooth(se=FALSE, colour="black",method="lm",formula=y~log(x))+
  geom_point()+
  xlab(expression(paste("Deer scats per 100", m^2)))+
  ylab("Vine abundance")+
  theme_bw() +
  theme(axis.line = element_line())+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         panel.grid.minor = element_line(colour = NA),
         panel.grid.major = element_line(colour = NA), legend.position="none")+
  theme(axis.title.x=element_text(size=9))+
  theme(axis.title.y=element_text(size=9))+
  annotate("text", x = 5, y = 220, label = "r^2==0.78", parse=T)


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
