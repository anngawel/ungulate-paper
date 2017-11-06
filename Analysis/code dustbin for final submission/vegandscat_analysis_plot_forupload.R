################################################
####   Analysis and ggplot for          ########
####   ungulate abundance               ########
####   vs plant communities             ########
###    last  modified by AMG            ########
####   Oct 19, 2017                      ########
################################################

###load library###
library(ggplot2)
library(lme4)
library(AICcmodavg)
library(MuMIn)

###upload datasets###
##both datasets are used for figures, but vegandsign.csv used for analysis
vegandsign<-read.csv("vegandsign.csv")
##below, reformatted csv where native/exotic is long-form column instead of
###two columns#####
vegandscat_reform<-read.csv("vegandscat_reform.csv")

summary(vegandsign)
str(vegandsign)
summary(vegandscat_reform)
str(vegandscat_reform)


###################
###LINEAR MODELS###
###################

##First, test lm with different response variables [total seedlings, native and exotic seedlings, and vines] with both pig and deer scat densities:

##LOOK AT MODELS PREDICTING TOTAL SEEDLING ABUNDANCE FIRST##
bothtot <-lm(totalsdl~pig*deer, vegandsign)
nointxntot<-lm(totalsdl~pig+deer, vegandsign)
pigtot <-lm(totalsdl~pig, vegandsign)
deertot <-lm(totalsdl~deer, vegandsign)
aictab(list(bothtot, nointxntot, pigtot, deertot))

###MODEL 4 (deertot) WHERE DEER SCAT DENSITIES ARE THE ONLY PREDICTIVE VARIABLE HAS THE BEST FIT

##Model selection based on AICc:
  
#     K   AICc Delta_AICc AICcWt Cum.Wt      LL
#Mod4 3 206.06       0.00   0.80   0.80  -98.83
#Mod2 4 209.55       3.49   0.14   0.94  -98.56
#Mod3 3 211.90       5.84   0.04   0.99 -101.75
#Mod1 5 214.26       8.20   0.01   1.00  -98.38


###SEPARATE SEEDLINGS INTO NATIVE AND NON-NATIVE TO TEST###
##NATIVE##
bothnat <- lm(nativsdl~pig+deer, vegandsign)
pignat <-lm(nativsdl~pig, vegandsign)
deernat <-lm(nativsdl~deer, vegandsign)
aictab(list(bothnat, pignat, deernat))

###MODEL 3 (deernat) WHERE DEER SCAT DENSITIES ARE THE ONLY PREDICTIVE VARIABLE HAS THE BEST FIT FOR NATIVE SEEDLINGS###

#Model selection based on AICc:
  
#     K   AICc Delta_AICc AICcWt Cum.Wt     LL
#Mod3 3 202.53       0.00   0.81   0.81 -97.07
#Mod1 4 205.93       3.40   0.15   0.95 -96.74
#Mod2 3 208.26       5.73   0.05   1.00 -99.93


##NON-NATIVE##
bothexo <- lm(nonnativesdl~pig+deer, vegandsign)
pigexo <-lm(nonnativesdl~pig, vegandsign)
deerexo <-lm(nonnativesdl~deer, vegandsign)
aictab(list(bothexo, pigexo, deerexo))

###MODEL 3 (deerexo) WHERE DEER SCAT DENSITIES ARE THE ONLY PREDICTIVE VARIABLE HAS THE BEST FIT FOR NON-NATIVE SEEDLINGS###

#Model selection based on AICc:
  
#     K   AICc Delta_AICc AICcWt Cum.Wt     LL
#Mod3 3 156.83       0.00   0.72   0.72 -74.22
#Mod2 3 159.64       2.81   0.18   0.90 -75.62
#Mod1 4 160.80       3.97   0.10   1.00 -74.18


###TEST MODELS WITH VINE ABUNDANCE###
bothvin <- lm(vines~pig+deer, vegandsign)
pigvin <-lm(vines~pig, vegandsign)
deervin <-lm(vines~deer, vegandsign)
aictab(list(bothvin, pigvin, deervin))

###MODEL 3 (deerVIN) WHERE DEER SCAT DENSITIES ARE THE ONLY PREDICTIVE VARIABLE HAS THE BEST FIT FOR VINES###
#Model selection based on AICc:
  
#     K   AICc Delta_AICc AICcWt Cum.Wt     LL
#Mod3 3 167.50       0.00   0.85   0.85 -79.55
#Mod1 4 171.35       3.85   0.12   0.98 -79.45
#Mod2 3 174.59       7.09   0.02   1.00 -83.10

#######################################################################
###### Next, test different response variables with just pig scats#####
######          test both linear and loglinear                    #####
#######################################################################

##PIGS ON TOTAL SEEDLING ABUNDANCE##
##test both loglinear and linear relationships
logpigtot<-lm(totalsdl~log(pig), vegandsign)
linpigtot <- lm(totalsdl~pig, vegandsign)
summary(logpigtot2)
summary(linpigtot)

##PIGS ON NATIVE AND NON-NATIVE SEEDLING ABUNDANCE##
#test both linear and loglinear relationships
logpignat<-lm(nativsdl~log(pig), vegandsign)
linpignat <- lm(nativsdl~pig, vegandsign)
summary(logpignat)
summary(linpignat)

logpigexo<-lm(nonnativesdl~log(pig), vegandsign)
linpigexo<-lm(nonnativesdl~pig, vegandsign)
summary(logpigexo)
summary(linpigexo)

##PIGS ON VINE ABUNDANCE##
#test both linear and loglinear relationships
logpigvin<-lm(vines~log(pig), vegandsign)
linpigvin<-lm(vines~pig, vegandsign)
summary(logpigvin)
summary(linpigvin)

###### Just deer ##############
logdeertot<-lm(totalsdl~log(deer), vegandsign)
lindeertot<-lm(totalsdl~deer, vegandsign)
summary(logdeertot)
summary(lindeertot)

logdeernat<-lm(nativsdl~log(deer), vegandsign)
lindeernat<-lm(nativsdl~deer, vegandsign)
summary(logdeernat)
summary(lindeernat)

logdeernexo<-lm(nonnativesdl~log(deer), vegandsign)
lindeerexo<-lm(nonnativesdl~deer, vegandsign)
summary(logdeernexo)
summary(lindeerexo)

logdeervin<-lm(vines~log(deer), vegandsign)
lindeervin<-lm(vines~deer, vegandsign)
summary(logdeervin)
summary(lindeervin)



#################################################
##                  FIGURES                    ##
#################################################


###r-squared values for input###
summary(lm(totalsdl~pig, vegandsign))$r.squared
##[1] 0.001490327
summary(lm(nativsdl~pig, vegandsign))$r.squared
##[1] 0.002768039
summary(lm(nonnativesdl~pig, vegandsign))$r.squared
##[1] 0.02305202
summary(lm(vines~pig, vegandsign))$r.squared
##[1] 0.001429389


###PIG PLOTS, LEFT-HAND PANELS###

###PIG and TOTAL SEEDLINGS###  bquote('x axis'~(Å^2))
r01 <- paste("r^2 <", "0.01")
r02 <- paste("r^2 ==", "0.02")
p1<- ggplot(data=vegandsign, aes(y=totalsdl, x=pig))+
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
  annotate("text", x = 0.75, y = 800, label = "r^2<0.01", parse=T)


####PIG AND TWO TRENDLINES NATIVE EXOTIC SEEDLINGS###
p2<-ggplot(vegandscat_reform, aes(y=sdls, x=pig, color= native_exotic))+
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
  annotate("text", x = 0.7, y = 700, label = "native~r^2<0.01", parse=T)+
  annotate("text", x = 0.7, y = 600, label = "nonnative~r^2==0.02", parse=T)

###PIGS AND VINES####
p3<-ggplot(data=vegandsign, aes(y=vines, x=pig))+
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
  annotate("text", x = 0.75, y = 200, label = "r^2<0.01", parse=T)

####DEER PLOTS - RIGHT-HAND PANELS#################


###r-squared values for input###
summary(lm(totalsdl~log(deer), vegandsign))$r.squared
###0.7069415
summary(lm(nativsdl~log(deer), vegandsign))$r.squared
###0.6409852
summary(lm(nonnativesdl~log(deer), vegandsign))$r.squared
###0.7917839
summary(lm(vines~log(deer), vegandsign))$r.squared
###0.7917234

###deer and total seedlings####
p4<- ggplot(data=vegandsign, aes(y=totalsdl, x=deer))+
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
  annotate("text", x = 7.2, y = 800, label = "r^2==0.71", parse=T)

####X vs 2 Y's same plot####
###I USED THIS INSTEAD OF THE INDIVIDUAL NATIVE AND EXOTIC PLOTS####
p5<-ggplot(vegandscat_reform, aes(y=sdls, x=deer, color= native_exotic))+
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
  annotate("text", x = 7.2, y = 700, label = "native~r^2==0.64", parse=T)+
  annotate("text", x = 7.2, y = 600, label = "nonnative~r^2==0.79", parse=T)


###DEER and VINES###
p6<- ggplot(data=vegandsign, aes(y=vines, x=deer))+
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
  annotate("text", x = 7.2, y = 220, label = "r^2==0.79", parse=T)


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

###MULTIPLOT###
multiplot(p1,p2,p3,p4,p5,p6,cols=2)
