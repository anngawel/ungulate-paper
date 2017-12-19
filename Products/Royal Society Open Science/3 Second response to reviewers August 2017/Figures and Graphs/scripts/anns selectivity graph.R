###Import data set####
selectivity <- read.csv("selectivity_r.csv", header=T)

summary(selectivity)

##Load packages to clean and analyze data
library(tidyr)
library(dplyr) #note that many functions are similar for plyr and dplyr but have slightly different syntax in dplyr. Use the newer one (dplyr) to avoid issues between two packages. 
library(ggplot2)

selectivity$season <- tolower(selectivity$season)
selectivity$nat_exo <- tolower(selectivity$nat_exo)
pd <- position_dodge(width=0.3)

##BOTH NATIVE AND EXOTIC###

ggplot(selectivity, aes(season,beta, shape=nat_exo)) +
  geom_point(aes(shape=nat_exo),size=5, position=pd) + 
  geom_hline(yintercept=0.5) +
  scale_color_manual(name="Native or Exotic",values=c("coral", "grey")) + 
  scale_shape_manual(name="Native or Exotic",values=c(19,15), labels=c("Exotic", "Native")) + 
  theme_bw() + 
  scale_y_continuous("Manly's Selectivity Index") +
  scale_x_discrete(labels=c("Dry Season", "Wet Season"))+
  geom_errorbar(aes(ymin=beta-se,ymax=beta+se),width=0.2, position=pd)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(legend.title=element_blank()) + theme(legend.position="none")

###ONLY NATIVE###
###not sure how I did this before, trying to take out without changing csv file###

##modified data set to take out exotic points##
selectivity$nat_exo<-as.class(selectivity$nat_exo)
levels(selectivity$nat_exo)[levels(selectivity$nat_exo)=="native "] <- "native"
selectivity2 <- selectivity[ which(selectivity$nat_exo =='native'), ]
summary(selectivity2)
str(selectivity2)

###dp2 is not working, hmm....###
dp2<-ggplot(selectivity2, aes(season,beta)) +
  geom_point(aes(shape=nat_exo, ymin=0, ymax=1),size=5, position=pd) + 
  geom_hline(yintercept=0.5) +
  theme_bw() + 
  scale_y_continuous("Manly's Selectivity Index") + 
  scale_x_discrete(labels=c("Dry season", "Wet season"))+
  geom_errorbar(aes(ymin=beta-std_err,ymax=beta+std_err),width=0.2, position=pd)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(legend.title=element_blank()) + theme(legend.position="none")
 
### graph to compare higher germination from pigs vs deer ##


###THIS WORKS####
###try with manually built dataset###
dvp <- data.frame(
  deer_pig = factor(c("deer", "pig")),
  avg_sdls = c(0.5, 53.45161),
  se = c(1.33857, 9.118648))

##define limits of error bars## this is actually working##
limits <- aes(ymax = avg_sdls + se, ymin=avg_sdls - se)

dp1<-ggplot(dvp, aes(x=deer_pig, y=avg_sdls)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = c("black", "gray")) +
  scale_y_continuous("Average seedlings per faecal group") +
  geom_errorbar(limits, position="dodge", width=0.25)

###MULTIPLOT###
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

par(mfrow=c(2,1))

multiplot(dp1,dp2,cols=2)
