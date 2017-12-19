#compare two datasets (seedlingplots.csv and ungulate2.csv) to make sure they are the same

ungulate <- read.csv("data/tidy data/ung_seedlingplot_tidy.csv")

ungulate2 <- read_csv("~/Box Sync/EBL/EBL Long Term Data Repository/Food Web/Food Web exclosures/Plant data/Ungulate_FWplots/data/raw data/ungulate2.csv")

#assign class of each column
factor_cols <- c("island","site", "species", "trt")
numeric_cols <- c("numplant", "alive", "dead")
ungulate2[factor_cols] <- lapply(ungulate2[factor_cols], as.factor)
ungulate2[numeric_cols] <- lapply(ungulate2[numeric_cols], as.numeric)
#change format of dates
ungulate2$plantdt <- dmy(ungulate2$plantdt)
ungulate2$checkdt <- dmy(ungulate2$checkdt)

#create growtime factors for analysis
ungulate2$growtime <- ungulate2$checkdt - ungulate2$plantdt
#standardize time - may help with convergence
ungulate2$growtime_st <- scale(ungulate2$growtime)

#create response variables
ungulate2$survival <- cbind(ungulate2$alive,ungulate$dead)
ungulate2$propalive <- ungulate2$alive/ungulate$numplant

test<-as.data.frame(anti_join(ungulate2, ungulate))
test<-test[test$island=="guam",]
