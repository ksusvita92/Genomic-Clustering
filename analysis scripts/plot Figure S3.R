# Title: Figure S3
# 
# Description:
#   Reproduce Figure S3
#
# Requirement:
#   Packages: dplyr, ggplot, patchwork, lr2cluster
#


library(ggplot2)
library(dplyr)
library(patchwork)
library(lr2cluster)


## ***** Split the data into training vs testing sets *****
## ********************************************************
source("./analysis scripts/tb_valencia.R")
dt <- dt %>% filter(Cluster != "unique")


set.seed(123)
id <- caret::createDataPartition(dt$Cluster, 1, .6, F)

traindt <- dt[id,]
testdt <- dt[-id,]


ptraindt <- zCovariate(traindt$Cluster, traindt[,4], traindt[,7:8], id = traindt$ID)
ptraindt$Spanish_born <- ifelse(ptraindt$Foreign == "No", 1, 0)
ptraindt$Foreign_born <- ifelse(ptraindt$Foreign == "YES", 1, 0)
ptraindt$Spanish_foreign <- ifelse(ptraindt$Foreign == "DIFF", 1, 0)

M <- cor(ptraindt[,c(4, 6:8)])
corrplot::corrplot(M, type = "upper", addCoef.col = "gray", tl.col = "black", tl.srt = 45)
