# Title: Figure 3
# 
# Description:
#   We estimate the expected cluster size per each cluster after assigning
#   new cases to their most probable clusters.
#
# Requirement:
#   Packages: lr2cluster, dplyr, doSNOW, foreach, tcltk, caret
#



library(lr2cluster)
library(dplyr)



# load data
source("./analysis scripts/tb_valencia.R")
dt <- dt %>% filter(Cluster != "unique")


# permutation to split data
set.seed(12345)
id <- caret::createDataPartition(dt$Cluster, 1, .6, F)
traindt <- dt[id,]
testdt <- dt[-id,]

cost <- 17
#



## ***** Draw plot *******
## ***********************
mod <- Cluster ~ Latitude+Longitude+Foreign

fit <- plr(mod, traindt) 
pred <- predict(fit, testdt, case.id = testdt$ID)
true_y <- zCovariate(testdt$Cluster, id = testdt$ID)

pi <- optThreshold(true_y$y, pred$y, cost.ratio = cost)
plot(pi)
