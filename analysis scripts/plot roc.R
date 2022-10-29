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
library(patchwork)



# load data
source("./analysis scripts/tb_valencia.R")
dt <- dt %>% filter(Cluster != "unique")
ut <- rawdt %>% filter(Cluster == "unique")


# permutation to split data
set.seed(415614)
id <- caret::createDataPartition(dt$Cluster, 1, .6, F)
traindt <- dt[id,]
testdt <- dt[-id,] #exclude unclustered cases
testdt2 <- bind_rows(testdt, ut) #include unclustered cases

cost1 <- 13
cost2 <- 1002
#



## ***** Draw plot *******
## ***********************
mod <- Cluster ~ Latitude+Longitude+Foreign

fit <- plr(mod, traindt) 
pred <- predict(fit, testdt, case.id = testdt$ID)
pred2 <- predict(fit, testdt2, case.id = testdt2$ID)
true_y <- zCovariate(testdt$Cluster, id = testdt$ID)
true_y2 <- zCovariate(testdt2$Cluster, id = testdt2$ID)


pi1 <- optThreshold(true_y$y, pred$y, cost.ratio = cost1)
pi2 <- optThreshold(true_y2$y, pred2$y, cost.ratio = cost2)


plot(pi1, col = "coral") 
plot(pi2, col = "light blue", add = TRUE)

