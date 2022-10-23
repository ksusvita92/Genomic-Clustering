# Title: Fit & predict pairwise logistic regression model
# 
# Description:
#
# Requirement:
#   Packages: lr2cluster, dplyr
#


library(lr2cluster)
library(dplyr)


## ***** Load TB Valencia data *****
## *********************************
source("./analysis scripts/tb_valencia.R")
clus_dt <- dt %>% filter(Cluster != "unique")
#



## ***** Split the data into training vs testing sets *****
## ********************************************************
set.seed(123)
id <- caret::createDataPartition(clus_dt$Cluster, 1, .6, F)

traindt <- clus_dt[id,]
testdt <- clus_dt[-id,]

mod1 <- Cluster ~ Latitude+Longitude+Gender+Diabetes+HIV+Foreign
mod2 <- Cluster ~ Latitude+Longitude+Foreign
fit1 <- plr(mod1, clus_dt) 
fit2 <- plr(mod2, clus_dt)
summary(fit1); summary(fit2) #this is the results in Table 3
#



## **** Get optimum threshold *******
## **********************************
fit3 <- plr(mod2, traindt) 
pred <- predict(fit3, testdt, case.id = testdt$ID)
true_y <- zCovariate(testdt$Cluster, id = testdt$ID)

pi0 <- optThreshold(true_y$y, pred$y, cost.ratio = 50)
plot(pi0)
#

