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
dt <- tb_valencia
clus_dt <- dt %>% filter(tr_cl != "unique") #filtered out unclustered cases
#



## ***** Split the data into training vs testing sets *****
## ********************************************************
load("./analysis scripts/id.RData") #load permutation data
id <- id[[836]] #pick one
traindt <- clus_dt[id,]
testdt <- clus_dt[-id,]

mod1 <- tr_cl ~ latitude+longitude+sex+diabetes+hiv+foreign
mod2 <- tr_cl ~ latitude+longitude+foreign
fit1 <- plr(mod1, clus_dt) 
fit2 <- plr(mod2, clus_dt)
summary(fit1); summary(fit2) #this is the results in Table 3
#



## **** Get optimum threshold *******
## **********************************
fit3 <- plr(mod2, traindt) 
pred <- predict(fit3, testdt, case.id = testdt$id_server)
true_y <- zCovariate(testdt$tr_cl, id = testdt$id_server)

pi0 <- optThreshold(true_y$y, pred$y, cost.ratio = 12)
plot(pi0)
#

