# Title: Estimate the expected cluster size
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
library(doSNOW)
library(foreach)
library(tcltk)




## ****** Configurations *********
## *******************************
method <- c("plr", "mlr")
cl_col_nm <- "Cluster"

# load data
source("./analysis scripts/tb_valencia.R")
ut <- dt
dt <- ut %>% filter(Cluster != "unique")
ut <- ut %>% filter(Cluster == "unique")

# permutation to split data
set.seed(123)
id <- caret::createDataPartition(dt$Cluster, 1000, .6)

mod <- Cluster ~ Latitude+Longitude+Foreign
tr_sz <- as.data.frame(table(dt %>% pull(cl_col_nm)))
names(tr_sz) <- c("cluster", "tr_size")




## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallsize <- function(model, rho, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_col_nm)
  test_cl <- newdata %>% pull(cl_col_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- clusterPLR(model, data, newdata, nbest = 1)
    else if(m == "mlr") assgn. <- clusterMLR(model, data, newdata, nbest = 1)
    
    tmp <- clusterSize(assgn., rho) %>% mutate(method = m)
    res <- bind_rows(res, tmp)
  }
  
  res
}
#



## ****** Parallel configuration ******
## ************************************
pb <- tkProgressBar(paste("Cluster size"), "Progress...", 0, length(id), 0)
progress <- function(n){
  info <- sprintf("%1.0f%% done", n/length(id)*100)
  setTkProgressBar(pb, n, paste("Cluster size"), info)
} 
#

# Perform parallel computation
cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

mysize <- foreach(i = 1:length(id), .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress), .errorhandling = "pass") %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  n0 <- as.data.frame(table(traindt %>% pull(cl_col_nm)))
  r <- nrow(ut)/(nrow(testdt)+nrow(ut))
  
  tmp1 <- getallsize(mod, 0, method, traindt, testdt) %>% mutate(rho = 0)
  tmp2 <- getallsize(mod, r, method, traindt, bind_rows(testdt, ut)) %>% mutate(rho = r)
  bind_rows(tmp1, tmp2) %>% left_join(tr_sz, by = "cluster") %>% left_join(n0, by = c("cluster"="Var1")) %>% mutate(size = Freq+increment)
}

close(pb)
stopCluster(cl)
#  

# save the output
#write.csv(mysize, file = "./results/exp_cluster_size.csv", row.names = F)
#




