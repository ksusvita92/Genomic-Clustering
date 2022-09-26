# Title: Estimate the expected cluster size
# 
# Description: 
#   Estimate the expected cluster size after assigning unclustered cases
#   into known clusters.
#
# Requirement:
#   Packages: lr2cluster, dplyr, doSNOW, foreach, tcltk, ggplot2, caret
#



library(lr2cluster)
library(dplyr)
library(doSNOW)
library(foreach)
library(tcltk)
library(ggplot2)




## ****** Configurations *********
## *******************************
N <- 10 #how many times to split the data
p <- .6 #approx. proportion of splitting data to train vs test set
r <- 0 #proportion of unclustered cases
method <- c("plr", "mlr") #add "random" if you want random assignment
cl_nm <- "tr_cl"

dt <- tb_valencia %>% filter(tr_cl != "unique") #change this with your data

mod <- tr_cl ~ latitude+longitude+foreign #formula for your model
  #NOTE: if spatial distance is one of the predictor, you have to name the geo
  #locatin to latitude (or lat) and longitude (or long).
## ***************



## ******* Define a function to compute accuracy for all ******
## ************************************************************
getallsize <- function(model, rho, method, data, newdata){
  res <- NULL
  cl_col <- data %>% pull(cl_nm)
  test_cl <- newdata %>% pull(cl_nm)
  
  for(m in method){
    if(m == "plr") assgn. <- clusterPLR(model, data, newdata, nbest = 1)
    else if(m == "mlr") assgn. <- clusterMLR(model, data, newdata, nbest = 1)
    else if(m == "random") assgn. <- clusterRandom(cl_col, nrow(newdata), nbest = 1)
    
    tmp <- clusterSize(assgn., rho) %>% mutate(method = m, msg = NA)
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
id <- caret::createDataPartition(dt[,cl_nm], N, p)

tr_sz <- as.data.frame(table(dt %>% pull(cl_nm)))
names(tr_sz) <- c("cluster", "tr_size")

cl <- makeCluster(parallel::detectCores(), type="SOCK")
registerDoSNOW(cl)

mysize <- foreach(i = 1:N, .combine = rbind, .packages = c("lr2cluster", "dplyr"), .options.snow = list(progress=progress), .errorhandling = "pass") %dopar% {
  traindt <- dt[id[[i]],]
  testdt <- dt[-id[[i]],]
  n0 <- as.data.frame(table(traindt %>% pull(cl_nm)))

  tmp <- tryCatch(getallsize(mod, r, method, traindt, testdt) %>% mutate(rho = r),
                  error = function(e) data.frame(cluster = NA, increment = NA, method = NA, rho = NA, msg = conditionMessage(e)))
  tmp %>% left_join(tr_sz, by = "cluster") %>% left_join(n0, by = c("cluster"="Var1")) %>% mutate(size = Freq+increment)
}

close(pb)
stopCluster(cl)
#  

# save the output
#write.csv(mysize, file = "exp_cluster_size.csv", row.names = F)
#



# create expected cluster size without unclustered cases
mysize %>% 
  ggplot() +
  geom_violin(aes(x = method, y = size, fill = method), alpha = .4) +
  geom_hline(aes(yintercept = tr_size), lty = "dashed") +
  facet_wrap(~cluster) +
  theme_light() +
  labs(x = "", y = "size")
#



