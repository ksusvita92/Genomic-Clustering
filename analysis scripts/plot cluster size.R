# Title: Plot Figure 7
# 
# Description:
#
# Requirement:
#   Packages: ggplot, dplyr
#


library(ggplot2)
library(dplyr)

dt <- read.csv("./results/exp_cluster_size.csv")



## ******* Create boxplot *******
## ******************************
# create expected cluster size without unclustered cases
dt %>% 
  filter(rho == 0) %>%
  ggplot() +
  geom_violin(aes(x = method, y = size, fill = method), alpha = .4) +
  geom_hline(aes(yintercept = tr_size), lty = "dashed") +
  facet_wrap(~cluster) +
  theme_light() +
  labs(x = "", y = "size")

# create expected cluster size with unclustered cases
dt %>% 
  filter(rho != 0) %>%
  ggplot() +
  geom_violin(aes(x = method, y = size, fill = method), alpha = .4) +
  geom_hline(aes(yintercept = tr_size), lty = "dashed") +
  facet_wrap(~cluster) +
  theme_light() +
  labs(x = "", y = "size")
