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
dt$method <- factor(dt$method, levels = c("plr", "mlr"))


## ******* Create boxplot *******
## ******************************
# create expected cluster size without unclustered cases
dt %>% 
  filter(rho == 0) %>%
  ggplot() +
  geom_violin(aes(x = method, y = size, fill = method), alpha = .4) +
  geom_hline(aes(yintercept = tr_size), lty = "dashed") +
  facet_wrap(~cluster, nrow = 4) +
  theme_light() + theme(legend.position = "top", legend.title = element_blank()) +
  labs(x = "", y = "size")

# create expected cluster size with unclustered cases
dt %>% 
  filter(rho != 0) %>%
  ggplot() +
  geom_violin(aes(x = method, y = size, fill = method), alpha = .4) +
  geom_hline(aes(yintercept = tr_size), lty = "dashed") +
  facet_wrap(~cluster, nrow = 4) +
  theme_light() +
  labs(x = "", y = "size")
#