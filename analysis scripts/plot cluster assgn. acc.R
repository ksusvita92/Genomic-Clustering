# Title: Reproduce Figure 5 and Table 4
# 
# Description:
#
# Requirement:
#   Packages: ggplot, dplyr
#



library(ggplot2)
library(dplyr)

dt <- read.csv("./results/cluster_assgn_acc.csv")
dt$method <- factor(dt$method, levels = c("plr", "mlr", "random"))



## ******* Create boxplot *******
## ******************************
dt %>% ggplot() +
  geom_boxplot(aes(x = K, y = acc, fill = method), alpha = .4) +
  facet_wrap(~model) +
  labs(x = "", y = "accuracy") +
  theme_light() +
  theme(legend.position = "top", legend.title = element_blank())
#



## ******* Table 4 *******
## ***********************
dt %>% group_by(model, method, K) %>% summarise(m.acc = mean(acc), sd.acc = sd(acc))


