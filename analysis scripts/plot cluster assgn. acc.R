# Title: Plot Figure 5
# 
# Description:
#
# Requirement:
#   Packages: ggplot, dplyr
#


library(ggplot2)
library(dplyr)

dt <- read.csv("./results/cluster_assgn_acc.csv")



## ******* Create boxplot *******
## ******************************
dt %>% ggplot() +
  geom_boxplot(aes(x = K, y = acc, fill = method)) +
  facet_wrap(~model) +
  labs(x = "", y = "accuracy") +
  theme_light()
#