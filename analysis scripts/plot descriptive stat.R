# Title: Prepare the TB data
# 
# Description: Reproduce Figure 1, Table 2
#
# Note:
#   We alter and jitter the location data to protect patients' privacy. The resulting 
#   analysis may be slightly different compared to the paper.
#
# Requirement:
#   Packages: ggplot2, patchwork, scales, lr2cluster
#


library(ggplot2)
library(patchwork)
library(lr2cluster)


source("./analysis scripts/tb_valencia.R")


## ***** Plot 1 **********
## ***********************
tmp <- as.data.frame(table(rawdt$Cluster))
names(tmp) <- c("Cluster", "Freq")
tmp$group <- ifelse(tmp$Cluster == "unique", "Unclustered", "Clustered")

piechart <- ggplot(tmp %>% group_by(group) %>% summarise(Value = sum(Freq)), aes(x = "", y = Value, fill = group)) +
  geom_bar(width = 1, stat = "identity") + scale_fill_manual(values = c("coral", "light blue")) +
  coord_polar("y", start=0) + theme_light() + labs(x = "", y = "") + 
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_text(aes(label = scales::percent(Value/nrow(rawdt))), position = position_stack(vjust = 0.5))

barchart <- ggplot(tmp %>% filter(Cluster != "unique") %>% group_by(Freq) %>% summarise(n = n()), aes(x = factor(Freq), y = n)) +
  geom_bar(width = .75, stat = "identity", fill = "coral") +
  coord_flip() + theme_light() + labs(x = "Cluster Size", y = "Frequency")

piechart + barchart
#



## ***** Table 2 **********
## ************************
clustdt <- dt %>% filter(Cluster != "unique")
lapply(clustdt[,-c(1,2,7,8)], function(x) table(x))

pairdt <- zCovariate(clustdt$Cluster, clustdt[,3:6])
lapply(pairdt[,4:7], function(x) table(x))

