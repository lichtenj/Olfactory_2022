library(ggplot2)
library(ggpubr)
library(tidyverse)
data=read.csv("06242022_withUID_outliersRemoved_deltaUPSIT.csv",header=TRUE,sep=",")
newdata<-data[(data$Group!="?/PD" & data$Group!="GD/ND" & data$Group!="WT/FH" & data$Group!="Conv" & data$Group!="GD3/PD" & data$Group!= "WT" & data$LastVisit!=FALSE),]
stat_pvalue <- newdata %>% rstatix::wilcox_test(Total.1 ~ Group) %>% filter(p < 0.05) %>% rstatix::add_significance("p") %>% rstatix::add_y_position() %>% mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
ggplot(newdata, aes(x=Group, y=Total.1)) + geom_boxplot() + ggpubr::stat_pvalue_manual(stat_pvalue, label = "p.signif") + theme_bw(base_size = 16) + labs(x="Group",y="UPSIT Score") + geom_text(aes(label=paste("n=",..count..)), y=2, stat='count', colour="red", size=4) + coord_cartesian(ylim=c(0,65))
