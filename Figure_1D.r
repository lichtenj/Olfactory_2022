library(ggplot2)
library(ggpubr)
library(tidyverse)
data=read.csv("06242022_withUID_outliersRemoved_deltaUPSIT.csv",header=TRUE,sep=",")
newdata<-data[(data$Group!="?/PD" & data$Group!="GD/ND" & data$Group!="WT/FH" & data$Group!="Conv" & data$Group!="GD3/PD" & data$LastVisit==TRUE & (data$Genotype=="N370S/N370S" | data$Genotype=="N370S/L444P" | data$Genotype=="N370S/wt" | data$Genotype=="L444P/wt")),]
stat_pvalue <- newdata %>% group_by(Genotype) %>% rstatix::wilcox_test(Total.1 ~ Group) %>% rstatix::add_significance("p") %>% rstatix::add_y_position() %>% mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
ggplot(newdata, aes(x=Group, y=Total.1)) + geom_boxplot() +facet_wrap(.~Genotype)+ ggpubr::stat_pvalue_manual(stat_pvalue, hide.ns=TRUE, step.increase=0.1, label = "p.signif") + theme_bw(base_size = 16) + theme(axis.text.x=element_text(size=10)) + labs(x="Group",y="UPSIT Score") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + geom_text(aes(label=paste("n=",..count..)), y=5, stat='count', colour="red", size=4) + coord_cartesian(ylim=c(0,50))
