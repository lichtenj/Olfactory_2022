library(ggplot2)
library(ggrepel)
library(tibble)
library(dplyr)
data=read.csv("06242022_withUID_outliersRemoved_deltaUPSIT.csv",header=TRUE,sep=",")
newdata<-data[(data$Group!="?/PD" & data$Group!="GD/ND" & data$Group!="WT/FH" & data$Group!="Conv" & data$Group!="GD3/PD" & data$Group!="WT" & data$Group!="GD" & data$Group!="GD/PD" & data$Group!="GD/FH" & data$Since.Binned!="9-12"),]
stat_pvalue <- newdata %>% group_by(Since.Binned) %>% rstatix::wilcox_test(Total.1 ~ Group) %>% rstatix::add_significance("p") %>% rstatix::add_y_position() %>% mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
labeling <- newdata %>% group_by(Since.Binned) %>% tally()
hum_names <- as_labeller(function(x) paste(x,'n=', labeling[labeling $Group==x,]$n))
ggplot(newdata, aes(x=Group, y=Total.1)) + geom_boxplot() + ggpubr::stat_pvalue_manual(stat_pvalue, hide.ns=TRUE, step.increase=0.1, label = "p.signif") + facet_wrap(. ~ Since.Binned, ncol=2) + geom_text(aes(label=paste("n=",..count..)), y=10, stat='count', colour="red", size=4) + coord_cartesian(ylim=c(0,72)) + labs(x="Groups",y="UPSIT Score",title="Heterozygous Cases")
