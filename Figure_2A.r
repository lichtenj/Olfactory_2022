library(ggplot2)
library(ggrepel)
library(tibble)
library(dplyr)
data=read.csv("06242022_withUID_outliersRemoved_deltaUPSIT.csv",header=TRUE,sep=",")
dataclean <-data[(data$Group!="?/PD" & data$Group!="GD/ND" & data$Group!="WT/FH" & data$Group!="Conv" & data$Group!="GD3/PD" & data$Group!="WT"),]
is_outlier <- function(x) { 
    return(x < quantile(x, 0.25,na.rm= TRUE) - 1.5 * IQR(x,na.rm=TRUE) | x > quantile(x, 0.75,na.rm= TRUE) + 1.5 * IQR(x,na.rm=TRUE)) 
}
datamod <- dataclean %>% tibble::rownames_to_column(var="outlier") %>% group_by(Since.First.Years,Group) %>% mutate(is_outlier=ifelse(is_outlier(Total.1), Total.1, as.numeric(NA))) 
datamod$outlier[which(is.na(datamod$is_outlier))] <- as.numeric(NA)
datamod = datamod %>% mutate(OUT_UID=ifelse(!(is.na(outlier)),SubjectID,NA))
datamod2<-subset(datamod,(datamod$Total.1!=""))
labeling <- datamod2 %>% group_by(Group) %>% summarize(n=length(which(table(SubjectID)>1)))
hum_names <- as_labeller(function(x) paste(x,'n=', labeling[labeling $Group==x,]$n))
datamod2 %>% group_by(Group) %>% mutate(dummy_var = as.character(x = factor(x = as.factor(SubjectID),labels = seq_len(length.out = n_distinct(x = as.factor(SubjectID)))))) %>% ungroup() %>% ggplot(mapping = aes(x = Since.First.Years, y = Total.1)) + geom_line(mapping = aes(colour = dummy_var),size=1) + facet_wrap(. ~ Group, labeller=hum_names)  + scale_color_discrete(guide = "none") + labs(x="Time since first visit (Years)",y="UPSIT Score") + geom_hline(yintercept=35,linetype='dashed') + coord_cartesian(ylim=c(0,40))
