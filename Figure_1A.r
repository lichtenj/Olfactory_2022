library(ggplot2)
library(ggrepel)
library(tibble)
data=read.csv("06242022_withUID_outliersRemoved_deltaUPSIT.csv",header=TRUE,sep=",")
newdata<-data[((data$Group=="GD" | data$Group=="GC" | data$Group=="GD/FH" | data$Group=="GC/FH") & data$VisitNumber==0),]
ggplot(newdata, aes(x=AgeBin, y=Total.1)) + geom_boxplot(aes(group=AgeBin)) + labs(x="Age Range (Decades)",y="UPSIT Score", title="Non-Parkinson Cases") + geom_text(aes(label=paste("n=",..count..)), y=5, stat='count', colour="red", size=4) + coord_cartesian(ylim=c(0,40),xlim=c(5,85))
