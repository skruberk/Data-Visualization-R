library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(dplyr)
library(tidyverse)
library(viridis)
library(stringr)
library(writexl)
head(data)

#data should be in tidy format with variables input by group, replicate/round, output is y

#calculating averages of each replicate
ReplicateStat <- data %>% group_by(group,round)%>% 
  summarise(DataStat = mean(y), sd(y),n())
view(ReplicateStat)
#writes the output of file to your WD
write.csv(ReplicateStat,file='filename.csv')

print(typeof(data$group))
#REMOVE NAS
data<-na.omit(data)
view(data)

#view all the group names
unique(data$group)
data$group <- as.factor(data$group)
levels(data$group)
view(data)

#superplot start 
unique(data$group)
data$group <- as.factor(data$group)
levels(data$group)
data$group <- factor(data$group, levels=c("variable1","variable2","variable3"))  
view(data)
#make scatter plot and boxplot with overlaying replicate stat plot
p<- ggplot(data, aes(x=group, y=y))+ ylim(0,55)+ geom_boxplot(outlier.shape = NA, notch=TRUE) + geom_beeswarm(dodge.width=0.15,aes(color=round, alpha=0.01)) + scale_color_brewer(palette = "Paired")+geom_beeswarm(data=ReplicateStat, aes(x=group, y=DataStat, color=round, size=8))
p
p<-p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
#superplot end


#extras
geom_beeswarm(dodge.width = 0.5 ,aes(color=round)) #change jitter
#+scale_y_continuous(breaks=seq(0,15,by=5))      scale_colour_brewer(palette = "Paired")+ + scale_color_manual(values=c("#A6CEE3","#FB9A99","#B2DF8A","#FDBF6F"))
