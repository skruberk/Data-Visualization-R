#makes things tidy for graphing in R
library(tidyverse)
library(readr)
library(dplyr)
library("writexl")
library(janitor)

#tidy_PA<-parse_number()
data <- remove_empty(data,c("rows"))
#tidy <- na.omit(data)
view(data)
data$group<-gsub('[[:digit:]]+', '',data$group)
data$group<- gsub('[0-9.]', '', data$group) #removes numbers from the group names
data$group <- gsub('-', '', data$group)
view(data)
write_xlsx(data,"C:/Users/~filepath/filename.xlsx")

#tidy<-data %>% gather(intensity, key="time", range(1:21))
tidy<-data %>% gather(intensity, key="group", starts_with("P"))
view(tidy)
tidy<-tidy %>% add_column(group="variable")
#tidy1<- gsub("t","",as.character(tidy$time))
select(tidy, -c(newColname, fill))
write_xlsx(data,"C:/Users/~filepath/filename.xlsx")
#tidy_PA<-  gather(data=data, key=(starts_with("t"), value=time ))
#tidy_PA<-  gather(data=data, key=(range(1:180) value=time )

data<- as.numeric("eGFP")
tidy<-na.omit(data)
view(tidy)
#data<-remove_empty(data(c("variable")))
tidy<-data %>% gather(key="group", value="y", na.rm=TRUE) #this is without rounds
view(tidy)

tidy<-data %>% gather(key="group", value="y", na.rm=TRUE) 
#this is without rounds
view(tidy)

csv <- read.csv(file = 'filename.csv', sep= ",",row.names=NULL, head=FALSE)
view(csv)
write_xlsx(csv,"C:/Users/~filepath/filename.xlsx")
