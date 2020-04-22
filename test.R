library('ggplot2')
setwd('C:/Users/82103/Desktop/purdue project/shiny/My_App')
#read data with .csv file
Ideal_Point_Data <- read.csv("data/PropTesting.csv", stringsAsFactors=F)
head(Ideal_Point_Data)
pp<-ggplot(data=Ideal_Point_Data, aes(x=sequence, y=prop0))+geom_line(color='#FFAA00', lwd=1)+geom_line(aes(x=sequence, y=prop1), color='red')+geom_line(aes(x=sequence, y=prop2), color='blue')
pp
