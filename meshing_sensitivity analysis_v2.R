

#meshing sensitivity analysis


#orientation 2

setwd('C:/Users/wilso/Documents/Research/dissertation/ANSYS St. James files')

library(readr)
library(ggplot2)
library(ggpubr)

rake2.0.027<-read.csv('orientation2_rake2_0.027.csv')
rake2.0.03<-read.csv('orientation2_rake2_0.03_2.csv')
rake2.0.04<-read.csv('orientation2_rake2_0.04.csv')


orientation2rake2<-rbind(rake2.0.027,rake2.0.03,rake2.0.04)
type2<-c(rep("0.027",99),rep("0.03",99),rep("0.04",99))
orientation2rake2$type<-type2

ggplot(data=orientation2rake2)+geom_point(aes(x=nodenumber,y=velocity.magnitude,colour=type))+
  scale_colour_discrete(name="Body Sizing Element Size")+
  scale_y_continuous(name="Velocity Magnitude")+
  scale_x_continuous(name="Node Number")

