library(data.table)
library(dtplyr)
library(dplyr)
library(ggplot2)
library(spatstat)

Felony=fread("Felony.csv")
Felony$CompStatMonth=factor(Felony$CompStatMonth,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))

ggplot(Felony%>%filter(OccurrenceYear==2015),aes(x=XCoordinate,y=YCoordinate))+geom_point()

ggplot(Felony%>%filter(OccurrenceYear==2015),aes(x=CompStatMonth))+geom_histogram(stat="count")+facet_grid(~as.factor(`Borough`))