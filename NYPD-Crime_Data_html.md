# NYPD Crime Data
Ben Goldy  
April 5, 2017  


#Descriptives {.tabset}
##Occurence Year

```r
ggplot(Felony,
       aes(x=OccurrenceYear))+
  geom_histogram()+xlab("Year")
```

![](NYPD-Crime_Data_html_files/figure-html/summary_year-1.png)<!-- -->

Its fairly clear that the present data is mostly from one year: 2015.

##Occurence Month

```r
ggplot(Felony,
       aes(x=OccurrenceMonth))+
  geom_histogram(stat="count")+
  xlab("Month")
```

![](NYPD-Crime_Data_html_files/figure-html/summary_occmonth-1.png)<!-- -->

##Reported Month

```r
ggplot(Felony,
       aes(x=CompStatMonth))+
  geom_histogram(stat="count")+
  xlab("Month")
```

![](NYPD-Crime_Data_html_files/figure-html/summary_reportedmonth-1.png)<!-- -->

##Crime Type

```r
crimetype=Felony%>%
  group_by(Offense)%>%
  mutate(Count=n())

ggplot(crimetype,
       aes(x=reorder(Offense,
                     Count)))+
  geom_histogram(stat="count")+
  xlab("Crime Type")+
  theme(
    axis.text.x=element_text(angle=90,vjust=1),
    text=element_text(size=8))
```

![](NYPD-Crime_Data_html_files/figure-html/summary_crimetype-1.png)<!-- -->

#Borough Interactions {.tabset}
##Total Crime by Borough

```r
ggplot(Felony,
       aes(x=Borough))+
  geom_histogram(stat="count")+
  xlab("Borough")+
  theme(
    axis.text.x=element_text(angle=90),
    text=element_text(size=8))
```

![](NYPD-Crime_Data_html_files/figure-html/borough_totalCrime-1.png)<!-- -->

##Borough by Crime Type

```r
ggplot(Felony,
       aes(x=Offense))+
  geom_histogram(stat="count")+
  xlab("Crime Type")+
  facet_grid(~Borough)+
  theme(
    axis.text.x=element_text(angle=90),
    text=element_text(size=8))
```

![](NYPD-Crime_Data_html_files/figure-html/borough_crimetype-1.png)<!-- -->

##Borough by Month

```r
ggplot(Felony,
       aes(x=OccurrenceMonth))+
  geom_histogram(stat="count")+
  xlab("Month")+
  facet_grid(~Borough)+
  theme(
    axis.text.x=element_text(angle=90),
    text=element_text(size=8))
```

![](NYPD-Crime_Data_html_files/figure-html/summary_borough_by_month-1.png)<!-- -->

#Spatial Data {.tabset}
##All Spatial Data

```r
ggplot(Felony%>%
         filter(OccurrenceYear==2015),
       aes(x=XCoordinate,
           y=YCoordinate))+
  geom_point()
```

![](NYPD-Crime_Data_html_files/figure-html/map-1.png)<!-- -->

##Colored by Borough

```r
ggplot(Felony%>%
         filter(OccurrenceYear==2015),
       aes(x=XCoordinate,
           y=YCoordinate,
           color=Borough))+
  geom_point()
```

![](NYPD-Crime_Data_html_files/figure-html/map_color-1.png)<!-- -->

##Colored by Precinct

```r
ggplot(Felony%>%
         filter(OccurrenceYear==2015),
       aes(x=XCoordinate,
           y=YCoordinate,
           color=as.factor(Precinct)))+
  geom_point()
```

![](NYPD-Crime_Data_html_files/figure-html/map_color_prcnct-1.png)<!-- -->

#Crime Patterns Overview

```r
ggplot(Felony%>%filter(Borough!=""),aes(x=XCoordinate,y=YCoordinate))+geom_density2d()+facet_grid(Borough~Offense,labeller=label_both)
```

![](NYPD-Crime_Data_html_files/figure-html/spatialpattern-1.png)<!-- -->

Data was accessed: http://www.nyc.gov/html/nypd/html/analysis_and_planning/historical_nyc_crime_data.shtml
Date accessed: April 5, 2017
