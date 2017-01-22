# Nighthold DPS Comparison - Heroic
JyuNyar  
Jan 22, 2017  



#Data Summary

Item level Distribution Heroic: 

###Heroic ilvl Distribution

```r
ggplot(parses%>%filter(difficulty=="Heroic",itemLevel>865),aes(x=itemLevel))+geom_histogram(binwidth=1)+ggtitle("ilvl Distrution for all Heroic Parses")
```

![](dps-boxplots_files/figure-html/heroic_hist-1.png)<!-- -->

###Mythic ilvl Distribution

```r
ggplot(parses%>%filter(difficulty=="Mythic",itemLevel>865),aes(x=itemLevel))+geom_histogram(binwidth=1)+ggtitle("ilvl Distrution for all Mythic Parses")
```

![](dps-boxplots_files/figure-html/mythic_hist-1.png)<!-- -->


##Data Cleanup

Data was cleaned using the median absolute deviation. For each spec + fight + difficulty combination, an exclusion criterion was calculated as follows:

$$Lower Bound = median - 2.5*MAD$$
$$Upper Bound = median + 2.5*MAD$$

Observations that fell either below the lower bound, or above the upper bound, were excluded.


```r
parses.filtered=parses%>%
  group_by(spec2,Fight,difficulty)%>%
  mutate(
    Count=n(),
    Omit=ifelse(Count<100,1,0))%>%
  filter(Omit==0,itemLevel>865)%>%
  ungroup()%>%
  group_by(spec2)%>%
  mutate(
    fullMedian=median(total))%>%
  ungroup()%>%
  group_by(spec2,Fight,difficulty)%>%
  mutate(
    lower = (median(total)-2.5*mad(total)),
    medianDPS=median(total),
    upper = (median(total)+2.5*mad(total)))%>%
  ungroup()%>%
  mutate(
    Outlier=ifelse(total<lower,1,
                   ifelse(total>upper,1,0)))%>%
  filter(Outlier==0)
```

Plotting function:


```r
boxplot=function(fight,diff,pal,data){
    ggplot(
      data%>%
        filter(Fight==fight,difficulty==diff),aes(x=reorder(spec2,medianDPS),y=total,color=spec2))+
    geom_boxplot(lwd=1)+
    scale_colour_manual(values=pal)+
    theme(
      axis.text.x=element_text(angle=90,vjust=1),
      axis.title.x=element_blank(),
      text=element_text(size=16))+
    labs(title=fight)
}
```

Palette:

```r
palette.alphabetical=c("#9482C9","#69CCF0","#C79C6E","#EDC84F","#FF7D0A","#ABD473","#7661B2","#BDAEE8","#0070DE","#63AAF0","#FFAA60","#2AC1F8","#48AACD","#C41F3B","#9B7954","#A330C9","#8BD428","#FFC300","#F58CBA","#000000","#E3C153","#BDD69A","#97001A","#00FF96")
```

#Heroic {.tabset}
##Overall

```r
ggplot(
      parses.filtered%>%filter(difficulty=="Heroic"),aes(x=reorder(spec2,medianDPS),y=total,color=spec2))+
    geom_boxplot(lwd=1)+
    scale_colour_manual(values=palette.alphabetical)+
    theme(
      axis.text.x=element_text(angle=90,vjust=1),
      axis.title.x=element_blank(),
      text=element_text(size=16))+
    labs(title="Heroic Nighthold")
```

![](dps-boxplots_files/figure-html/heroic_all-1.png)<!-- -->

##Heroic Skorpyron

```r
boxplot("Skorpyron","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_skorpyron-1.png)<!-- -->

##Heroic Chronomatic Anomaly

```r
boxplot("Chronomatic Anomaly","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_anomaly-1.png)<!-- -->

##Heroic Trilliax

```r
boxplot("Trilliax","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_trilliax-1.png)<!-- -->

##Heroic Spellblade Aluriel

```r
boxplot("Spellblade Aluriel","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_aluriel-1.png)<!-- -->

##Heroic Tichondrius

```r
boxplot("Tichondrius","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_tichondrius-1.png)<!-- -->

##Heroic Krosus

```r
boxplot("Krosus","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_krosus-1.png)<!-- -->

##Heroic High Botanist Tel'arn

```r
boxplot("High Botanist Tel'arn","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_botanist-1.png)<!-- -->

##Heroic Grand Magistrix Elisande

```r
boxplot("Grand Magistrix Elisande","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_elisande-1.png)<!-- -->

##Heroic Gul'dan

```r
boxplot("Gul'dan","Heroic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/heroic_guldan-1.png)<!-- -->

#Mythic {.tabset}
##Overall

```r
ggplot(
      parses.filtered%>%filter(difficulty=="Heroic"),aes(x=reorder(spec2,medianDPS),y=total,color=spec2))+
    geom_boxplot(lwd=1)+
    scale_colour_manual(values=palette.alphabetical)+
    theme(
      axis.text.x=element_text(angle=90,vjust=1),
      axis.title.x=element_blank(),
      text=element_text(size=16))+
    labs(title="Mythic Nighthold")
```

![](dps-boxplots_files/figure-html/Mythic_all-1.png)<!-- -->

##Mythic Skorpyron

```r
boxplot("Skorpyron","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_skorpyron-1.png)<!-- -->

##Mythic Chronomatic Anomaly

```r
boxplot("Chronomatic Anomaly","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_anomaly-1.png)<!-- -->

##Mythic Trilliax

```r
boxplot("Trilliax","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_trilliax-1.png)<!-- -->

##Mythic Spellblade Aluriel

```r
boxplot("Spellblade Aluriel","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_aluriel-1.png)<!-- -->

##Mythic Tichondrius

```r
boxplot("Tichondrius","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_tichondrius-1.png)<!-- -->

##Mythic Krosus

```r
boxplot("Krosus","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_krosus-1.png)<!-- -->

##Mythic High Botanist Tel'arn

```r
boxplot("High Botanist Tel'arn","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_botanist-1.png)<!-- -->

##Mythic Grand Magistrix Elisande

```r
boxplot("Grand Magistrix Elisande","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_elisande-1.png)<!-- -->

##Mythic Gul'dan

```r
boxplot("Gul'dan","Mythic",palette.alphabetical,parses.filtered)
```

![](dps-boxplots_files/figure-html/Mythic_guldan-1.png)<!-- -->
