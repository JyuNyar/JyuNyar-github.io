library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(jsonlite)

#Dataframe with all the class, spec combinations to test:
iterate=data.frame(Class=c(1,1,2,2,3,3,3,4,4,4,5,6,7,8,8,8,9,9,10,10,10,11,11,12),Spec=c(2,3,1,2,1,2,3,1,2,3,3,3,3,1,3,4,1,2,1,2,3,1,2,1))
fights= data.frame(id=c(1849,1865,1867,1871,1862,1863,1842,1886,1872,1866),EncounterName=c("Skorpyron","Chronomatic Anomaly","Trilliax","Spellblade Aluriel","Tichondrius","Star Augur Etraeus","Krosus","High Botanist Tel'arn","Grand Magistrix Elisande","Gul'dan"))
slots=list(id=c(1:18),Slot=c("Helm","Neck","Shoulders","Tabard","Chest","Belt","Leggings","Boots","Wrists","Glove","Ring1","Ring2","Trinket1","Trinket2","Cloak","Weapon1","Weapon2","EmptySlot"))

#Loop and API lookup variables
maxdifficulty=5
difficultycounter=4
fightcounterMax = 10
fightcounter = 1
counter = 24
class.rep.count=1
class=1
spec=2
key="4d6a511f1df061d3729663bfeb8caad2"
encounter=1849
metric="dps"
difficulty=4 #4 = heroic, 5 = mythic
limit= 1000
reps = 1000
page=1

#This single call before the loops creates the data frames that I store all the data into

datalist=fromJSON(paste("https://www.warcraftlogs.com/v1/rankings/encounter/",encounter,"?api_key=",key,"&metric=",metric,"&difficulty=",difficulty,"&class=",class,"&spec=",spec,"&limit=",limit,"&page=",page,sep=""))
datalist.talents=datalist$rankings%>%select(name,reportID,fightID,duration,startTime,talents)%>%unnest()
datalist.talents$encounter=encounter
datalist.talents$difficulty=difficulty
datalist.gear=datalist$rankings%>%select(name,reportID,fightID,duration,startTime,gear)%>%unnest()
datalist.gear$encounter=encounter
datalist.gear$difficulty=difficulty
datalist=datalist$rankings%>%select(name,class,spec,total,reportID,fightID,duration,itemLevel,startTime)
datalist$encounter=encounter
datalist$difficulty=difficulty
merged=datalist
merged.talents=datalist.talents
merged.gear=datalist.gear

while(difficultycounter <= maxdifficulty){
  
  print(paste("*** Difficulty: ",difficultycounter," ***"))
  difficulty=difficultycounter
  
  while(fightcounter <= fightcounterMax){
    
  print(paste("*** Encounter: ",fights[fightcounter,1]," ","Name: ",fights[fightcounter,2]," ","Difficulty: ",difficulty ,"***"))
  encounter = fights[fightcounter,1]
  
    #Outer loop cycles through the 22 class + spec combinations from the table [iterate]
    while(class.rep.count <= counter){
      
    class = iterate[class.rep.count,1]
    spec = iterate[class.rep.count,2]
    print(paste("*** Class: ",class," ","Spec: ",spec, "***"))
    
    if(tryCatch({
      datalist=fromJSON(paste("https://www.warcraftlogs.com/v1/rankings/encounter/",encounter,"?api_key=",key,"&metric=",metric,"&difficulty=",difficulty,"&class=",class,"&spec=",spec,"&limit=",limit,"&page=",page,sep=""))
      datalist.talents=datalist$rankings%>%select(name,reportID,fightID,duration,startTime,talents)%>%unnest()
      datalist.gear=datalist$rankings%>%select(name,reportID,fightID,duration,startTime,gear)%>%unnest()
      datalist=datalist$rankings%>%select(name,class,spec,total,reportID,fightID,duration,itemLevel,startTime)
      length(datalist)
      }, error=function(e){0})==0){
      print("Spec Lookup Failed")
      class.rep.count=class.rep.count+1
      next
      }
    else{
      datalist$encounter=encounter
      datalist$difficulty=difficulty
      datalist.talents$encounter=encounter
      datalist.talents$difficulty=difficulty
      datalist.gear$encounter=encounter
      datalist.gear$difficulty=difficulty
        }
        while(page<=reps){ #this inner loop cycles through pages of rankeds results from warcraftlogs. Default value is 1000 [reps] pages of 300 [limit] results per spec. The large number is to ensure all levels are sampled.
          if (tryCatch({
            import=fromJSON(paste("https://www.warcraftlogs.com/v1/rankings/encounter/",encounter,"?api_key=",key,"&metric=",metric,"&difficulty=",difficulty,"&class=",class,"&spec=",spec,"&limit=",limit,"&page=",page,sep=""))
            import.talents=import$rankings%>%select(name,reportID,fightID,duration,startTime,talents)%>%unnest()
            import.gear=import$rankings%>%select(name,reportID,fightID,duration,startTime,gear)%>%unnest()
            import=import$rankings%>%select(name,class,spec,total,reportID,fightID,duration,itemLevel,startTime)
            length(datalist)
            }, error=function(e){0})==0)
          {
            print("Page Lookup Failed")
            break
          }
          else{
          print(page)
          import$encounter=encounter
          import$difficulty=difficulty
          import.talents$encounter=encounter
          import.talents$difficulty=difficulty
          import.gear$encounter=encounter
          import.gear$difficulty=difficulty
          datalist=rbind(datalist,import)
          datalist.talents=rbind(datalist.talents,import.talents)
          datalist.gear=rbind(datalist.gear,import.gear)
          page=page+1
          rm(import)
          rm(import.gear)
          rm(import.talents)
          }
        }
    merged = rbind(merged,datalist)
    merged.talents = rbind(merged.talents,datalist.talents)
    merged.gear=rbind(merged.gear,datalist.gear)
    page=1
    class.rep.count=class.rep.count+1
    rm(datalist)
    rm(datalist.gear)
    rm(datalist.talents)
    }
  page=1
  class.rep.count=1
  fightcounter=fightcounter+1
  }
page=1
class.rep.count=1
fightcounter=1
difficultycounter=difficultycounter+1
}
print("End")
