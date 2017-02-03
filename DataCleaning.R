library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(jsonlite)

################################################################################################################################################
classes = list(id=1:12,name=c("Death Knight","Druid","Hunter","Mage","Monk","Paladin","Priest","Rogue","Shaman","Warlock","Warrior","Demon Hunter"))
dk=list(id = 1:3, name = c("Blood", "Frost", "Unholy"))
druid=list(id = 1:4, name = c("Balance", "Feral", "Guardian", "Restoration"))
hunter=list(id = 1:3, name = c("Beast Mastery", "Marksmanship", "Survival"))
mage=list(id = 1:3, name = c("Arcane", "Fire", "Frost"))
monk=list(id = 1:3, name = c("Brewmaster", "Mistweaver", "Windwalker"))
paladin=list(id = 1:3, name = c("Holy", "Protection", "Retribution"))
priest=list(id = 1:3, name = c("Discipline", "Holy", "Shadow"))
rogue=list(id = 1:4, name = c("Assassination", "Combat", "Subtlety", "Outlaw"))
shaman=list(id = 1:3, name = c("Elemental", "Enhancement", "Restoration"))
warlock=list(id = 1:3, name = c("Affliction", "Demonology", "Destruction"))
warrior=list(id = 1:4, name = c("Arms", "Fury", "Protection", "Gladiator"))
demonh=list(id = 1:2, name = c("Havoc", "Vengeance"))
difficulty.names = list(id=c(4,5),Name=c("Heroic","Mythic"))

specs = list(id=c("1 2","1 3", "2 1", "2 2", "3 1", "3 2","3 3", "4 1", "4 2", "4 3", "5 3", "6 3", "7 3", "8 1", "8 3","8 4", "9 1", "9 2", "10 1", "10 2", "10 3", "11 1", "11 2","12 1"), name = c("Frost-Dk", "Unholy", "Balance", "Feral","Beast Mastery","Marksmanship","Survival","Arcane","Fire","Frost","Windwalker","Retribution","Shadow","Assassination","Subtlety","Outlaw","Elemental","Enhancement","Affliction","Demonology","Destruction","Arms","Fury","Havoc"))
fightsnames = list(id=c(1849,1865,1867,1871,1862,1863,1842,1886,1872,1866),EncounterName=c("Skorpyron","Chronomatic Anomaly","Trilliax","Spellblade Aluriel","Tichondrius","Star Augur Etraeus","Krosus","High Botanist Tel'arn","Grand Magistrix Elisande","Gul'dan"))
slots=list(id=c(1:18),Slot=c("Helm","Neck","Shoulders","Tabard","Chest","Belt","Leggings","Boots","Wrists","Glove","Ring1","Ring2","Trinket1","Trinket2","Cloak","Weapon1","Weapon2","EmptySlot"))

################################################################################################################################################

names(merged.talents)[6]="talentName"

merged.talents$talentRow=seq(from=1,to=7,by=1)
merged.talents=unique(merged.talents)

merged.talents=merged.talents%>%filter(name!="Anonymous")%>%select(name,reportID,fightID,duration,startTime,encounter,difficulty,talentName,talentRow)
merged.talents=merged.talents%>%filter(name!="Anonymous")%>%group_by(name,reportID,fightID,duration,startTime,encounter,difficulty,talentRow)%>%mutate(Count=n())
test.heroic=merged.talents%>%filter(Count==1,difficulty==4)
test.mythic=merged.talents%>%filter(Count==1,difficulty==5)
test.heroic=test.heroic%>%spread(talentRow,talentName)
test.mythic=test.mythic%>%spread(talentRow,talentName)
rm(merged.talents)

names(test.heroic)[9]="talent1"
names(test.heroic)[10]="talent2"
names(test.heroic)[11]="talent3"
names(test.heroic)[12]="talent4"
names(test.heroic)[13]="talent5"
names(test.heroic)[14]="talent6"
names(test.heroic)[15]="talent7"

names(test.mythic)[9]="talent1"
names(test.mythic)[10]="talent2"
names(test.mythic)[11]="talent3"
names(test.mythic)[12]="talent4"
names(test.mythic)[13]="talent5"
names(test.mythic)[14]="talent6"
names(test.mythic)[15]="talent7"

talents=rbind(test.heroic,test.mythic)

################################################################################################################################################


names(merged.gear)[6]="gearName"
merged.gear.legendary = merged.gear%>%select(name,reportID,fightID,duration,startTime,gearName,quality,encounter,difficulty)%>%filter(quality=="legendary")
merged.gear.legendary = unique(merged.gear.legendary)
merged.gear.legendary=merged.gear.legendary%>%filter(name!="Anonymous")%>%select(name,reportID,fightID,duration,encounter,difficulty,gearName)%>%group_by(name,reportID,encounter,difficulty)%>%mutate(legCount = seq_len(n()))%>%ungroup()%>%group_by(name)%>%mutate(maxLegCount=max(legCount))
merged.gear.legendary=merged.gear.legendary%>%filter(maxLegCount<3)
merged.gear.legendary.spread=merged.gear.legendary%>%spread(legCount,gearName)
names(merged.gear.legendary.spread)[8]="legendary1"
names(merged.gear.legendary.spread)[9]="legendary2"
rm(merged.gear)
rm(merged.gear.legendary)

################################################################################################################################################

parses=merged
parses=parses%>%left_join(talents)%>%left_join(.,merged.gear.legendary.spread)

parses$Date=as.Date(as.POSIXct(parses$startTime/1000, origin="1970-01-01"))
parses=parses%>%mutate(spec2=paste(as.character(class),as.character(spec)))
parses$class=factor(parses$class,levels=classes$id,labels=classes$name)
parses$Fight=factor(parses$encounter,levels=fightsnames$id,labels=fightsnames$EncounterName)
parses$spec2 = factor(parses$spec2,levels=specs$id,labels=specs$name)

parses=parses%>%filter(Date>"2016-10-24",spec2!="NA",name!="Anonymous")

################################################################################################################################################

parses$difficulty=factor(parses$difficulty,levels=difficulty.names$id,labels = difficulty.names$Name)
parses$maxLegCount[is.na(parses$maxLegCount)]=0
parses$legendary1[is.na(parses$legendary1)]="none"
parses$legendary2[is.na(parses$legendary2)]="none"
parses=parses%>%select(-talentName)
parses=parses%>%na.omit()
parses=parses%>%select(-encounter,-startTime,-Count)
write.csv(parses,"parses.csv")

