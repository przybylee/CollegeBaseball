#Lee Przybylski
#11/16/2020
#Explore the hitting data from the baseball cube, clean up to one data frame
#for analysis

library(plyr)
library(xtable)
source("ComparisonsWithMLB/CompFunctions.R")
options(stringsAsFactors = T)

Summer0 <- read.csv("ComparisonsWithDI/Przybylski_college_summer_batting.csv", header = TRUE, sep = ",")
ncaa0 <- read.csv("ComparisonsWithDI/przybylski_college_batting_recent.csv", header = TRUE, sep = ",")
summary(Summer)
summary(ncaa)
names(ncaa)
names(Summer)

ncaa_zeros <- ncaa[ncaa$playerid == 0,]
head(ncaa_zeros)
unique(ncaa_zeros$leagueName)
#Store league abreviations
NCAA_lgs <- data.frame(unique(ncaa0[c("leagueName", "LeagueAbbr")]))
#Remove useless observations and columns
names <- names(ncaa0)[c(1:5,7:25,30:45)]
ncaa <- ncaa0[ncaa0$playerid != 0, names]

#Check for data that does not make sense
table(ncaa$Age)
#We have over 200 players older than 23
#Collect the players info
ncaa0$id <- as.factor(ncaa0$playerid)
players_ncaa <- ddply(ncaa0, .(id), college.bios)

#Players indexed by playerid
players_spring <- ncaa[ncaa$playerid!= 0,c(1:3,5,7,8,30:36)]
players_summer <- Summer[,c(1:6,28:34)]
names(players_spring)<- names(players_summer)
players <- rbind(players_spring,players_summer)
#Make sure all players with the same nonzero id have the same first name
first.name <- function(playerid){
  d <- subset(players, playerid == playerid)
  getmode(d$firstname)
}
last.name <- function(playerid){
  d <- subset(players, playerid == playerid)
  getmode(d$lastname)
}
nzeropids <- unique(players$playerid[players$playerid != 0])
first.names <- sapply(nzeropids, first.name)
last.names <- sapply(nzeropids, last.name)
#Identify playerid with more than one first name or last name
countnames1 <- function(playerid){
  d <- players[players$playerid == playerid,]
  length(unique(d$firstname))
}
countnames2 <- function(playerid){
  d <- players[players$playerid == playerid,]
  length(unique(d$lastname))
}
namecount1 <- sapply(nzeropids, countnames1)
namecount2 <- sapply(nzeropids, countnames2)
d.id_names <- data.frame(cbind(nzeropids, namecount1, namecount2))
summary(d.id_names)
cleanSummer1 <- Summer
cleanSummer1$firstname <- as.character(cleanSummer1$firstname)
cleanSummer1$lastname <- as.character(cleanSummer1$lastname)
cleanncaa1 <- ncaa[ncaa$playerid != 0,]
cleanncaa1$firstName <- as.character(cleanncaa1$firstName)
cleanncaa1$lastName <- as.character(cleanncaa1$lastName)
dirtyids <- nzeropids[namecount1 !=1 | namecount2 != 1]
D <- players[players$playerid %in% dirtyids,]
for (id in dirtyids){
  D <- players[players$playerid == id,]
  name1 <- as.character(getmode(D$firstname))
  name2 <- as.character(getmode(D$lastname))
  cleanncaa1[cleanncaa1$playerid == id, "firstName"] <- name1
  cleanncaa1[cleanncaa1$playerid == id, "lastName"] <- name2
  cleanSummer1[cleanSummer1$playerid == id, "firstname"] <- name1
  cleanSummer1[cleanSummer1$playerid == id, "lastname"] <- name2
}
write.table(cleanncaa1, file = "ncaaH.2010-2020_clv1.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(cleanSummer1, file = "SummerH.2010-2019_clv1.csv", col.names = TRUE, row.names = F, sep = ",")

#are the id's unique?
ids <- c(players_spring$playerid,players_summer$playerid)
ids <- unique(ids)
ids <- ids[ids != 0]
firstnames <- rep(0, length(ids))
lastnames <- rep(0,length(ids))
n <- 1
for (id in ids){
  P <- players[players$playerid == id,]
  firstnames[n] <- length(unique(P$firstname))
  lastnames[n] <- length(unique(P$lastname))
  n <- n+1
}
#19 of the player ids have two different first names assigned, but each id only has one last name
vn <- firstnames== 2
P <- players[players$playerid %in% ids[vn],]
#Save the ids with more than one first name
twonames <- unique(players$playerid[players$playerid %in% ids[vn]])
#besides the ids on this list, we assume each nonzero id is assigned to one unique player

#look at playerids equal to 0
players_spring <- ncaa[,c(1,2,7,8)]
players_summer <- Summer[,c(1,2,5,6)]
names(players_spring)<- names(players_summer)
players <- rbind(players_spring,players_summer)
players <- players[players$playerid==0,]
players$namestr <- paste(players$lastname, players$firstname, sep = ".")
#Throw out players with no name
players <- players[players$namestr != ".",]
namestrs <- unique(players$namestr)
playerobs <- rep(0, length(namestrs))
yr.range <- rep(0,length(namestrs))
n<- 1
for (name in namestrs){
  P <- players[players$namestr == name,]
  playerobs[n] <- length(P$year)
  yr.range[n] <- max(P$year) - min(P$year)
  n <- n+1
}
multiobs <- playerobs >1
multi.names <- namestrs[multiobs]
D <- players[players$namestr%in% multi.names,]
ncaa$namestr <- paste(ncaa$lastName,ncaa$firstName, sep=".")
Summer$namestr <- paste(Summer$lastname, Summer$firstname, sep = ".")
#All the namestrs with multiple observations are in the Summer data
rep.namestr0 <- namestrs[yr.range == 0 & playerobs >1]
rep.namestr0.d <- Summer[Summer$namestr %in% rep.namestr0 & Summer$playerid == 0,]
#tag observations that have the same namestr and league
Pleagues <- rep(0, length(rep.namestr0))
n<-1
for (name in rep.namestr0){
  P <- rep.namestr0.d[rep.namestr0.d$namestr == name,]
  Pleagues[n] <- length(unique(P$LeagueName))
  n <- n+1
}
rep.namestr.mleague <- rep.namestr0[Pleagues >1]
rep.namestr0.d2 <- rep.namestr0.d[rep.namestr0.d$namestr%in% rep.namestr.mleague,]
#We have checked all instances of the same name appearing in one season and they match
rep.namestr1 <- namestrs[yr.range == 1]
rep.namestr1.d <- Summer[Summer$namestr %in% rep.namestr1 & Summer$playerid == 0,]
#tag observations that have the same namestr and league
Pleagues <- rep(0, length(rep.namestr1))
n<-1
for (name in rep.namestr1){
  P <- rep.namestr1.d[rep.namestr1.d$namestr == name,]
  Pleagues[n] <- length(unique(P$LeagueName))
  n <- n+1
}
summary(Pleagues)
rep.namestr.mleague <- rep.namestr1[Pleagues >1]
rep.namestr1.d2 <- rep.namestr1.d[rep.namestr1.d$namestr%in% rep.namestr.mleague,]
#Just check observations with more than a four season range
rep.namestr5 <- namestrs[yr.range > 5]
rep.namestr5.d <- Summer[Summer$namestr %in% rep.namestr5 & Summer$playerid == 0,]
#tag observations that have the same namestr and league
Pleagues <- rep(0, length(rep.namestr1))
n<-1
for (name in rep.namestr1){
  P <- rep.namestr1.d[rep.namestr1.d$namestr == name,]
  Pleagues[n] <- length(unique(P$LeagueName))
  n <- n+1
}
summary(Pleagues)
rep.namestr.mleague <- rep.namestr1[Pleagues >1]
rep.namestr1.d2 <- rep.namestr1.d[rep.namestr1.d$namestr%in% rep.namestr.mleague,]

summer_chk <- Summer[,c("lastname", "Slg","obp","OPS")]
ncaa_chk <- ncaa[,c("lastName", "Slg", "obp", "OPS")]
spring <- ncaa[,c(1:3,5,8:30,33:35,41:44)]
summer <- Summer[,c(1:28,43)]
names(spring)
names(summer)
#names(summer)[c(3:4, 7:27)]<- paste(names(summer)[c(3,4,7:27)], "Smr", sep = ".")
summary(spring)
summary(summer)

#Just pick one season
yr = 2014
spring.yr <- spring[spring$year == yr,]
spring.yr <- spring.yr[spring.yr$AB >19,]
spring.yr <- spring.yr[spring.yr$playerid!=0,]
names(spring.yr)[4] <- "league"
summer.yr <- summer[summer$year == yr,]
summer.yr <- summer[summer.yr$AB >19,]
names(summer.yr)[4] <- "league"
springtosummer <- spring.yr$playerid %in%summer.yr$playerid
sum(springtosummer)
summertospring <- summer14$playerid %in% spring14$playerid
sum(summertospring)
Lgs <- unique(summer.yr$LeagueName)
MLgs <- Lgs[c(1,5:6, 8:11,13,16)]
sumlgs <- summer.yr[summer.yr$LeagueName %in% MLgs,]
sumlgs$LeagueName <- droplevels(sumlgs$LeagueName)
boxplot(OPS ~ LeagueName, data = sumlgs)
#Put all hitters together
table(summer$LeagueName[summertospring])


