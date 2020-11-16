#Lee Przybylski
#11/13/2020

library(Lahman)
#library(vistime)
library(plyr)
library(MASS)
library(nlme)
library(igraph)
library(lme4)
library(lmerTest)

source("CompFunctions.R")
hitters <- Batting
hitters$YR <- hitters$yearID
hitters$birth.yr <- get.birthyr(hitters$playerID)

#############
#cutyr <- 1870
minPA <- 10
#hit <- hitters[hitters$yearID > cutyr,]
#hit <- hitters[hitters$yearID >1898 & hitters$yearID <1915,]
#Stint <- hit.2010$stint
#Experience <- hit.2010$experience
hit <- comp.bat(hitters)
head(hit)
summary(hit)
hit <- hit[hit$PA >minPA, ]
hit$PAsc <- hit$PA/max(hit$PA)
summary(hit)
#hit.2010$plyr.yr <- as.factor(paste(hit.2010$Plyr, hit.2010$YR, sep = "."))
hit$lg.yr <- as.factor(paste(hit$LG, hit$YR, sep = "."))
hit$YRf <- as.factor(hit$YR)
hit <- hit[!is.na(hit$birth.yr + hit$OPS),]
players <- ddply(hit, .(Plyr), player.bios)
summary(players)