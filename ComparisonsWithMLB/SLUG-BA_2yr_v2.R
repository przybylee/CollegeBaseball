library(Lahman)
library(vistime)
library(plyr)
library(MASS)
library(igraph)

source("CompFunctions.R")

d.connections <- read.csv("MLB_2yr_intersections.csv", header = TRUE, sep = ",")
hitters <- Batting
hitters$LGYR <- as.factor(paste(hitters$lgID, hitters$yearID, sep = "."))
hitters.lgyr <- ddply(hitters, .(playerID, LGYR), collapse.season)
summary(hitters.lgyr)
min.pa <- 20
obs <- hitters.lgyr[hitters.lgyr$PA >= 20,]
obs$playerID <- as.factor(obs$playerID)
summary(obs)
#data frame for storing all of the comparisons
d.comps <- data.frame(matrix(NA, nrow = 147*20, ncol = 11))
comp.names <- c("start.yr", "end.yr", "LG1", "LG2", "LG.ef", "LE.std", "LE.pval", "Tal.diff", "TD.std", "TD.pval", "comn.plyrs")
names(d.comps) <- comp.names

t0 <- Sys.time()
n <- 1
for(yr in 1872:2018){
  obs.yr <- obs[obs$YR >= yr-1 & obs$YR <= yr,c(1:8, 21:23)]
  lgs <- as.character(unique(obs.yr$LGYR))
  N <- length(lgs)
  model1 <- lm(OPS ~ playerID + LGYR, weights = PA, data = obs.yr)
  X <- model.matrix(model1)
  betahat <- coef(model1)
  anova(model1)
  for (i in 1:N){
   for (j in 1:N){
    if (i == j){
      next
    }
    d <- data.frame(matrix(NA, nrow = 1, ncol = 11))
    names(d) <- comp.names
    lg1 <- lgs[i]
    lg2 <- lgs[j]
    lg1r <- obs.yr$playerID[obs.yr$LGYR == lg1]
    lg2r <- obs.yr$playerID[obs.yr$LGYR == lg2]
    plyrs <- unique(obs.yr$playerID)
    ovrlp <-sum(plyrs %in% lg1r & plyrs %in% lg2r)
    d[1,"comn.plyrs"] <- ovrlp
    ind1<- ifelse(obs.yr$LGYR == lg1, 1,0)
    c1 <- ind1/sum(ind1)
    ind2<- ifelse(obs.yr$LGYR == lg2, 1,0)
    c2 <- ind2/sum(ind2)
    lgeffs <- rep(0,length(coef(model1)))
    r1 <- names(betahat) == paste("LGYR", lg1, sep = "")
    r2 <- names(betahat) == paste("LGYR", lg2, sep = "")
    #sum(r1)
    en <- rep(0, length(betahat))
    en[r1]<- -1
    en[r2] <- 1
    c <- t(c1-c2)%*%X+t(en)
    d1 <- t.test.lm(model1,c)
    d2 <- conf_int.lm(model1, c, a = 0.05)
    d[1,"start.yr"] <- yr-1
    d[1,"end.yr"] <- yr
    d[1,3] <- lg1
    d[1,4] <- lg2
    d[1,"Tal.diff"] <- d2[1,"est"]
    d[1,"TD.pval"] <- d1[1, "pvalue"]
    d[1,"TD.std"] <- d2[1, "std.dev"]
    c.lg <- -t(en)
    d3 <- conf_int.lm(model1, c.lg, a = 0.05)
    d[1,5] <- d3[1,"est"]
    d[1,6] <- d3[1, "std.dev"]
    d4 <- t.test.lm(model1, c.lg)
    d[1,7] <- d4[1,"pvalue"]
    d.comps[n,] <- d
    n <- n+1
   }}
  print(paste("Year", yr, "done"))
}
tf <- Sys.time()
tf-t0
summary(d.comps)
d.comps <- d.comps[!is.na(d.comps$start.yr),]

#From d.comps, we need to extract leagues and yrs and compare consecutive years of each league to 
summary(obs)
d.NLcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.NLcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
#compare all NL seasons to NL 2017
Lg <- "NL"
yrs <- as.numeric(2017:1876)
lg.diff <- 0
tal.diff <- 0
for (yr in yrs){
  #yr <- 2017
  lg1 <- paste(Lg, yr+1, sep = ".")
  lg2 <- paste(Lg, yr, sep = ".")
  d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
  lg.diff <- lg.diff - d[1,"LG.ef"]
  tal.diff <- tal.diff - d[1,"Tal.diff"]
  p1 <- d[1,"LE.pval"]
  p2 <- d[1,"TD.pval"]
  d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
  names(d.new) <- names(d.NLcomps)
  d.NLcomps <- rbind(d.NLcomps, d.new)
}
X= d.yrcomps$YR
plot(d.NLcomps$YR, d.NLcomps$rel.LE)
plot(d.NLcomps$YR, d.NLcomps$rel.HA, type = "p")
summary(d.NLcomps)

#compare all NA seasons to NL 2017
d.NAcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.NAcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "NA"
max(obs$YR[obs$LG == "NA"])
min(obs$YR[obs$LG == "NA"])
yrs <- as.numeric(1874:1871)
lg.diff <- d.NLcomps[d.NLcomps$YR == 1876, "rel.LE"]
tal.diff <- d.NLcomps[d.NLcomps$YR == 1876, "rel.HA"]
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 1875
lg1 <- paste("NL", yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.NLcomps)
d.NAcomps <- rbind(d.NAcomps, d.new)

for (yr in yrs){
  #yr <- 2017
  lg1 <- paste(Lg, yr+1, sep = ".")
  lg2 <- paste(Lg, yr, sep = ".")
  d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
  lg.diff <- lg.diff - d[1,"LG.ef"]
  tal.diff <- tal.diff - d[1,"Tal.diff"]
  p1 <- d[1,"LE.pval"]
  p2 <- d[1,"TD.pval"]
  d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
  names(d.new) <- names(d.NLcomps)
  d.NAcomps <- rbind(d.NAcomps, d.new)
}
plot(d.NAcomps$YR, d.NAcomps$rel.LE)
plot(d.NAcomps$YR, d.NAcomps$rel.HA, type = "p")

#compare all AL seasons to NL 2018
d.ALcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.ALcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "AL"
max(obs$YR[obs$LG == "AL"])
min(obs$YR[obs$LG == "AL"])
yrs <- as.numeric(2017:1901)
lg.diff <- 0
tal.diff <- 0
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 2018
lg1 <- paste("NL", yr, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.ALcomps)
d.ALcomps <- rbind(d.ALcomps, d.new)

for (yr in yrs){
  #yr <- 2017
  lg1 <- paste(Lg, yr+1, sep = ".")
  lg2 <- paste(Lg, yr, sep = ".")
  d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
  lg.diff <- lg.diff - d[1,"LG.ef"]
  tal.diff <- tal.diff - d[1,"Tal.diff"]
  p1 <- d[1,"LE.pval"]
  p2 <- d[1,"TD.pval"]
  d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
  names(d.new) <- names(d.ALcomps)
  d.ALcomps <- rbind(d.ALcomps, d.new)
}
plot(d.ALcomps$YR, d.ALcomps$rel.LE)
plot(d.ALcomps$YR, d.ALcomps$rel.HA, type = "p")

#compare all NA seasons to NL 2018
d.AAcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.AAcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "AA"
max(obs$YR[obs$LG == "AA"])
min(obs$YR[obs$LG == "AA"])
yrs <- as.numeric(1890:1882)
lg.diff <- d.NLcomps[d.NLcomps$YR == 1892, "rel.LE"]
tal.diff <- d.NLcomps[d.NLcomps$YR == 1892, "rel.HA"]
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 1891
lg1 <- paste("NL", yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.AAcomps)
d.AAcomps <- rbind(d.AAcomps, d.new)

for (yr in yrs){
  #yr <- 2017
  lg1 <- paste(Lg, yr+1, sep = ".")
  lg2 <- paste(Lg, yr, sep = ".")
  d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
  lg.diff <- lg.diff - d[1,"LG.ef"]
  tal.diff <- tal.diff - d[1,"Tal.diff"]
  p1 <- d[1,"LE.pval"]
  p2 <- d[1,"TD.pval"]
  d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
  names(d.new) <- names(d.NLcomps)
  d.AAcomps <- rbind(d.AAcomps, d.new)
}
plot(d.AAcomps$YR, d.AAcomps$rel.LE)
plot(d.AAcomps$YR, d.AAcomps$rel.HA, type = "p")

#compare the two FL seasons to NL 2018
d.FLcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.FLcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "FL"
max(obs$YR[obs$LG == "FL"])
min(obs$YR[obs$LG == "FL"])
#yrs <- as.numeric(1874:1871)
lg.diff <- d.NLcomps[d.NLcomps$YR == 1916, "rel.LE"]
tal.diff <- d.NLcomps[d.NLcomps$YR == 1916, "rel.HA"]
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 1915
lg1 <- paste("NL", yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.FLcomps)
d.FLcomps <- rbind(d.FLcomps, d.new)
#Get the first season of FL
yr <- 1914
lg1 <- paste(Lg, yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.FLcomps)
d.FLcomps <- rbind(d.FLcomps, d.new)
d.FLcomps

#Compare season of UA to NL 2018
d.UAcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.UAcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "UA"
max(obs$YR[obs$LG == "UA"])
min(obs$YR[obs$LG == "UA"])
#yrs <- as.numeric(1874:1871)
lg.diff <- d.NLcomps[d.NLcomps$YR == 1885, "rel.LE"]
tal.diff <- d.NLcomps[d.NLcomps$YR == 1885, "rel.HA"]
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 1884
lg1 <- paste("NL", yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.UAcomps)
d.UAcomps <- rbind(d.UAcomps, d.new)

#Compare season of PL to NL 2018
d.PLcomps <- data.frame(matrix(0, nrow = 0, ncol = 6))
names(d.PLcomps) <- c("LG", "YR", "rel.LE", "LE.pval", "rel.HA", "HA.pval")
Lg <- "PL"
max(obs$YR[obs$LG == "PL"])
min(obs$YR[obs$LG == "PL"])
lg.diff <- d.NLcomps[d.NLcomps$YR == 1891, "rel.LE"]
tal.diff <- d.NLcomps[d.NLcomps$YR == 1891, "rel.HA"]
#Get a comparison for the last season by comparing to the relevant NL season
yr <- 1890
lg1 <- paste("NL", yr+1, sep = ".")
lg2 <- paste(Lg, yr, sep = ".")
d <- d.comps[d.comps$LG1 == lg1 & d.comps$LG2 == lg2,]
lg.diff <- lg.diff - d[1,"LG.ef"]
tal.diff <- tal.diff - d[1,"Tal.diff"]
p1 <- d[1,"LE.pval"]
p2 <- d[1,"TD.pval"]
d.new <- data.frame(LG = Lg, YR = yr, rel.LE = lg.diff, LE.pval = p1, rel.HA = tal.diff, HA.pval = p2)
names(d.new) <- names(d.PLcomps)
d.PLcomps <- rbind(d.PLcomps, d.new)

#Make a data frame to store all of the comparisons to NL.2018
ssn2NL.2018 <- rbind(d.NLcomps, d.ALcomps, d.NAcomps, d.AAcomps, d.PLcomps, d.FLcomps, d.UAcomps)
summary(ssn2NL.2018)
ssn2NL.2018$signHA <- as.factor(ifelse(ssn2NL.2018$HA.pval < 0.05, "yes","no"))
ssn2NL.2018$signLE <- as.factor(ifelse(ssn2NL.2018$LE.pval < 0.05, "yes","no"))

library(ggplot2)
LEplot <- ggplot(ssn2NL.2018, aes(x = YR, y = rel.LE, color = LG)) + 
  geom_line() +
  geom_point(aes(shape = signLE, size = signLE))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 2018 OPS")+ scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(0, 1.2, 0.1))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)
HAplot <- ggplot(ssn2NL.2018, aes(x = YR, y = rel.HA, color = LG)) + 
  geom_line() +
  geom_point(aes(shape=signHA, size = signHA))+scale_shape_manual(values=c(20,8))+scale_size_manual(values=c(1,2))
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-1.3,0.2,0.1))+scale_x_continuous(breaks=seq(1870, 2020, 10))+
  geom_vline(xintercept = 1961, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1977, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1998, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1892, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1900, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1962, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1977, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1998, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1973, linetype="dotted", color = "blue", size=1)
  