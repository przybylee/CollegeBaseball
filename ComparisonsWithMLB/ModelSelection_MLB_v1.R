library(Lahman)
library(vistime)
library(plyr)
library(MASS)
library(lmerTest)
library(igraph)
library(lme4)

source("CompFunctions.R")
hitters <- Batting
hitters$YR <- hitters$yearID
#hitters$LGYR <- as.factor(paste(hitters$lgID, hitters$yearID, sep = "."))
#hitters.lgyr <- ddply(hitters, .(playerID, LGYR), collapse.season)
#summary(hitters.lgyr)
#Count each season for each hitter
ssns.all <- ddply(hitters, .(playerID), season.tab)
head(ssns.all)
summary(ssns.all)
hitters$experience <- 0
N <- length(hitters$experience)
for (n in 1:N){
  plyr <- hitters$playerID[n]
  r.ssn <- ssns.all[plyr == ssns.all$playerID, "r.ssn"]
  hitters$experience[n] <- hitters$YR[n] - r.ssn + 1
}

#############
minpa <- 20
#If desired, give range for years to look at:
lowyr <- 1940
highyr <- 1980
#Cramer's Range:
hit <- hitters[hitters$yearID >= 1876 & hitters$yearID <= 1979,]
#hit <- hitters[hitters$yearID >= lowyr & hitters$yearID <= highyr,]
#hit <- hitters
Stint <- hit$stint
Experience <- hit$experience
hit <- comp.bat(hit)
hit$experience <- Experience
hit$stint <- Stint
hit <- hit[hit$PA > minpa, ]
hit$PAsc <- hit$PA/max(hit$PA)
hit$Class <- as.factor(hit$YR +1 - hit$experience)
head(hit)
summary(hit)
hit$plyr.yr <- as.factor(paste(hit$Plyr, hit$YR, sep = "."))
hit$lg.yr <- as.factor(paste(hit$LG, hit$YR, sep = "."))
hit$tm.yr <- as.factor(paste(hit$TM, hit$YR, sep = "."))
ssns <- ddply(hit, .(Plyr), season.tab)
summary(ssns)
############
head(hit)
summary(hit)
t0 <- Sys.time()
mod1 <-plyr.reg <-lm(OPS ~ Plyr + lg.yr, data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
t0 <- Sys.time()
mod2 <-lmer(OPS ~ Plyr + lg.yr +(1|plyr.yr)+(1|tm.yr), REML = F, data = hit, weights = PAsc, verbose = 1)
tf <- Sys.time()
print(tf - t0)
#Took almost 20 hours to fit on 8/20
t0 <- Sys.time()
mod2r <-lmer(OPS ~ Plyr + lg.yr +(1|tm.yr), REML = F, data = hit, weights = PAsc, verbose = 1)
tf <- Sys.time()
print(tf - t0)
t0 <- Sys.time()
mxmod3 <-lmer(OPS ~ lg.yr + Class + (1|Plyr) +(1|plyr.yr)+(1|tm.yr), REML = F , data = hit, weights = PAsc, verbose = 1)
tf <- Sys.time()
print(tf - t0)
########Compare mod1, mod2 and mod21
anova(mod2,mod2r,mod1)

#################
#We use lmerTest to compare models 1 and 2 since they have the same mean structure.
model = mxmod2
Pblups <- ranef(model)$Plyr
Lblups <- ranef(model)$lg.yr
lg0 <- "NL.1976"
d.lg0 <- hit[hit$lg.yr == lg0,]
roster0 <- as.character(unique(d.lg0$Plyr))
LE0 <- Lblups[lg0,1]
PEffsLg0 <- Pblups[roster0,1]
Tal0 <- mean(PEffsLg0)
d.comps <- data.frame(matrix(data = NA, nrow = 300, ncol = 4))
names(d.comps) <- c("YR", "LG", "hitting_talent", "hitting_lg_eff")
head(d.comps)
d.comps$YR = 0
n = 1
for (yr in 1871:2018){
  hit.yr <- hit[hit$YR == yr,]
  lgs.yr <- unique(hit.yr$LG)
  n_L <- length(lgs.yr)
  for (lg in lgs.yr){
    lg1 <- paste(lg, yr, sep = ".")
    d.lg1 <- hit[hit$lg.yr == lg1,]
    roster1 <- as.character(unique(d.lg1$Plyr))
    LE1 <- Lblups[lg1,1]
    PEffsLg1 <- Pblups[roster1,1]
    Tal1 <- mean(PEffsLg1)
    d.comps$YR[n] <- yr
    d.comps$LG[n] <- lg
    d.comps$hitting_talent[n] <- Tal1-Tal0 
    d.comps$hitting_lg_eff[n] <- LE1 - LE0
    n <- n+1
  }
  print(paste(yr, "is done", sep = " "))
}
d.comps = d.comps[d.comps$YR != 0,]
summary(d.comps)
write.table(d.comps, "comps_MxEff2_MLB_7-29-2020.csv", sep = ",")

library(ggplot2)
LEplot <- ggplot(d.comps, aes(x = YR, y = hitting_lg_eff, color = LG)) + 
  geom_line(linetype = "solid", size = 0.8) 
  #geom_point(aes(shape = le_lg_diff, size = le_lg_diff))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
#LEplot <- LEplot+geom_line(data = d.comps, aes(x = YR, replace(d.comps$hitting_lg_eff, d.comps$le_ssn_diff == "no", NA)), size = 1)
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 2018 OPS")+ scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(-0.1, 0.15, 0.02))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)
HAplot <- ggplot(d.comps, aes(x = YR, y = hitting_talent, color = LG)) + 
  geom_line(linetype = "solid", size = 0.8) 
  #geom_point(aes(shape = tal_lg_diff, size = tal_lg_diff))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
#HAplot <- HAplot+geom_line(data = d.comps, aes(x = YR, replace(d.comps$hitting_talent, d.comps$tal_ssn_diff == "no", NA)), size = 1)
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-0.05,0.1,0.03))+scale_x_continuous(breaks=seq(1870, 2020, 10))+
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


#################
#Plot the results
#We plot everyting relative to NL.1976
#This is for mxmod3.  Since League effects are fixed now, we can do hypothesis testing on the league effects.
model = mxmod3
beta = fixef(model)
par.names <- names(fixef(model))
N = length(par.names)
lg0 <- "NL.1976"
efflg0 = paste("lg.yr", lg0, sep = "")
d.lg0 <- hit[hit$lg.yr == lg0,]
roster0 <- as.character(unique(d.lg0$Plyr))
c.lg0 <- ifelse(par.names ==  paste("lg.yr", lg0, sep = ""), 1, 0)
Pblups <- ranef(model)$Plyr 
PEffsLg0 <- Pblups[roster0,1]
Tal0 <- mean(PEffsLg0)
d.comps <- data.frame(matrix(data = NA, nrow = 300, ncol = 4))
names(d.comps) <- c("YR", "LG", "hitting_talent", "hitting_lg_eff")
head(d.comps)
d.comps$YR = 0
n = 1
for (yr in 1871:2018){
  hit.yr <- hit[hit$YR == yr,]
  lgs.yr <- unique(hit.yr$LG)
  n_L <- length(lgs.yr)
  for (lg in lgs.yr){
    lg1 <- paste(lg, yr, sep = ".")
    d.lg1 <- hit[hit$lg.yr == lg1,]
    efflg1 = paste("lg.yr", lg1, sep = "")
    roster1 <- as.character(unique(d.lg1$Plyr))
    c.lg1 <- ifelse(par.names ==  paste("lg.yr", lg1, sep = ""), 1, 0)
    PEffsLg1 <- Pblups[roster1,1]
    Tal1 <- mean(PEffsLg1)
    d.comps$YR[n] <- yr
    d.comps$LG[n] <- lg
    d.comps$hitting_talent[n] <- Tal1-Tal0 
    d.comps$hitting_lg_eff[n] <- t(c.lg1-c.lg0)%*%as.matrix(beta)
    n <- n+1
  }
  print(paste(yr, "is done", sep = " "))
}
d.comps = d.comps[d.comps$YR != 0,]
summary(d.comps)
write.table(d.comps, "comps_MxEff3_MLB_7-29-2020.csv", sep = ",")

###
d.comps <- data.frame(matrix(data = NA, nrow = 300, ncol = 8))
names(d.comps) <- c("YR", "LG", "hitting_talent", "tal_ssn_diff", "tal_lg_diff", "hitting_lg_eff", "le_ssn_diff", "le_lg_diff")
head(d.comps)
d.comps$YR = 0
d.comps$tal_ssn_diff = "no"
d.comps$le_ssn_diff = "no"
d.comps$tal_lg_diff = "no"
d.comps$le_lg_diff = "no"
n <- 1
lgs.prev <- c("PCL")
t0 = Sys.time()
for (yr in 1871:2018){
  hit.yr <- hit1[hit1$YR == yr,]
  lgs.yr <- unique(hit.yr$LG)
  n_L <- length(lgs.yr)
  C_LE <- matrix(data = 0, nrow = n_L, ncol = N)
  C_tal <- matrix(data = 0, nrow = n_L, ncol = N)
  i_LG = 1
  for (lg in lgs.yr){
    lg1 <- paste(lg, yr, sep = ".")
    d.lg1 <- hit[hit$lg.yr == lg1,]
    roster1 <- unique(d.lg1$Plyr)
    peffs1 <- paste("Plyr", roster1, sep = "")
    c.ros1 <- ifelse(par.names %in% peffs1, 1, 0)
    c.ros1 <- c.ros1/sum(c.ros1)
    c.lg1 <- ifelse(par.names ==  paste("lg.yr", lg1, sep = ""), 1, 0)
    d.comps$YR[n] <- yr
    d.comps$LG[n] <- lg
    d.comps$hitting_talent[n] <- t(c.ros1-c.ros0)%*%beta - t(c.lg1 - c.lg0)%*%beta 
    d.comps$hitting_lg_eff[n] <- t(c.lg1 - c.lg0)%*%beta
    C_LE[i_LG,] = t(c.lg1)
    C_tal[i_LG,]= t(c.ros1 - c.lg1)
    if (lg %in% lgs.prev){
      lg2 <- paste(lg, yr-1, sep = ".")
      d.lg2 <- hit[hit$lg.yr == lg2,]
      roster2 <- unique(d.lg2$Plyr)
      peffs2 <- paste("Plyr", roster2, sep = "")
      c.ros2 <- ifelse(par.names %in% peffs2, 1, 0)
      c.ros2 <- c.ros2/sum(c.ros2)
      c.lg2 <- ifelse(par.names ==  paste("lg.yr", lg2, sep = ""), 1, 0)
      c.ssnLE <- c.lg1-c.lg2
      dLE <- t.test.lm(plyr.reg, c.ssnLE)
      if (dLE$pvalue[1] < 0.05){
        d.comps$le_ssn_diff[n] = "yes"
        d.comps$le_ssn_diff[(d.comps$YR == yr) & (d.comps$LG == lg)] = "yes"
      }
      c.ssntal = c.ros1 - c.ros2 - (c.lg1 - c.lg2)
      dtal <- t.test.lm(plyr.reg, c.ssntal)
      if (dtal$pvalue[1] < 0.05){
        d.comps$tal_ssn_diff[n] = "yes"
        d.comps$tal_ssn_diff[(d.comps$YR == yr) & (d.comps$LG == lg)] = "yes"
      }
    }
    n <- n+1
    i_LG = i_LG + 1
  }
  lgs.prev <- lgs.yr
  #Test for lg differences within a season
  if (n_L <2){
    next
  }
  M = cbind(matrix(data = 1, nrow = n_L-1, ncol = 1), -diag(n_L-1)) 
  C_LE = M%*% C_LE
  dLE <- F.test.lm(plyr.reg, C_LE)
  if (dLE$pvalue[1] < 0.05){
    d.comps$le_lg_diff[d.comps$YR == yr] = "yes"
  }
  C_tal = M%*%C_tal
  dtal <- F.test.lm(plyr.reg, C_tal)
  if (dtal$pvalue[1] < 0.05){
    d.comps$tal_lg_diff[d.comps$YR == yr] = "yes"
  }
  print(paste(yr, "done", sep = " "))
}
tf = Sys.time()
print(tf - t0)
d.comps <- d.comps[!(d.comps$YR == 0),]

#Plot the results from d.comps

LEplot <- ggplot(d.comps, aes(x = YR, y = hitting_lg_eff, color = LG)) + 
  geom_line(linetype = "solid", size = 0.8) 
#geom_point(aes(shape = le_lg_diff, size = le_lg_diff))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
#LEplot <- LEplot+geom_line(data = d.comps, aes(x = YR, replace(d.comps$hitting_lg_eff, d.comps$le_ssn_diff == "no", NA)), size = 1)
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 2018 OPS")+ scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(-0.1, 0.15, 0.02))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)
HAplot <- ggplot(d.comps, aes(x = YR, y = hitting_talent, color = LG)) + 
  geom_line(linetype = "solid", size = 0.8) 
#geom_point(aes(shape = tal_lg_diff, size = tal_lg_diff))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
#HAplot <- HAplot+geom_line(data = d.comps, aes(x = YR, replace(d.comps$hitting_talent, d.comps$tal_ssn_diff == "no", NA)), size = 1)
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-0.04,0.08,0.02))+scale_x_continuous(breaks=seq(1870, 2020, 10))+
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
