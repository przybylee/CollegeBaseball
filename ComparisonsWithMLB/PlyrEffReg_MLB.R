library(Lahman)
#library(vistime)
library(plyr)
library(MASS)
library(nlme)
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
cutyr <- 1801
hit.2010 <- hitters[hitters$yearID > cutyr,]
#hit.2010 <- hitters
Stint <- hit.2010$stint
Experience <- hit.2010$experience
hit.2010 <- comp.bat(hit.2010)
hit.2010$experience <- Experience
hit.2010$Stint <- Stint
hit.2010 <- hit.2010[hit.2010$PA >10, ]
head(hit.2010)
summary(hit.2010)
hit.2010$plyr.yr <- as.factor(paste(hit.2010$Plyr, hit.2010$YR, sep = "."))
hit.2010$lg.yr <- as.factor(paste(hit.2010$LG, hit.2010$YR, sep = "."))
ssns.2010 <- ddply(hit.2010, .(Plyr), season.tab)
summary(ssns.2010)
############
#For quadratic reqressions, we remove all players who have less than 3 seasons and fit the model
roster <- ssns.2010$Plyr[ssns.2010$n.ssns >2]
hit <- hit.2010
head(hit)
summary(hit)
t0 <- Sys.time()
plyr.reg <-lm(OPS ~ Plyr + lg.yr, data = hit, weights = PAsc)
tf <- Sys.time()
tf - t0
summary(plyr.reg)
anova(plyr.reg)
sum(is.na(coef(plyr.reg)))
#X <- model.matrix(MLB.qr)
##########

###########
##################
#Since vectors for reading the contrasts are too large we extract the predictions and league coefficients.
#We equate everything to NL.1976 just like Cramer
hit1 <- hit
OPShat <- predict(plyr.reg, newdata = hit1)
beta <- coef(plyr.reg)
lgeffs <- beta[grepl("lg.yr", names(beta))]
head(hit)
head(OPShat)
lgs <- unique(hit1$LG)
lgeff0 <- lgeffs["lg.yrNL.1976"]
talent0 <- mean(OPShat[hit1$lg.yr == "NL.1976"])
d.comps <- data.frame(matrix(data = NA, nrow = 300, ncol = 4))
names(d.comps) <- c("YR", "LG", "tal.diff", "lg.diff")
n <- 1
for (yr in 1871:2018){
  hit.yr <- hit1[hit1$YR == yr,]
  lgs.yr <- unique(hit.yr$LG)
  for (lg in lgs.yr){
    lgyr <- paste(lg, yr, sep = ".")
    lgeff.ind <- paste("lg.yr",lgyr, sep="")
    lgeff <- lgeffs[lgeff.ind]
    talent <- mean(OPShat[hit1$lg.yr == lgyr])
    d.comps$YR[n] <- yr
    d.comps$LG[n] <- lg
    d.comps$tal.diff[n] <- (talent-talent0) - (lgeff - lgeff0) 
    d.comps$lg.diff[n] <- lgeff - lgeff0
    n <- n+1
  }
}
d.comps <- d.comps[!is.na(d.comps$tal.diff),]
summary(d.comps)

library(ggplot2)
LEplot <- ggplot(d.comps, aes(x = YR, y = lg.diff, color = LG)) + 
  geom_line() 
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 2018 OPS")+ scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(-0.2, 0.4, 0.05))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)
HAplot <- ggplot(d.comps, aes(x = YR, y = tal.diff, color = LG)) + 
  geom_line() 
#geom_point(aes(shape=signHA, size = signHA))+scale_shape_manual(values=c(20,8))+scale_size_manual(values=c(1,2))
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-0.4,0.2,0.05))+scale_x_continuous(breaks=seq(1870, 2020, 10))+
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

###
#Here we code an alternative for d.comps that includes hypothesis testing.  We test two hypotheses for lg effects and 
#hitting talent.  First, were these quantitnties equal for all leagues in a given season, and second, in a given league, 
#is there no significant difference in the quatity compared to the previous season.
#We plot everyting relative to NL.1976
#Comparing all seasons in Lahman takes about 8 hours
par.names <- names(coef(plyr.reg))
beta = as.matrix(coef(plyr.reg))
N <- length(par.names)
lg0 <- "NL.1976"
d.lg0 <- hit[hit$lg.yr == lg0,]
roster0 <- unique(d.lg0$Plyr)
peffs0 <- paste("Plyr", roster0, sep = "")
c.ros0 <- ifelse(par.names %in% peffs0, 1, 0)
c.ros0 <- c.ros0/sum(c.ros0)
c.lg0 <- ifelse(par.names ==  paste("lg.yr", lg0, sep = ""), 1, 0)
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
for (yr in 1999:2020){
  hit.yr <- hit[hit$YR == yr,]
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
    d.comps$hitting_talent[n] <- t(c.ros1-c.ros0)%*%beta 
    d.comps$hitting_lg_eff[n] <- t(c.lg1 - c.lg0)%*%beta
    C_LE[i_LG,] = t(c.lg1)
    C_tal[i_LG,]= t(c.ros1)
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
        #d.comps$le_ssn_diff[(d.comps$YR == yr) & (d.comps$LG == lg)] = "yes"
      }
      c.ssntal = c.ros1 - c.ros2
      dtal <- t.test.lm(plyr.reg, c.ssntal)
      if (dtal$pvalue[1] < 0.05){
        d.comps$tal_ssn_diff[n] = "yes"
        #d.comps$tal_ssn_diff[(d.comps$YR == yr) & (d.comps$LG == lg)] = "yes"
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
d.comps <- d.comps[order(d.comps$LG, d.comps$YR),]
N <- length(d.comps$YR)
for (j in 1:(N-1)){
    if (d.comps$LG[j] == d.comps$LG[j+1]){
      if (d.comps$le_ssn_diff[j+1] == "yes"){
        d.comps$le_ssn_diff[j] = "yes"
      }
      if (d.comps$tal_ssn_diff[j+1] == "yes"){
        d.comps$tal_ssn_diff[j] = "yes"
      }
    }
}
summary(d.comps)
#write.table(d.comps, "comps_PlyrEffReg_7-28-20.csv", sep = ",")
d.comps <- read.csv("comps_PlyrEffReg_7-28-20.csv", header = T, sep = ",")
d.comps$LG <- as.character(d.comps$LG)
d.comps$LG[is.na(d.comps$LG)]<- "NA"
d.comps$LG <- factor(d.comps$LG)
summary(d.comps)

library(ggplot2)
LEplot <- ggplot(d.comps, aes(x = YR, y = hitting_lg_eff, color = LG)) + 
  geom_line() 
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 1976 OPS")+ 
  scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(-0.2, 0.4, 0.05))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)
HAplot <- ggplot(d.comps, aes(x = YR, y = hitting_talent, color = LG)) + 
  geom_line() +
  geom_point(aes(shape=tal_ssn_diff, size = tal_ssn_diff))+scale_shape_manual(values=c(20,8))+scale_size_manual(values=c(1,2))
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-0.4,0.2,0.05))+scale_x_continuous(breaks=seq(1870, 2020, 10))+
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

###Here we write a little code to do a simple t test to compare any two league years in OPS effect and avg hitting ability.
lg1 <- "AL.1972"
lg2 <- "AL.1973"
par.names <- names(coef(plyr.reg))
beta = as.matrix(coef(plyr.reg))
N <- length(par.names)
d.lg1 <- hit[hit$lg.yr == lg1,]
roster1 <- unique(d.lg1$Plyr)
peffs1 <- paste("Plyr", roster1, sep = "")
c.ros1 <- ifelse(par.names %in% peffs1, 1, 0)
c.ros1 <- c.ros1/sum(c.ros1)
c.lg1 <- ifelse(par.names ==  paste("lg.yr", lg1, sep = ""), 1, 0)

d.lg2 <- hit[hit$lg.yr == lg2,]
roster2 <- unique(d.lg2$Plyr)
peffs2 <- paste("Plyr", roster2, sep = "")
c.ros2 <- ifelse(par.names %in% peffs2, 1, 0)
c.ros2 <- c.ros2/sum(c.ros2)
c.lg2 <- ifelse(par.names ==  paste("lg.yr", lg2, sep = ""), 1, 0)

c.LE <- c.lg2-c.lg1
c.tal <- c.ros2-c.ros1

dLE <- conf_int.lm(plyr.reg, c.LE, a=0.05)
LEdff <- dLE$est[1]
print(paste("The league effect on OPS for", lg2, "is", LEdff, "bigger than the league effect on OPS for", lg1, sep = " "))
print(dLE)

dtal <- conf_int.lm(plyr.reg, c.tal)
taldff <- dtal$est[1]
print(paste("The avg hitting talent in", lg2, "is", taldff, "bigger than the avg hitting talent in", lg1, sep = " "))
print(dtal)
