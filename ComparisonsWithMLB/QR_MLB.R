library(Lahman)
library(vistime)
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
cutyr <- 2000
hit.2010 <- hitters[hitters$yearID > cutyr,]
#hit.2010 <- hitters
Stint <- hit.2010$stint
Experience <- hit.2010$experience
hit.2010 <- comp.bat(hit.2010)
hit.2010$experience <- Experience
hit.2010$Stint <- Stint
hit.2010 <- hit.2010[hit.2010$PA >5, ]
head(hit.2010)
summary(hit.2010)
hit.2010$plyr.yr <- as.factor(paste(hit.2010$Plyr, hit.2010$YR, sep = "."))
hit.2010$lg.yr <- as.factor(paste(hit.2010$LG, hit.2010$YR, sep = "."))
ssns.2010 <- ddply(hit.2010, .(Plyr), season.tab)
summary(ssns.2010)
############
#For quadratic reqressions, we remove all players who have less than 3 seasons and fit the model
roster <- ssns.2010$Plyr[ssns.2010$n.ssns >2]
hit <- hit.2010[hit.2010$Plyr %in% roster, ]
head(hit)
summary(hit)
t0 <- Sys.time()
MLB.qr <-lm(OPS ~ Plyr + Plyr:experience + Plyr:I(experience^2) + lg.yr, data = hit, weights = PA)
tf <- Sys.time()
tf - t0
summary(MLB.qr)
anova(MLB.qr)
sum(is.na(coef(MLB.qr)))
coef(MLB.qr)[is.na(coef(MLB.qr))]
X <- model.matrix(MLB.qr)
##########
MLB.lme1 <- lme(OPS ~ lg.yr + plyr.yr,
                random= ~ 1|Plyr, data=hit.2010,
                method="REML")
MLB.cs1 <- gls(OPS ~ lg.yr + plyr.yr, data= hit.2010, correlation = corCompSymm(form=~1|Plyr),
               method="REML")

###########
#We may instead model player effects as a linear function of experience instead.
hit.2010$YRf <- as.factor(hit.2010$YR)
MLB.lm <- lm(OPS ~ Plyr:YRf + LG:YRf, data = hit.2010, weights = PA)
sum(is.na(coef(MLB.lm)))
#summary(MLB.lm)
tail(coef(MLB.lm), n=20)
length(coef(MLB.lm))
d.connections <- read.csv("MLB_2yr_intersections.csv", header = TRUE, sep = ",")
anova(MLB.lm)
y <- qr(model.matrix(MLB.lm))
y$rank
length(coef(MLB.lm))

MLB.lm1 <- lm(OPS ~ lg.yr + Plyr, data = hit.2010, weights = PA)
summary(MLB.lm1)

MLB.lme1 <- lmer(OPS ~ lg.yr + plyr.yr + (1|Plyr), data = hit.2010)
#Error message:  fixed-effect model matrix is rank deficient so dropping 7 columns / coefficients
summary(MLB.lme1)

###########
#This section focuses on ploting the differences in lg effects and talent over the years.
#First a rough plot of the league effect coefficients:
beta <- coef(MLB.qr)
lgeffs <- beta[grepl("lg.yr", names(beta))]
 #names(input)[grepl("S", names(input))]
d.comps <- data.frame(matrix(NA, nrow = 147*20, ncol = 11))
comp.names <- c("start.yr", "end.yr", "LG1", "LG2", "LG.ef", "LE.std", "LE.pval", "Tal.diff", "TD.std", "TD.pval", "comn.plyrs")
names(d.comps) <- comp.names
head(d.comps)
t0 <- Sys.time()
n <- 1
for(yr in 1872:2018){
  obs.yr <- hit[hit$YR >= yr-1 & hit$YR <= yr,]
  lgs <- as.character(unique(obs.yr$lg.yr))
  N <- length(lgs)
  #model1 <- lm(BA ~ playerID + LGYR, weights = PA, data = obs.yr)
  X <- model.matrix(MLB.qr)
  betahat <- coef(MLB.qr)
  for (i in 1:N){
    for (j in 1:N){
      if (i == j){
        next
      }
      d <- data.frame(matrix(NA, nrow = 1, ncol = 11))
      names(d) <- comp.names
      lg1 <- lgs[i]
      lg2 <- lgs[j]
      lg1r <- hit$Plyr[hit$lg.yr == lg1]
      lg2r <- hit$Plyr[hit$lg.yr == lg2]
      plyrs <- unique(obs.yr$playerID)
      ovrlp <-sum(plyrs %in% lg1r & plyrs %in% lg2r)
      d[1,"comn.plyrs"] <- ovrlp
      ind1<- ifelse(hit$lg.yr == lg1, 1,0)
      c1 <- ind1/sum(ind1)
      ind2<- ifelse(hit$lg.yr == lg2, 1,0)
      c2 <- ind2/sum(ind2)
      lgeffs <- rep(0,length(coef(MLB.qr)))
      r1 <- names(betahat) == paste("lg.yr", lg1, sep = "")
      r2 <- names(betahat) == paste("lg.yr", lg2, sep = "")
      #sum(r1)
      en <- rep(0, length(betahat))
      en[r1]<- -1
      en[r2] <- 1
      c <- unname(t(c1-c2)%*%X+t(en))
      d1 <- t.test.lm(MLB.qr,c)
      d2 <- conf_int.lm(MLB.qr, c, a = 0.05)
      d[1,"start.yr"] <- yr-1
      d[1,"end.yr"] <- yr
      d[1,3] <- lg1
      d[1,4] <- lg2
      d[1,"Tal.diff"] <- d2[1,"est"]
      d[1,"TD.pval"] <- d1[1, "pvalue"]
      d[1,"TD.std"] <- d2[1, "std.dev"]
      c.lg <- -t(en)
      d3 <- conf_int.lm(MLB.qr, c.lg, a = 0.05)
      d[1,5] <- d3[1,"est"]
      d[1,6] <- d3[1, "std.dev"]
      d4 <- t.test.lm(model1, c.lg)
      d[1,7] <- d4[1,"pvalue"]
      d.comps[n,] <- d
      n <- n+1
    }}
}
tf <- Sys.time()
tf-t0
summary(d.comps)

##################
#Since vectors for reading the contrasts are too large we extract the predictions and league coefficients.
hit1 <- hit[hit$Plyr != "zuninmi01",]
OPShat <- predict(MLB.qr, newdata = hit1)
beta <- coef(MLB.qr)
lgeffs <- beta[grepl("lg.yr", names(beta))]
head(hit)
head(OPShat)
lgs <- unique(hit1$LG)
lgeff0 <- lgeffs["lg.yrNL.2018"]
talent0 <- mean(OPShat[hit1$lg.yr == "NL.2018"])
d.comps <- data.frame(matrix(data = NA, nrow = 300, ncol = 4))
names(d.comps) <- c("YR", "LG", "tal.diff", "lg.diff")
n <- 1
for (yr in 1981:2018){
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
d.comps

library(ggplot2)
LEplot <- ggplot(d.comps, aes(x = YR, y = lg.diff, color = LG)) + 
  geom_line() 
  #geom_point(aes(shape = signLE, size = signLE))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference from NL 2018 OPS")+ scale_x_continuous(breaks=seq(2000, 2020, 2))
LEplot + scale_y_continuous(breaks=seq(-1.5, 0.2, 0.1))
HAplot <- ggplot(d.comps, aes(x = YR, y = tal.diff, color = LG)) + 
  geom_line() 
  #geom_point(aes(shape=signHA, size = signHA))+scale_shape_manual(values=c(20,8))+scale_size_manual(values=c(1,2))
HAplot <- HAplot+labs(title = "Average Hitting Ability", x = "Year", y = "Difference from NL 2018 OPS")
HAplot + scale_y_continuous(breaks = seq(-0.1,1.3,0.1))+scale_x_continuous(breaks=seq(2000, 2020, 2))
