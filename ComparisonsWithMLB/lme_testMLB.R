library(Lahman)
library(vistime)
library(plyr)
library(MASS)
library(nlme)
library(igraph)
library(lme4)

source("CompFunctions.R")
hitters <- Batting
#hitters$LGYR <- as.factor(paste(hitters$lgID, hitters$yearID, sep = "."))
#hitters.lgyr <- ddply(hitters, .(playerID, LGYR), collapse.season)
#summary(hitters.lgyr)

#############
cutyr <- 2015
hit.2010 <- hitters[hitters$yearID >cutyr,]
Stint <- hit.2010$stint
hit.2010 <- comp.bat(hit.2010)
hit.2010$Stint <- Stint
hit.2010 <- hit.2010[hit.2010$PA >10, ]
head(hit.2010)
summary(hit.2010)
hit.2010$plyr.yr <- as.factor(paste(hit.2010$Plyr, hit.2010$YR, sep = "."))
hit.2010$lg.yr <- as.factor(paste(hit.2010$LG, hit.2010$YR, sep = "."))

############
MLB.lme1 <- lme(OPS ~ lg.yr + plyr.yr,
                random= ~ 1|Plyr, data=hit.2010,
                method="REML")
MLB.cs1 <- gls(OPS ~ lg.yr + plyr.yr, data= hit.2010, correlation = corCompSymm(form=~1|Plyr),
               method="REML")

############
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
