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
############
#Fit models with random player effects
############
comptimes <- rep(NA, 10)
t0 <- Sys.time()
lm0 <-lm(OPS ~ LG + YR, data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
comptimes[1] <- (tf - t0)

t0 <- Sys.time()
lm1 <-lm(OPS ~ LG + YRf, data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
comptimes[2] <- (tf - t0)

t0 <- Sys.time()
lm2 <-lm(OPS ~ lg.yr, data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
comptimes[3] <- (tf - t0)

t0 <- Sys.time()
mixed1 <-lmer(OPS ~ lg.yr + (1|Plyr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)
comptimes[4] <- (tf - t0)
#summary(mixed1)
#anova(mixed1)
#fixef(mixed1)
df <- data.frame(cbind(hit$OPS, predict(mixed1), residuals(mixed1)))
head(df)

#Add an effect for the player's birth year
t0 <- Sys.time()
mixed2 <-lmer(OPS ~ lg.yr + birth.yr + (1|Plyr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)
comptimes[5] <- tf - t0
#fixef(mixed2)
#fixef(mixed1)
#summary(mixed2)
#anova(mixed1, mixed2)

#Treat birth.yr as a factor
hit$birth.yrf <- as.factor(hit$birth.yr)
#head(hit)
t0 <- Sys.time()
mixed3 <-lmer(OPS ~ lg.yr + birth.yrf + (1|Plyr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)
comptimes[6] <- tf - t0
fixef(mixed2)
fixef(mixed3)


#anova(mixed2,mixed3)
hit$tm.yr <- as.factor(paste(hit$TM,hit$YR, sep = "."))
head(hit)
t0 <- Sys.time()
mixed4 <-lmer(OPS ~ lg.yr + birth.yrf + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)
comptimes[7] <- tf - t0

#Try fitting without weights
t0 <- Sys.time()
mixed5 <-lmer(OPS ~ lg.yr + birth.yrf + (1|Plyr), data = hit, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

t0 <- Sys.time()
mixed6 <-lmer(OPS ~ LG + birth.yrf + Age + Age2 + (1|Plyr) + (1|tm.yr), 
              data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

#Rank Deficient - no Age 
#This is not the same as mixed5 since birth.yr is not a factor
#birth.yrf is a significant improvement over birth.yr
t0 <- Sys.time()
mixed7 <-lmer(OPS ~ lg.yr + birth.yr + Age + Age2 + (1|Plyr) + (1|tm.yr), 
              data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

#Rank Deficient - no Age
hit$Agef <- as.factor(hit$Age)
t0 <- Sys.time()
mixed8 <-lmer(OPS ~ lg.yr + birth.yrf + Agef + (1|Plyr) + (1|tm.yr), 
              data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

anova(lm0)
anova(lm1)
anova(lm2)
anova(lm3)
anova(mixed1)

anova(mixed1, lm2)
anova(mixed2)
anova(mixed3)
anova(mixed2,mixed3)
anova(mixed4)
anova(mixed3,mixed4)
summary(mixed4)

length(fixef(mixed4))
length(coefficients(mixed4))

#Some Residiual anlaysis
model <- mixed5
residuals <- residuals(model)
plot(hit$YR, residuals, xlab = "YR")
abline(h = 0, col = "red")
#plot residuls against plate appearances
plot(hit$PA, residuals, xlab = "PA")
abline(h = 0, col = "red")

#Plot fixed effects model by yr
lgeffects <- fixef(model)[grepl("lg.yr", names(fixef(model)))]
head(lgeffects)
ssns <- as.numeric(substrRight(names(lgeffects), 4))
head(ssns)
plot(ssns, lgeffects, xlab = "season", ylab = "lg.yr effect")
abline(h = 0)
abline(v = 1973, col = "blue", lty = "dashed")

#Plot fixed birth yr effects 
birth.effects <- fixef(model)[grepl("birth.yrf", names(fixef(model)))]
head(birth.effects)
byrs <- as.numeric(substrRight(names(birth.effects), 4))
plot(byrs, birth.effects, xlab = "birth year", ylab = "birth.yr effect")
abline(h = 0)
