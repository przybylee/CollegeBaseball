#Lee Przybylski
#11/20/2020
#Code for fitting mixed models on college datausing max likelihood.
#At the end, there is code for obtaining some residual plots, as well as 
#scatter plots representing the estimates of fixed intercepts in the models.

library(Lahman)
#library(vistime)
library(plyr)
library(MASS)
library(nlme)
library(igraph)
library(lme4)
library(lmerTest)

source("ComparisonsWithMLB/CompFunctions.R")
hitters <- read.csv("college_Batting_2008-2020.csv", header = TRUE, sep = ",")
hitters$YR <- hitters$yearID
bios <- read.csv("college_hitter_bios.csv", header = TRUE, sep = ",")
head(hitters)
summary(hitters)

#############
cutyr <- 2018
minPA <- 10
#hitters <- hitters[hitters$yearID > cutyr,]
#hit <- hitters[hitters$yearID >1898 & hitters$yearID <1915,]
#Stint <- hit.2010$stint
#Experience <- hit.2010$experience
hit <- comp.bat(hitters)
head(hit)
summary(hit)
hit <- hit[hit$PA >minPA, ]
hit$PAsc <- hit$PA/max(hit$PA)
summary(hit)
hit$tm.yr <- as.factor(paste(hit$TM, hit$YR, sep = "."))
hit$lg.yr <- as.factor(paste(hit$LG, hit$YR, sep = "."))
hit$YRf <- as.factor(hit$YR)
hit$r.lg <- NA
hit$r.tm <- NA
hit$r.ssn <- NA
j <- 1
for (id in bios$id){
  indicator <- hit$Plyr == id
  lg <- bios$r.lg[j]
  tm <- bios$r.tm[j]
  #ssn <- bios$r.ssn[j]
  hit$r.lg[indicator] <- as.character(lg)
  hit$r.tm[indicator] <- as.character(tm)
  #hit$r.ssn[indicator] <- ssn
  print(j)
  j <- j+1
}
#This took way too long, so we store the results
write.table(hit, "college_hit.csv", col.names = TRUE, row.names = FALSE, sep = ",")
hit$r.lg <- as.factor(hit$r.lg)

t0 <- Sys.time()
mixed1 <-lmer(OPS ~ lg.yr + (1|Plyr), data = hit, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

t0 <- Sys.time()
mixed2 <-lmer(OPS ~ lg.yr + r.lg + (1|Plyr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

t0 <- Sys.time()
mixed3 <-lmer(OPS ~ lg.yr + r.lg  + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

hit$r.ssn.sc <- hit$r.ssn - 2014
t0 <- Sys.time()
mixed4 <-lmer(OPS ~ lg.yr + r.lg  + r.ssn.sc + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

t0 <- Sys.time()
mixed5 <-lmer(OPS ~ lg.yr + r.lg*r.ssn.sc + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

hit$r.ssn.f <- as.factor(hit$r.ssn)
t0 <- Sys.time()
mixed5 <-lmer(OPS ~ lg.yr + r.lg*r.ssn.f + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

anova(mixed1,mixed2)

###We check residual plots:
#Some Residiual anlaysis
model <- mixed1
residuals <- residuals(model)
plot(hit$YR, residuals, xlab = "YR")
abline(h = 0, col = "red")
#plot residuls against plate appearances
plot(hit$PA, residuals, xlab = "PA")
abline(h = 0, col = "red")
#Plot resiudulas against fitted values
opshat <- predict(model)
plot(opshat, residuals)

#Plot fixed effects model by yr
lgeffects <- fixef(model)[grepl("lg.yr", names(fixef(model)))]
head(lgeffects)
ssns <- as.numeric(substrRight(names(lgeffects), 4))
head(ssns)
plot(ssns, lgeffects, xlab = "season", ylab = "lg.yr effect")
abline(h = 0)

#Plot fixed birth yr effects 
birth.effects <- fixef(model)[grepl("birth.yrf", names(fixef(model)))]
head(birth.effects)
byrs <- as.numeric(substrRight(names(birth.effects), 4))
plot(byrs, birth.effects, xlab = "birth year", ylab = "birth.yr effect")
abline(h = 0)

#Residual analysis for weights
usable <- residuals >0.000001
useable_pa <- hit$PA[usable]
usable_res <- residuals[usable]
abs_res <- abs(usable_res)
plot(log(useable_pa), log(abs_res^2))
lm_res <- lm(log(abs_res^2)~ log(useable_pa))
abline(lm_res, col = "red")
summary(lm_res)
#Plot sqaured residuals vs PA
plot(hit$PA, residuals^2)
inv.PA <- 1/hit$PA
lm_res <- lm(residuals^2~ 0+inv.PA)
grid <- 1:800
inv.grid <- 1/grid
reshat <- coef(lm_res)[1]*inv.grid
lines(grid, reshat, col = "red")
lm_res2 <- lm(residuals^2 ~ 0 + sqrt(inv.PA))
inv.grid2 <- 1/sqrt(grid)
reshat2 <- coef(lm_res2)[1]*inv.grid2
lines(grid, reshat2, col = "green")
#Compare sqared residuals to Plate appearances
residuals <- residuals(model)
sq_res <- residuals^2
sum(sq_res == 0)
lm(log(sq_res) ~ log(hit$PA))
