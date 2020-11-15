#Lee Przybylski
#11/2/2020
#Fit a variety of weighted linear models and assess the fit
library(lmerTest)
#############
#Run the first part of PlyrEffReg_MLB.R to get hitters dataframe

#The full model takes too long to fit, so we run tests using certain 15 year spans
#Span 1: 1899-1918 The start of the AL
#Span 2: 1899 - 1972
hit <- hitters[hitters$yearID >1898 & hitters$yearID <1973,]
#Stint <- hit.2010$stint
#Experience <- hit.2010$experience
hit <- comp.bat(hit)
head(hit)
summary(hit)
hit <- hit[hit$PA >minPA, ]
hit$birth.yr <- get.birthyr(hit$Plyr)
hit$PAsc <- hit$PA/max(hit$PA)
hit$YRf <- as.factor(hit$YR)
summary(hit)
hit$lg.yr <- as.factor(paste(hit$LG, hit$YR, sep = "."))
ssns <- ddply(hit, .(Plyr), season.tab)
summary(ssns)
############
#For quadratic reqressions, we remove all players who have less than 3 seasons and fit the model
#roster <- ssns.2010$Plyr[ssns.2010$n.ssns >2]
head(hit)
summary(hit)
#hit <- na.omit(hit)

t0 <- Sys.time()
mod.YR <-lm(OPS ~  0 + LG*YR + Plyr, data = hit, weights = PAsc)
tf <- Sys.time()
tf - t0
summary(mod.YR)
anova(mod.YR)
beta <- coef(mod.YR)
head(beta)
tail(beta)

#Treat YR as a factor
t0 <- Sys.time()
mod.YRf <-lm(OPS ~ 0 + lg.yr + Plyr , data = hit, weights = PAsc)
#unweighted residuals
mod.YRf <-lm(OPS ~ 0 + lg.yr + Plyr , data = hit)

tf <- Sys.time()
tf - t0
summary(mod.YRf)
anova(mod.YR, mod.YRf)
beta <- coef(mod.YRf)
head(beta)
tail(beta, n = 20)

#Check the residual plots
res <- residuals(mod.YRf)
summary(lm(res^2 ~ 1/hit$PAsc^2))
plot(1/hit$PAsc, res^2)
plot(hit$PA, res, xlab = "Plate Appearances", ylab = "Residual")
plot(hit$YR, res, xlab = "Year", ylab = "Residual")
plot(hit$Age, res, xlab = "Age", ylab = "Residual")
plot(hit$experience, res, xlab = "Experience", ylab = "Residual")
abline(h = 0, col = "red")
plot(hit$PA, res)

######
t0 <- Sys.time()
plyr.age1 <-lm(OPS ~ 0 + LG*YR + Plyr*Age, data = hit, weights = PAsc)
tf <- Sys.time()
tf - t0
anova(plyr.age1)

hit$Age2 <- hit$Age^2
t0 <- Sys.time()
plyr.age2 <-lm(OPS ~ 0 + LG*YR + Plyr + Plyr*Age + Plyr*Age2, data = hit, weights = PAsc)
tf <- Sys.time()
tf - t0
anova(plyr.age2)
summary(plyr.age1)
beta <- coef(plyr.age2)
ageslope <- beta[grepl("Age", names(beta))]
lgeffs <- beta[grepl("lg.yr", names(beta))]

anova(plyr.age1)
anova(plyr.age2)
anova(plyr.age3)

########################
##Random Player Effects:
summary(hit)
t0 <- Sys.time()
mixed1 <-lmer(OPS ~ lg.yr + (1|Plyr), data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
summary(mixed1)
fixef(mixed1)
summary(hit)

#Add an effect for the player's birth year
t0 <- Sys.time()
mixed2 <-lmer(OPS ~ 0 + lg.yr + birth.yr + (1|Plyr), data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
fixef(mixed2)
fixef(mixed1)
summary(mixed2)


hit$birth.yrf <- as.factor(hit$birth.yr)
head(hit)
t0 <- Sys.time()
mixed3 <-lmer(OPS ~ 0 + lg.yr + birth.yrf + (1|Plyr), data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)
fixef(mixed2)
fixef(mixed3)

anova(mixed1,mixed3)
#There is not a significant effect of birth year