#Here we try to recreate the dick cramer results for comparing talent accross consecutive years of major league baseball
library(Lahman)
library(vistime)
library(plyr)
library(MASS)
data()

#college <- CollegePlaying
hitters <- Batting
battingLabels
summary(hitters)
SLG <- (hitters$H-hitters$X2B-hitters$X3B-hitters$HR+2*hitters$X2B+3*hitters$X3B+4*hitters$HR)/hitters$AB
OBP <- (hitters$H+hitters$BB+hitters$HBP)/(hitters$AB+hitters$BB+hitters$HBP+hitters$SF)
OPS <- SLG+OBP
hitters$OBP <-OBP
hitters$SLG <- SLG
hitters$OPS <- OPS
Leagues <- as.character(unique(hitters$lgID))
Start <- rep(NA, length(Leagues))
End <- rep(NA, length(Leagues))
for (l in 1:length(Leagues)){
  d <- hitters[hitters$lgID == Leagues[l],]
  Start[l] <- min(d$yearID)
  End[l] <- max(d$yearID)
}
Start
End
d.timeline <- data.frame(cbind(Leagues, Start, End))
summary(d.timeline)
mode(Leagues)
mode(d.timeline$Leagues)
d.timeline$Leagues <- Leagues
d.timeline$start <- paste(Start, "01-01", sep ="-")
d.timeline$end <- paste(End, "12-01", sep ="-")
d.timeline$event <- Leagues
View(d.timeline)
#d.timeline$Leagues <- as.factor(d.timeline$Leagues)
vistime(d.timeline, title = "Timeline of the Major Leagues")

collapse.season <- function(d){
  G <- sum(d$G); AB <- AB <- sum(d$AB); R <- sum(d$R)
  H<- sum(d$H); X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <- sum (d$HR) ; RBI <- sum(d$RBI); SB <- sum(d$SB, na.rm = T)
  CS <- sum (d$CS, na.rm = T); BB <- sum(d$BB, na.rm = T) ; SH <- sum(d$SH, na.rm = T)
  SF <- sum(d$SF, na.rm = T); HBP <- sum(d$HBP, na.rm = T)
  SLG <- (H-X2B-X3B-HR+2*X2B+3*X3B+4*HR)/AB
  OBP <- (H+BB+HBP)/(AB+BB+HBP+SF)
  OPS <- SLG+OBP; YR <- d$yearID[1]
  TM <- d$teamID[which.max(d$AB)]
  LG <- d$lgID[which.max(d$AB)]
  data.frame(YR = YR, LG = LG, TM = TM,G =G, AB = AB, R = R, H=H, X2B= X2B, X3B = X3B,
             HR= HR, RBI = RBI, SB=SB, CS=CS, BB=BB, HBP = HBP,
             SH=SH, SF=SF, SLG=SLG, OBP=OBP, OPS=OPS)
}
#confidence interval for lm
conf_int.lm=function(lmout,c,d=0,a){
  b=coef(lmout)
  var.b <- vcov(lmout)
  X=model.matrix(lmout)
  df= nrow(X) - length(b)
  cb.d=c%*%b-d
  X.X <- t(X)%*%X
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(c%*%var.b%*%t(c))
  lower = cb.d - tquant%*%sqrt(c%*%var.b%*%t(c))
  upper = cb.d + tquant%*%sqrt(c%*%var.b%*%t(c))
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}

#Test function
d <- hitters[hitters$playerID== "puellce01" & hitters$yearID == 2017,]
collapse.season(d)[1,]
#Pick a year and league
yr = 1876
lg = "AL"
min.pa <- 100
#NL runs 1876-2018.  Make a dataframe
#AL runs 1901-2018
#NL.d <- data.frame(matrix(data = NA, nrow = 2017-1876, ncol = 5))
names(NL.d) <- c("Start.YR", "est", "std.dev", "lower", "upper")
AL.d <- data.frame(matrix(data = NA, nrow = 2017-1901, ncol = 5))
names(AL.d) <- c("Start.YR", "est", "std.dev", "lower", "upper") 
m = 1
for(yr in 1901:2017){
  d1 <- hitters[yr == hitters$yearID & hitters$lgID == lg, 1:22]
  d2 <- hitters[yr+1 == hitters$yearID & hitters$lgID == lg, 1:22 ]
  h1 <- ddply(d1, .(playerID), collapse.season)
  h2 <- ddply(d2, .(playerID), collapse.season)
  h1 <- h1[h1$AB >= min.pa ,c(1:5,18:21)]
  h2 <- h2[h2$AB >= min.pa ,c(1:5,18:21)]
  h <- rbind(h1, h2)
  h$YR <- as.factor(h$YR)
  model1 <- lm(OPS ~ playerID + YR, data= h)
  #summary(model1)
  #anova(model1)
  #yhat <- predict(model1)
  #yhat1 <- yhat[h$YR == yr]
  #yhat2 <- yhat[h$YR == yr+1]
  #lambda <- unlist(coef(model1)[length(coef(model1))])
  #This is the test statistic for the difference between seasons
  #mean(yhat1)-mean(yhat2)+lambda
  X <- model.matrix(model1)
  r <- ncol(X)
  c1 <- ifelse(h$YR == yr, 1,0)
  c1 <- c1/sum(c1)
  c2 <- ifelse(h$YR != yr, 1,0)
  c2 <- c2/sum(c2)
  en <- rep(0,length(coef(model1)))
  en[r]<- 1
  c <- -t(c1-c2)%*%X-t(en)
  #Alternative way to get the test stat:
  c%*%coef(model1)
  #Do a t.test
  #t.test.lm(model1,c)
  d <- conf_int.lm(model1, c, a = 0.05)
  AL.d[m,]<- cbind(yr,d)
  m = m+1
  print(paste(yr, "done"))
}
summary(AL.d)
#Use the estimated steps to compute everything in terms of difference from 2018 ops
n <- length(NL.d$Start.YR)
NL.d$diff.2018 <- NA
for(j in 1:n){
  NL.d$diff.2018[j] <- -sum(NL.d$est[j:n])
}
plot(NL.d$Start.YR, NL.d$diff.2018, xlab = "year", ylab = "Change in OPS", main = "Average Talent in the NL")
abline(v= 1944)
abline(v = 1962)
abline(v = 1969)
abline(v = 1993)
#Do the same for the AL
n <- length(AL.d$Start.YR)
AL.d$diff.2018 <- NA
for(j in 1:n){
  AL.d$diff.2018[j] <- -sum(AL.d$est[j:n])
}
plot(AL.d$Start.YR, AL.d$diff.2018, xlab = "year", ylab = "Change in OPS from 2018", main = "Average Talent in the AL")
abline(v= 1944)
abline(v = 1961)
abline(v = 1969)
abline(v = 1977)
abline(v = 1973)

#Look at residuals based on the number of at bats for the model above
yrs <- c(1920,1940, 1960, 1980,2000)
yrs.1 <- yrs+1
d1 <- hitters[hitters$yearID %in% yrs, 1:22]
d2 <- hitters[hitters$yearID %in% yrs.1, 1:22 ]
h1 <- ddply(d1, .(playerID), collapse.season)
h2 <- ddply(d2, .(playerID), collapse.season)
h1 <- h1[h1$AB >4,c(1:6, 15:21)]
h2 <- h2[h2$AB >4,c(1:6,15:21)]
repeaters <- h1$playerID %in% h2$playerID
sum(repeaters)
h1 <- h1[repeaters, ]
h2 <- h2[h2$playerID %in% h1$playerID,]
h <- rbind(h1, h2)
h$PA <- h$AB +h$BB + h$HBP+h$SF
summary(h)
h$playerID <- as.factor(h$playerID)
h$YR <- as.factor(h$YR)
model1 <- lm(OPS ~ playerID + YR, data= h)
summary(model1)
anova(model1)
h$residual <- resid(model1)
summary(h)
plot(h$PA, h$sq.error)
t <- 1: 1200
lines(x = t, y = 1/t, col = "red")
h$inv.PA <- 1/abs(h$PA)
h$sq.error <- h$residual^2
res.reg <-lm( sq.error~ inv.PA, data = h) 
ind <-order(res.reg$fitted.values)
anova(res.reg)
summary(res.reg)
lines(x =h$PA[ind] , y= res.reg$fitted.values[ind], col = "red")
