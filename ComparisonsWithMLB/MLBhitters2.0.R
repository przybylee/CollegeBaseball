#Here we try to recreate the dick cramer results for comparing talent accross 
#consecutive years of major league baseball
library(Lahman)
library(vistime)
library(plyr)
library(MASS)
library(igraph)
#data()

#college <- CollegePlaying
hitters <- Batting
#battingLabels
summary(hitters)
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
  d$PA <- d$AB + d$BB
  G <- sum(d$G); AB <- sum(d$AB); R <- sum(d$R)
  H<- sum(d$H); X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <- sum (d$HR) ; RBI <- sum(d$RBI); SB <- sum(d$SB, na.rm = T)
  CS <- sum (d$CS, na.rm = T); BB <- sum(d$BB, na.rm = T) ; SH <- sum(d$SH, na.rm = T)
  SF <- sum(d$SF, na.rm = T); HBP <- sum(d$HBP, na.rm = T)
  SLG <- (H-X2B-X3B-HR+2*X2B+3*X3B+4*HR)/AB
  PA <- AB+BB+HBP+SF
  OBP <- (H+BB+HBP)/PA
  OPS <- SLG+OBP; YR <- d$yearID[1]
  TM <- d$teamID[which.max(d$PA)]
  LG <- d$lgID[which.max(d$PA)]
  data.frame(YR= YR, LG = LG, TM = TM, G =G,PA = PA, AB = AB, R = R, H=H, X2B= X2B, X3B = X3B,
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
#d <- hitters[hitters$playerID== "puellce01" & hitters$yearID == 2017,]
#collapse.season(d)[1,]

#Plan:  
#1: Scan each 2 year period to determine the connectivity of the leagues
#2: Compare all leagues to AL.2018.  Do changes in average player and league effects
#3: Plot timeline for all legues
 
#Pick a year 
yr = 2018
min.pa <- 20
d.g <- hitters[hitters$yearID >= yr-1 & hitters$yearID <= yr, 1:22]
g <- ddply(d.g,.(playerID, lgID, yearID), collapse.season)
summary(g)
g <- g[g$PA >= min.pa, c(1:7,21:23)]
g$LGYR <- as.factor(paste(g$lgID, g$yearID, sep = "."))
#summary(g)
model1<- lm(OPS ~ playerID + LGYR, data = g)
X <- model.matrix(model1)
anova(model1)
summary(model1)
#identify leagues
leagues <- as.character(sort(unique(g$LGYR)))
#identify players in each league
N <- length(leagues)
rosters <- vector("list", N)
names(rosters) <- leagues
for (l in 1:length(leagues)){
  d <- g[g$LGYR == leagues[l],]
  rosters[[l]] <- unique(d$playerID)
}
#Form an adjacency matrix between the leagues.  Two leagues are adjacent if they share a player
A <- matrix(0, nrow = N, ncol = N)
row.names(A) <- leagues
colnames(A) <- leagues
for (l in leagues){
  for (k in leagues){
    if (l != k){
      if (sum(hitters$playerID %in% rosters[[l]] & hitters$playerID %in% rosters[[k]])>0){
        A[l,k] <- 1
      }
    }
  }
}
#To acutally see the players in common
test <- hitters[hitters$playerID %in% rosters[["AL.2017"]] & hitters$playerID %in% rosters[["NL.2017"]],]
lg1 <- leagues[2]
lg2 <- leagues[3]
ind1<- ifelse(g$LGYR == lg1, 1,0)
c1 <- ind1/sum(ind1)
ind2<- ifelse(g$LGYR == lg2, 1,0)
c2 <- ind2/sum(ind2)
betahat <- coef(model1)
lgeffs <- rep(0,length(coef(model1)))
r1 <- names(betahat) == paste("LGYR", lg1, sep = "")
r2 <- names(betahat) == paste("LGYR", lg2, sep = "")
sum(r2)
en <- rep(0, length(betahat))
en[r1]<- -1
en[r2] <- 1
c <- t(c2-c1)%*%X-t(en)
#Alternative way to get the test stat:
c%*%coef(model1)
#Do a t.test
#t.test.lm(model1,c)
d <- conf_int.lm(model1, c, a = 0.05)
#Check calculation:
#g1 <- g[g$LGYR == lg1,]
#g2 <- g[g$LGYR == lg2,]
#g$predict <- predict(model1,g)
#g1$predict <- predict(model1, g1)
#g2$predict <- predict(model1,g2)
#y1 <- mean(g1$predict)
#y2 <- mean(g2$predict)
#y2-y1-betahat["LGYRAL.2018"]
#t(c2-c1)%*%X%*%betahat


#NL runs 1876-2018.  Make a dataframe
#AL runs 1901-2018
NL.d <- data.frame(matrix(data = NA, nrow = 2017-1876, ncol = 5))
names(NL.d) <- c("Start.YR", "est", "std.dev", "lower", "upper")
AL.d <- data.frame(matrix(data = NA, nrow = 2017-1901, ncol = 5))
names(AL.d) <- c("Start.YR", "est", "std.dev", "lower", "upper") 


m = 1
for(yr in 1901:2017){
  d1 <- hitters[yr == hitters$yearID, 1:22]
  d2 <- hitters[yr+1 == hitters$yearID, 1:22 ]
  h1 <- ddply(d1, .(playerID), collapse.season)
  h2 <- ddply(d2, .(playerID), collapse.season)
  h1 <- h1[,c(1:7,20:22)]
  h2 <- h2[,c(1:7,20:22)]
  h <- rbind(h1, h2)
  h$YR <- as.factor(h$YR)
  model1 <- lm(OPS ~ playerID + YR:LG, data= h)
  repeaters <- (h$playerID %in% h1$playerID) & (h$playerID %in% h2$playerID)
  g <- h[repeaters,]
  h <- h[h$PA >= min.pa,]
  summary(model1)
  anova(model1)
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
h$inv.PA <- 1/abs(h$rPA)
h$sq.error <- h$residual^2
res.reg <-lm( sq.error~ inv.PA, data = h) 
anova(res.reg)
summary(res.reg)
