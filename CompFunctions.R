library(plyr)

#These are all of the functions used by the several scripts used for comparing leagues
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

#This is designed to work with the Lahman batting data
comp.bat <- function(d){
  PA <- rowSums(d[,c("AB", "BB", "HBP", "SF")], na.rm = TRUE)
  OBP <- rowSums(d[,c("H", "BB", "HBP")], na.rm = TRUE)/PA
  TB <- d$H + d$X2B + 2*d$X3B + 3*d$HR
  BA <- ifelse(d$AB >0, d$H/d$AB, 0)
  SLG <- ifelse(d$AB >0, TB/d$AB, 0)
  OPS <- SLG + OBP 
  data.frame(Plyr = d$playerID, YR = d$yearID, LG = d$lgID, TM = d$teamID, G = d$G,PA = PA, AB = d$AB, H=d$H, 
             X2B= d$X2B, X3B = d$X3B, HR= d$HR, BB=d$BB, HBP = d$HBP,SB=d$SB, CS=d$CS,
             SH=d$SH, SF=d$SF, BA = BA, SLG=SLG, OBP=OBP, OPS=OPS)
}

#Find the number of seasons and number of observations for each player in a data frame.  
#meant for use with a roster of player ids and 
season.tab <- function(d){
  n.obs <- length(d$YR)
  n.ssns <- length(unique(d$YR))
  r.ssn <- min(d$YR)
  data.frame(n.obs = n.obs, n.ssns = n.ssns, r.ssn = r.ssn)
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
#t test for linear model lm
t.test.lm=function(lmout,c,d=0){
  b=coef(lmout)
  var.b <- vcov(lmout)
  X=model.matrix(lmout)
  df= nrow(X) - length(b)
  cb.d=c%*%b-d
  X.X <- t(X)%*%X
  tstat=drop(cb.d)/sqrt(c%*%var.b%*%t(c))
  pvalue=2*(1-pt(abs(tstat),df))
  data.frame(tstat=tstat,pvalue=pvalue)
}
