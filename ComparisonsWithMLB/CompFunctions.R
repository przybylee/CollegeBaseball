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

#Get the age correspoding to a player ID in the Lahman database
get.age <- function(plyrid, YR){
  df <- People[People$playerID %in% plyrid,]
  row.names(df) <- df$playerID
  j <- 1
  birthyr <- rep(0, length(plyrid))
  for (id in plyrid){
    birthyr[j] <- df[id, "birthYear"]
    j <- j +1
  }
  age <- YR - birthyr
  return(age)
}

#Get the birthyr for each player in the lahman data
get.birthyr <- function(plyrid){
  df <- People[People$playerID %in% plyrid,]
  row.names(df) <- df$playerID
  j <- 1
  birthyr <- rep(0, length(plyrid))
  for (id in plyrid){
    birthyr[j] <- df[id, "birthYear"]
    j <- j +1
  }
  return(birthyr)
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
             SH=d$SH, SF=d$SF, BA = BA, SLG=SLG, OBP=OBP, OPS=OPS)#, birth.yr = d$birth.yr)
}

#Find the number of seasons and number of observations for each player in a data frame.  
#meant for use with a roster of player ids and 
season.tab <- function(d){
  n.obs <- length(d$YR)
  n.ssns <- length(unique(d$YR))
  r.ssn <- min(d$YR)
  data.frame(n.obs = n.obs, n.ssns = n.ssns, r.ssn = r.ssn)
}

player.bios <- function(d){
  n.obs <- length(d$YR)
  n.ssns <- length(unique(d$YR))
  r.ssn <- min(d$YR)
  f.ssn <- max(d$YR)
  r.lg <- d$LG[d$YR == r.ssn]
  birth.yr <- d$birth.yr[1]
  data.frame(n.obs = n.obs, n.ssns = n.ssns, r.ssn = r.ssn,
             r.lg = r.lg, birth.yr = birth.yr, f.ssn = f.ssn)
}

#Get player info from observations off baseball cube's D1 data
college.bios <- function(d){
  n.obs <- length(d$year);  n.ssns <- length(unique(d$year))
  r.ssn <- min(d$year);  f.ssn <- max(d$year)
  season1 <- d[d$year == r.ssn,]
  r.tm <- season1$teamName[which.max(season1$AB)]
  r.lg <- season1$LeagueAbbr[which.max(season1$AB)]
  seasonf <- d[d$year == f.ssn,]
  f.tm <- seasonf$teamName[which.max(seasonf$AB)]
  f.lg <- seasonf$LeagueAbbr[which.max(seasonf$AB)]
  n.tm <- length(unique(d$teamName))
  bday <- d$borndate[1]; place <- d$Place[1]
  bats <- d$Bats[1]; throws <- d$Throws[1]; posit <- d$posit[1]
  draft.yr <- d$draft_year[1]; draft.rd <- d$draft_Round[1]
  draft.overall <- d$draft_overall[1]; draft.tm <- d$Draft_Team[1]
  mlbid <- d$mlbid[1]
  data.frame(n.obs = n.obs, n.ssns = n.ssns, r.ssn = r.ssn, r.tm = r.tm,
             r.lg = r.lg, f.ssn = f.ssn, f.tm = f.tm, f.lg = f.lg, n.tm = n.tm,
             bday = bday, place = place, bats = bats, throws = throws,
             posit = posit, draft.yr = draft.yr, draft.rd = draft.rd,
             draft.overall = draft.overall, draft.tm = draft.tm, mlbid = mlbid)
}

#Get player info from observations off baseball cube's summer data
summer.bios <- function(d){
  n.obs.summer <- length(d$year);  n.ssns.summer <- length(unique(d$year))
  r.ssn.summer <- min(d$year);  f.ssn.summer <- max(d$year)
  season1 <- d[d$year == r.ssn.summer,]
  school <- d$school[1]
  r.tm.summer <- season1$teamName[which.max(season1$AB)]
  r.lg.summer <- season1$LeagueName[which.max(season1$AB)]
  seasonf <- d[d$year == f.ssn.summer,]
  f.tm.summer <- seasonf$teamName[which.max(seasonf$AB)]
  f.lg.summer <- seasonf$LeagueName[which.max(seasonf$AB)]
  bday <- d$borndate[1]; place <- d$place[1]
  bats <- d$Bats[1]; throws <- d$Throws[1]; posit <- d$posit[1]
  draft.yr <- d$draft_Year[1]; draft.rd <- d$draft_Round[1]
  draft.overall <- d$draft_overall[1]; draft.tm.abbr <- d$draft_teamabbr[1]
  status <- d$status[1]; current.tm <- d$CurrentTeam[1]
  data.frame(n.obs.summer = n.obs.summer, n.ssns.summer = n.ssns.summer, r.ssn.summer = r.ssn.summer, r.tm.summer = r.tm.summer,
             r.lg.summer = r.lg.summer, f.ssn.summer = f.ssn.summer, f.tm.summer = f.tm.summer, f.lg.summer = f.lg.summer, 
             school = school, bday = bday, place = place, bats = bats, throws = throws,
             posit = posit, draft.yr = draft.yr, draft.rd = draft.rd,
             draft.overall = draft.overall, draft.tm.abbr = draft.tm,
             status = status, current.tm = current.tm)
}

#confidence interval for lm
conf_int.lm=function(lmout,c,d=0,a=0.05){
  b=as.matrix(coef(lmout))
  c = as.matrix(c)
  var.b <- vcov(lmout)
  X=model.matrix(lmout)
  df= nrow(X) - length(b)
  cb.d=t(c)%*%b-d
  X.X <- t(X)%*%X
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(t(c)%*%var.b%*%c)
  lower = cb.d - tquant%*%sqrt(t(c)%*%var.b%*%c)
  upper = cb.d + tquant%*%sqrt(t(c)%*%var.b%*%c)
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}
#t test for linear model lm
t.test.lm=function(lmout,c,d=0){
  b=as.matrix(coef(lmout))
  var.b <- vcov(lmout)
  c = as.matrix(c)
  X=model.matrix(lmout)
  df= nrow(X) - length(b)
  cb.d=t(c)%*%b-d
  #X.X <- t(X)%*%X
  sse = sqrt(t(c)%*%var.b%*%c)
  tstat=drop(cb.d)/sse
  pvalue=2*(1-pt(abs(tstat),df))
  data.frame(tstat=tstat,pvalue=pvalue)
}

#F test for linear model lm
F.test.lm=function(lmout,C,d=0){
  b=as.matrix(coef(lmout))
  V=vcov(lmout)
  dfn=nrow(C)
  dfd=lmout$df
  Cb.d=C%*%b-d
  Fstat=drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn)
  pvalue=1-pf(Fstat,dfn,dfd)
  data.frame(Fstat=Fstat,pvalue=pvalue)
}

#Extract last n digits from a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
