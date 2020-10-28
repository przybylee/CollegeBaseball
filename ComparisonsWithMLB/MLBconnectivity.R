#Explore connectivity of leagues for MLBhitters
options(stringsAsFactors = T)
hitters <- Batting
summary(hitters)
hitters$LGYR <- as.factor(paste(hitters$lgID, hitters$yearID))
hitters.lgyr <- ddply(hitters, .(playerID, LGYR), collapse.season)
summary(hitters.lgyr)
min.pa <- 20
obs <- hitters.lgyr[hitters.lgyr$PA >= 20,]
summary(obs)
leagues <- unique(obs$LG)
yrs <- unique(hitters$yearID)
head(yrs)
tail(yrs)
N <- length(yrs)
d.connections <- data.frame(matrix(0, nrow = N, ncol = 17)) 
names(d.connections) <- c("YR", as.character(leagues), "NL-AL", "NL-FL", "AL-FL", "NL-AA", "NL-UA", "NL-PL", "AA-UA", "AA-PL", "NA-NL")
yr = 1876
n <- 1
for (yr in yrs){
  d1 <- obs[obs$YR == yr-1,]
  d2 <- obs[obs$YR == yr,]
  AL1 <- unique(d1$playerID[d1$LG == "AL"])
  AL2 <- unique(d2$playerID[d2$LG == "AL"])
  NL1 <- unique(d1$playerID[d1$LG == "NL"])
  NL2 <- unique(d2$playerID[d2$LG == "NL"])
  FL1 <- unique(d1$playerID[d1$LG == "FL"])
  FL2 <- unique(d2$playerID[d2$LG == "FL"])
  AA1 <- unique(d1$playerID[d1$LG == "AA"])
  AA2 <- unique(d2$playerID[d2$LG == "AA"])
  UA1 <- unique(d1$playerID[d1$LG == "UA"])
  UA2 <- unique(d2$playerID[d2$LG == "UA"])
  PL1 <- unique(d1$playerID[d1$LG == "PL"])
  PL2 <- unique(d2$playerID[d2$LG == "PL"])
  NA1 <- unique(d1$playerID[d1$LG == "NA"])
  NA2 <- unique(d2$playerID[d2$LG == "NA"])
  ALr <- sum(AL1 %in% AL2)
  NLr <- sum(NL1 %in% NL2)
  FLr <- sum(FL1 %in% FL2)
  AAr <- sum(AA1 %in% AA2)
  UAr <- sum(UA1 %in% UA2)
  PLr <- sum(PL1 %in% PL2)
  NAr <- sum(NA1 %in% NA2)
  AL <- unique(c(AL1,AL2))
  NL <- unique(c(NL1, NL2))
  FL <- unique(c(FL1,FL2))  
  AA <- unique(c(AA1,AA2))  
  UA <- unique(c(UA1,UA2))  
  PL <- unique(c(PL1,PL2))  
  N.A <- unique(c(NA1, NA2))  
  NL.AL <- sum(AL %in% NL)
  NL.FL <- sum(NL %in% FL)
  AL.FL <- sum(AL %in% FL)
  NL.AA <- sum(NL %in% AA)
  NL.UA <- sum(NL %in% UA)
  NL.PL <- sum(NL %in% PL)
  AA.UA <- sum(AA %in% UA)
  AA.PL <- sum(AA %in% PL)
  NA.NL <- sum(N.A %in% NL)
  d <- data.frame(t(c(yr, ALr, NLr, NAr, FLr, AAr, UAr, PLr, NL.AL, NL.FL, AL.FL, NL.AA, NL.UA, NL.PL, AA.UA, AA.PL, NA.NL)))
  d.connections[n,] <- d
  n <- n+1
}

write.table(d.connections, file = "MLB_2yr_intersections.csv", col.names = T, row.names = F, sep = ",")
