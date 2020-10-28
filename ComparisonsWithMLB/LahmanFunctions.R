#Here are some useful functions for baseball data.  Most are written with the Lahman data in mind

collapse.stint <- function(d){
  G <- sum(d$G); AB <- AB <- sum(d$AB); R - sum(d$R)
  H<- sum(d$H); X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <- sum (d$HR) ; RBI <- (d$RBI); SB <- sum(d$SB)
  CS <- sum (d$CS); BB <- sum(d$BB) ; SH <- sum(d$SH)
  SF <- sum(d$SF); HBP <- sum(d$HBP)
  SLG <- (H-X2B-X3B-HR+2*X2B+3*X3B+4*HR)/AB
  OBP <- (H+BB+HBP)/(AB+BB+HBP+SF)
  OPS <- SLG+OBP
  data.frame(G =G, AB = AB, R = R, H=H, X2B= X2B, X3B = X3B,
             HR= HR, RBI = RBI, SB=SB, CS=CS, BB=BB, HBP = HBP,
             SH=SH, SF=SF, SLG=SLG, OBP=OBP, OPS=OPS,
             Career.AB = d$Career.AB[1], POS = d$POS[1])
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
