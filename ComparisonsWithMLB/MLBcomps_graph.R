#This script is for comparing leagues and seasons accross 2 years of MLB

library(Lahman)
library(vistime)
library(plyr)
library(MASS)
library(igraph)
source("CompFunctions.R")

d.connections <- read.csv("MLB_2yr_intersections.csv", header = TRUE, sep = ",")
hitters <- Batting
hitters$LGYR <- as.factor(paste(hitters$lgID, hitters$yearID, sep = "."))
source("CompFunctions.R")
hitters.lgyr <- ddply(hitters, .(playerID, LGYR), collapse.season)
summary(hitters.lgyr)
min.pa <- 20
obs <- hitters.lgyr[hitters.lgyr$PA >= 20,]
obs$playerID <- as.factor(obs$playerID)
summary(obs)

#4 adjacency matrices
# A.conn is the number of common players between the two leagues
# A.comp is the t statistic describing the how much more the average players ops is in lg1 vs lg2
# A.pval is the p value associated with the t stat in A.comp
# A.lgeff is the t stat is the difference in league effects:  lg1 - lg2
yr <- 1885
  obs.yr <- obs[obs$YR >= yr-1 & obs$YR <= yr,c(1:8, 21:23)]
  summary(obs.yr)
  lgs <- as.character(unique(obs.yr$LGYR))
  N <- length(lgs)
  A.conn <- matrix(0, nrow = N, ncol = N)
  dimnames(A.conn) <- list(lgs,lgs)
  A.comp <- matrix(0, nrow = N, ncol = N)
  dimnames(A.comp) <- list(lgs,lgs)
  A.pval <-matrix(0, nrow = N, ncol = N)
  dimnames(A.pval) <- list(lgs,lgs)
  A.var <-matrix(0, nrow = N, ncol = N)
  dimnames(A.var) <- list(lgs,lgs)
  A.lgeff <- matrix(0, nrow = N, ncol = N)
  dimnames(A.lgeff) <- list(lgs,lgs)
  A.lgvar <- matrix(0, nrow = N, ncol = N)
  dimnames(A.lgvar) <- list(lgs,lgs)
  model1 <- lm(OPS ~ playerID + LGYR, data = obs.yr)
  X <- model.matrix(model1)
  betahat <- coef(model1)
  anova(model1)
  for (i in 1:(N-1)){
    for (j in (i+1):N){
      lg1 <- lgs[i]
      lg2 <- lgs[j]
      lg1r <- obs.yr$playerID[obs.yr$LGYR == lg1]
      lg2r <- obs.yr$playerID[obs.yr$LGYR == lg2]
      plyrs <- unique(obs.yr$playerID)
      ovrlp <-sum(plyrs %in% lg1r & plyrs %in% lg2r)
      A.conn[lg1,lg2] <- ovrlp
      A.conn[lg2,lg1] <- ovrlp
      ind1<- ifelse(obs.yr$LGYR == lg1, 1,0)
      c1 <- ind1/sum(ind1)
      ind2<- ifelse(obs.yr$LGYR == lg2, 1,0)
      c2 <- ind2/sum(ind2)
      lgeffs <- rep(0,length(coef(model1)))
      r1 <- names(betahat) == paste("LGYR", lg1, sep = "")
      r2 <- names(betahat) == paste("LGYR", lg2, sep = "")
      #sum(r1)
      en <- rep(0, length(betahat))
      en[r1]<- -1
      en[r2] <- 1
      c <- t(c1-c2)%*%X+t(en)
      d1 <- t.test.lm(model1,c)
      d2 <- conf_int.lm(model1, c, a = 0.05)
      A.comp[lg1,lg2] <- d2[1,"est"]
      A.pval[lg1,lg2] <- d1[1, "pvalue"]
      A.var[lg1,lg2] <- d2[1, "std.dev"]
      c.lg <- -t(en)
      d3 <- conf_int.lm(model1, c.lg, a = 0.05)
      A.lgeff[lg1,lg2] <- d3[1,"est"]
      A.lgvar[lg1,lg2] <- d3[1, "std.dev"]
    }}

G.comp <- graph_from_adjacency_matrix(A.comp, mode = "directed", diag = F, weighted = T)  
plot(G.comp, edge.label = round(E(G.comp)$weight, 3), main = "Avg Talent OPS Comparison", layout = layout_in_circle)
G.pval <- graph_from_adjacency_matrix(A.pval, mode = "directed", diag = F, weighted = T)  
plot(G.comp, edge.label = E(G.pval)$weight, main = "P-values", layout = layout_in_circle)
G.var <- graph_from_adjacency_matrix(A.var, mode = "max", diag = F, weighted = T)  
plot(G.var, edge.label = round(E(G.var)$weight, 3), main = "Std Dev of Comp Estimates", layout = layout_in_circle)
G.conn <- graph_from_adjacency_matrix(A.conn, mode = "undirected", weighted = T)
plot(G.conn, edge.label=E(G.conn)$weight, layout = layout_in_circle, main = "Number of Common Players")
G.lgeff <- graph_from_adjacency_matrix(A.lgeff, mode = "directed", diag = F, weighted = T)
plot(G.lgeff, edge.label= round(E(G.lgeff)$weight, 4), layout = layout_in_circle, main = "League Effects on OPS")
G.lgvar <- graph_from_adjacency_matrix(A.lgvar, mode = "max", diag = F, weighted = T)  
plot(G.lgvar, edge.label = round(E(G.lgvar)$weight, 3), main = "Std Dev of Lg Effect Estimates", layout = layout_in_circle)
