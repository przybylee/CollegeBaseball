#Lee Przybylski
#11/21/2020
#Fit the final model on the collegiate data using REML, peform hypothesis testing
#You must run the first portion of college_mixed model.R

t0 <- Sys.time()
model.c <-lmer(OPS ~ lg.yr + r.lg*r.ssn.sc + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc)
tf <- Sys.time()
print(tf - t0)