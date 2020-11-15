#Lee Przybylski
#11/2/2020
#Understand the mean variance relationship for baseball data
#load hit data in ModelSelection_MLB.R and PLyrEffReg_MLB.R

head(hit)
summary(hit)
head(ssns)
#roster <- ssns$Plyr[ssns$n.obs >1]
#veterans <- hit[hit$Plyr %in% roster,]

#fit unweighted model
t0 <- Sys.time()
Unwtd <-lm(OPS ~  0 + lg.yr + Plyr, data = hit)
tf <- Sys.time()
tf - t0

res <- residuals(Unwtd)
PA <- hit$PA[res != 0]
sqres <- (res[res != 0])^2
summary(cbind(PA, sqres))
plot(PA, sqres, xlab = "Plate Appearances", ylab = "Squared Residuals")
abline(h = 0, col = "red")
plot(log(PA), log(sqres), xlab = "log(PA)", ylab = "log(Res^2)")
resreg <- lm(log(sqres) ~ log(PA))
abline(resreg, col = "red")
summary(resreg)

smallres <- hit[log(res^2) < -60,]
summary(smallres)
ssns_small <- ssns[ssns$Plyr %in% smallres$Plyr,]
summary(ssns_small)
