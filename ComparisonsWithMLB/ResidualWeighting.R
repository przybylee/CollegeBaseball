#Lee Przybylski
#11/2/2020
#Understand the mean variance relationship for baseball data
#load hit data in ModelSelection_MLB.R and PLyrEffReg_MLB.R

head(hit)
head(ssns)
roster <- ssns$Plyr[ssns$n.obs >1]
veterans <- hit[hit$Plyr %in% roster,]

#fit unweighted model
t0 <- Sys.time()
Unwtd <-lm(OPS ~  0 + lg.yr + Plyr, data = hit)
tf <- Sys.time()
tf - t0

res <- residuals(Unwtd)
PA <- hit$PA[res != 0]
res0 = res[res != 0]
cbind(PA, res0)
plot(PA, res0, xlab = "Plate Appearances")
abline(h = 0, col = "red")
plot(log(PA), log(res0))
resreg <- lm(log(res) ~ log(veterans$PA))
abline(resreg)
summary(resreg)
