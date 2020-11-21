#Lee Przybylski
#11/21/2020
#Fit the final model on the collegiate data using REML, peform hypothesis testing
#You must run the first portion of college_mixed model.R
library(ggplot2)

t0 <- Sys.time()
model.c <-lmer(OPS ~ lg.yr + r.lg + r.ssn.sc + r.lg:r.ssn.sc  + r.lg:I(r.ssn.sc^2)+ (1|Plyr) + (1|tm.yr),
               data = hit, weights = PAsc, REML = FALSE)
tf <- Sys.time()
print(tf - t0)

xtable(anova(model.c))
summary(model.c)
unique(NCAA_lgs$Abbr)
#Extract lgeffects for power 5 conf in any year
yr <- 2012:2019
power5 <- c("SEC", "Pac12", "Big10", "ACC", "Big12")
lgs <- rep(power5, each = length(yr))
yrs <- rep(yr, 5)
lg.yrs <- paste(lgs, yrs, sep = ".")
effs <- paste("lg.yr", lg.yrs, sep ="" )

lgeffects <- fixef(model.c)[effs]
head(lgeffects)
d_lgeffs <- cbind.data.frame(yrs, lgeffects, lgs)
names(d_lgeffs) <- c("Year", "Effect", "League")
head(d_lgeffs)
summary(d_lgeffs)
LEplot <- ggplot(d_lgeffs, aes(x = Year, y = Effect, color = League)) + 
  geom_line(size = 1.5) 
LEplot <- LEplot+labs(title = "League Effects on OPS in Power 5", x = "Year", y = "Difference in OPS")+ 
  scale_x_continuous(breaks=seq(2010, 2020, 2))

residuals <- residuals(model.c)
plot(hit$YR, residuals)

#Test how much larger lgeffects are for power5 between 2014 and 2018
lgs_minus <- paste(power5, 2014, sep = ".")
eff_minus <- paste("lg.yr", lgs_minus, sep = "")
Cminus <- ifelse(names(fixef(model.c)) %in% eff_minus, 1, 0)
lgs_plus <- paste(power5, 2018, sep = ".")
eff_plus <- paste("lg.yr", lgs_plus, sep = "")
Cplus <- ifelse(names(fixef(model.c)) %in% eff_plus, 1, 0)
C <- Cplus - Cminus
#Compute the t-statistic and confidence interval for the test
contest(model.c, L = C, joint = F, confint = T)

#How do MAC hitters compare to BIG10 hitters from 2014
Cmac <- ifelse(names(fixef(model.c)) == "r.lgMAC", 1, 0)
CB10 <- ifelse(names(fixef(model.c)) == "r.lgBig10", 1, 0)
C0 <- ifelse(names(fixef(model.c)) == "r.lgnon-DI", 1, 0)
C <- matrix(rbind(CB10-Cmac, CB10-C0, Cmac-C0), nrow = 3)
o <- contest(model.c, L = C, joint = F, confint = T)
o
xtable(o, digits = 4)

#How Many Big 10 people get drafted, vs MaC, vs non-DI
d <- bios[bios$r.lg == "Big10",]
sum(d$draft.rd >0)/length(d$draft.rd)
sum(d$draft.rd >0 & d$r.ssn == 2014)/sum(d$r.ssn== 2014)
#248 total, 24 from 2014
#23.46 percent total, 35.29 % in 2014

d <- bios[bios$r.lg == "MAC",]
sum(d$draft.rd >0)/length(d$draft.rd)
sum(d$draft.rd >0 & d$r.ssn == 2014)/sum(d$r.ssn== 2014)
#105 total, 8 in 2014
#10.57% total, 10.26% in 2014

d <- bios[bios$r.lg == "non-DI",]
sum(d$draft.rd >0)/length(d$draft.rd)
sum(d$draft.rd >0 & d$r.ssn == 2014)/sum(d$r.ssn== 2014)
#396 total, 53 in 2014
#2.93 % total, 5.45% from 2014
