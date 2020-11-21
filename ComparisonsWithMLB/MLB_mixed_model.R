#Lee Przybylski
#11/21/2020

#Fit the final MLB OPS model and perform hypothesis testing.  Must run the start of FitMixedEffModel_MLB.R
library(xtable)

model.mlb <-lmer(OPS ~ lg.yr + birth.yrf + (1|Plyr) + (1|tm.yr), data = hit, weights = PAsc, REML = TRUE)
#Extract the anova table
xtable(anova(model.mlb))
summary(model.mlb)
lgeffects <- fixef(model.mlb)[grepl("lg.yr", names(fixef(model.mlb)))]
head(lgeffects)
ssns <- as.numeric(substrRight(names(lgeffects), 4))
head(ssns)
plot(ssns, lgeffects, xlab = "season", ylab = "lg.yr effect")
abline(h = 0)
abline(v = 1973, col = "blue", lty = "dashed")
lgs <- substr(names(lgeffects),1,7)
lgs <- as.factor(substrRight(lgs, 2))
d_lgeffs <- cbind.data.frame(ssns, lgeffects, lgs)
names(d_lgeffs) <- c("Year", "Effect", "League")
head(d_lgeffs)
summary(d_lgeffs)
library(ggplot2)
LEplot <- ggplot(d_lgeffs, aes(x = Year, y = Effect, color = League)) + 
  geom_line(size = 1.5) 
LEplot <- LEplot+labs(title = "League Effects on OPS", x = "Year", y = "Difference in OPS")+ 
  scale_x_continuous(breaks=seq(1870, 2020, 10))
LEplot + scale_y_continuous(breaks=seq(-0.2, 0.4, 0.05))+
  geom_vline(xintercept = 1884, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1889, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1901, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 1903, linetype="dotted", color = "blue", size=1)+
  geom_vline(xintercept = 1893, linetype="dotted", color = "black", size=1)+
  geom_vline(xintercept = 1969, linetype="dotted", color = "black", size=1)

birth.yr.eff <- fixef(model.mlb)[grepl("birth.yr", names(fixef(model.mlb)))]
birth.yr <- as.numeric(substrRight(names(birth.yr.eff), 4))
head(birth.yr)
plot(birth.yr, birth.yr.eff)
abline(h = 0)
#abline(v = 1973, col = "blue", lty = "dashed")
d_b.effs <- cbind.data.frame(birth.yr, birth.yr.eff)
names(d_b.effs) <- c("Birth_Year", "Effect")
head(d_b.effs)
BEplot <- ggplot(d_b.effs, aes(x = Birth_Year, y = Effect)) + 
  geom_point(size = 1.5) 
BEplot <- BEplot+labs(title = "Hitting Ability by Birth Year", x = "Birth Year",
                      y = "Difference in OPS")
BEplot + scale_x_continuous(breaks=seq(1840, 2020, 10))
  