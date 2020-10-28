#Trouble shooting for the fitting of plyr.mx in PlyrMxEff_MLB.R
library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")

tt <- getME(plyr.mx,"theta")
ll <- getME(plyr.mx,"lower")
min(tt[ll==0])
#Since this is larger than e-6, there should not be any problems with singularities

#here we double check the gradient calculations
#This gives us the gradient and hessian from the fitting
derivs1 <- plyr.mx@optinfo$derivs
grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(grad1),abs(derivs1$gradient)))

#Try fitting with some more iterations, starting from the previous point:
ss <- getME(plyr.mx,c("theta","fixef"))
mx2 <- update(plyr.mx,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
#Same Warning messages:
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :unable to evaluate scaled gradient
#  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                    Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#Try fitting with a different optimizer
mx3 <- update(plyr.mx, start=ss,control=lmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
#Same Error message appears
#Try Nelder-Mead Optimization and try fiting with MaxLiklihd instead of REML
t0 <- Sys.time()
mx.4 <-lmer(OPS ~ Plyr + lg.yr + (1 | tm.yr) + (1 |plyr.yr) + (1|LG), data = hit, weights = PA, verbose = 1, REML = FALSE,control = lmerControl(optimizer ="Nelder_Mead"))
tf <- Sys.time()
tf - t0
#It seems we got a singular fit:
#boundary (singular) fit: see ?isSingular
#> tf <- Sys.time()
#> tf - t0
#Time difference of 2.465056 hours
isSingular(mx.4)
#[1] TRUE
isSingular(plyr.mx)
#[1] FALSE
tt <- getME(mx.4,"theta")
ll <- getME(mx.4,"lower")
min(tt[ll==0])
#The LG variance is zero

#Try Nelder-Mead Optimization fiting with REML
t0 <- Sys.time()
mx.5 <-lmer(OPS ~ Plyr + lg.yr + (1 | tm.yr) + (1 |plyr.yr) + (1|LG), data = hit, weights = PA, verbose = 1, REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))
tf <- Sys.time()
tf - t0
#Same errors as with mx.4
#> isSingular(mx.5)
#[1] TRUE
tt <- getME(mx.5,"theta")
ll <- getME(mx.5,"lower")
min(tt[ll==0])
#this time the tm.yr variance is 0

#Change the model slightly
hit$YRf <- as.factor(hit$YR)
t0 <- Sys.time()
mx.6 <-lmer(OPS ~ Plyr + lg.yr + (1 | tm.yr) + (1 |plyr.yr) + (1|YRf), data = hit, weights = PA, verbose = 1, REML = TRUE, control = lmerControl(optimizer ="Nelder_Mead"))
tf <- Sys.time()
tf - t0
#Same singular fit
tt <- getME(mx.6,"theta")
ll <- getME(mx.6,"lower")
min(tt[ll==0])
#this time the tm.yr variance is 0

#Try excluding some of the random effects
t0 <- Sys.time()
mx.5 <-lmer(OPS ~ Plyr + lg.yr + (1 | tm.yr), data = hit, weights = PA, verbose = 1, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
tf <- Sys.time()
tf - t0
#Warning message:
#  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                 Model is nearly unidentifiable: very large eigenvalue
#               - Rescale variables?
fixed.effects(mx.5)
tail(fixed.effects(mx.5), n = 20)

#Try Lg:YRf as a fixed effect
head(hit)
t0 <- Sys.time()
mx.6 <-lmer(OPS ~ Plyr + LG:YRf + (1 | TM:YRf), data = hit, weights = PA, verbose = 1, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
tf <- Sys.time()
tf - t0
#Same warning as above

#Try a TM random effect only
t0 <- Sys.time()
mx.5 <-lmer(OPS ~ Plyr + lg.yr + (1 | TM), data = hit, weights = PA, verbose = 1, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
tf <- Sys.time()
tf - t0
#This model was able to fit in about 25 minutes with no error message.
