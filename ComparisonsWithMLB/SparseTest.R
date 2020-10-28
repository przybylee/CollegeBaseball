library(Matrix)

#an experiment to get hypothesis testing using sparse matrices
summary(plyr.reg)
memory.size()
memory.limit()
memory.limit(size = 1000000)
memory.limit()
#confidence interval for lm
conf_int.lm_sp=function(lmout,c,d=0,a){
  c.sp <- Matrix(c, sparse = T)
  b=Matrix(coef(lmout), sparse = T)
  var.b <- Matrix(vcov(lmout), sparse = T)
  X=Matrix(model.matrix(lmout), sparse = T)
  df= nrow(X) - length(b)
  cb.d=(c%*%b)[1,1]-d
  #X.X <- t(X)%*%X
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(t(c)%*%var.b%*%c)[1,1]
  lower = cb.d - tquant*std.dev
  upper = cb.d + tquant*std.dev
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}
#confidence interval for lm
conf_int.lm=function(lmout,c,d=0,a){
  b=as.matrix(coef(lmout))
  c = as.matrix(c)
  var.b <- vcov(lmout)
  X=model.matrix(lmout)
  df= nrow(X) - length(b)
  cb.d=t(c)%*%b-d
  #X.X <- t(X)%*%X
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(t(c)%*%var.b%*%c)
  lower = cb.d - tquant%*%sqrt(t(c)%*%var.b%*%c)
  upper = cb.d + tquant%*%sqrt(t(c)%*%var.b%*%c)
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}

par.names <- names(coef(plyr.reg))
lg1 <- "AL.1971"
d.lg1 <- hit[hit$lg.yr == lg1,]
roster1 <- unique(d.lg1$Plyr)
peffs1 <- paste("Plyr", roster1, sep = "")
c.ros1 <- ifelse(par.names %in% peffs1, 1, 0)
c.ros1 <- c.ros1/sum(c.ros1)
c.lg1 <- ifelse(par.names ==  paste("lg.yr", lg1, sep = ""), 1, 0)

#Experiment with original conf_int function
t0 <- Sys.time()
d1 <- conf_int.lm(plyr.reg, c.ros1, a = 0.05)
tf<- Sys.time()
tf-t0
#Takes about 50 seconds to run on desktop for c.lg1
#Takes about 50 seconds to run for c.ros1

#Experiment with sparse matrix computations:
#First check multiplication:
Z=Matrix(model.matrix(plyr.reg), sparse = T)
#Still not enough memory
ncol(Matrix(c.lg1, sparse = T))
Sig = Matrix(vcov(plyr.reg), sparse = T)
vec = Matrix(c.ros1, sparse = T)
ncol(vec)
A = sqrt(t(vec)%*%Sig%*%vec)[1,1]
A
print(d1)
#Experiment with sparse conf_int function
t0 = Sys.time()
d2 = conf_int.lm_sp(plyr.reg, c.ros1, a = 0.05)
tf = Sys.time()
tf - t0
#This code takes about 50 seconds for c.lg1
#This code takes about 50 seconds for c.ros1
print(d2)

#ggplot demo
x = 1:10
y = c(5,6,3, 4:8, 7, 12)
M = cbind(x,y)
df = data.frame(X = x, Y = y)
head(df)
v = c("yes", "no")
df$lg_dffs = sample(v, 10, replace = T)
df$ssn_dffs = sample(v, 10, replace = T)
df1 = data.frame(X = 1:5, Y = 7:3)
df1$lg_dffs = df$lg_dffs[1:5]
df1$ssn_dffs = sample(v,5, replace = T)
df = rbind(df, df1)
df$LG = c(rep("NL", 10), rep("AL", 5))

library(ggplot2)
LEplot <- ggplot(df, aes(x = X, y = Y, col = LG)) + 
  geom_line(linetype = "dashed") +
  geom_point(aes(shape = lg_dffs, size = lg_dffs))+scale_shape_manual(values = c(20,8))+scale_size_manual(values = c(1,2))
LEplot <- LEplot+labs(title = "League Effects on BA", x = "Year", y = "Difference from NL 2018 BA")+ scale_x_continuous(breaks=seq(0, 10, 1))
#Now we plot over top of this some solid lines for places where ssn_diff is significant
dfline <- df
dfline$Y[dfline$ssn_dffs == "no"] <- NA
ggplot(dfline, aes(x = X, y = Y, col = LG)) + geom_line(linetype = "solid")
#Adding the second gemline draws in the solid line
LEplot + geom_line(data = df, aes(X, replace(Y, df$ssn_dffs == "no", NA)), size = 1)


#Stack Exchange demo:
#Data
z = data.frame(x = seq(1,10),y = c(1,2,2,3,2,15,2,3,4,2))
ggplot() + geom_line(data=z,aes(x,y), linetype="dotted") + geom_line(data=z, aes(x, replace(y, y==15, NA)))
