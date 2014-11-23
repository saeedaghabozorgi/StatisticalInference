library(ggplot2)
nosim=1000
nsam=40
lambda=0.2
dataSam<-rexp(nosim*nsam,lambda)
data.mean<-apply(matrix(dataSam,nosim,nsam),1,mean)

#distribution center:
dist.center<-mean(data.mean)
dist.center
#Theoretical Center (exponential distribution):
exp.center<-1/lambda
exp.center
#--------------------------------------
#distribution
dist.variance<-(sd(data.mean))^2
dist.variance

#Theoretical variance (exponential distribution):
exp.variance<-((1/lambda)^2)/nsam
exp.variance

#---------------------------------------


hist(data.mean, freq = FALSE, breaks = 20, xlim = c(2, 8))
nx <- seq(2, 8, length=100)
hx <- dnorm(nx,mean=5,sd=1)
lines(nx,hx, lwd=2, col="red")


#--------------------------------------

interval <- dist.center +c(-1,1)*1.96*sqrt(dist.variance/nsam)
interval

