# Homework 11 R code	
require(survival)
require(KMsurv)
require(SurvRegCensCov)
data(tongue)
# if you want to see a brief description
# of the data set 
help(tongue)

fit.cox<-coxph(Surv(time,delta)~factor(type),tongue)
summary(fit.cox)
fit.zph<-cox.zph(fit.cox)
fit.zph
plot(fit.zph)

fit.wei<-WeibullReg(Surv(time,delta)~factor(type),tongue)
fit.wei

fit.llogis<-survreg(Surv(time,delta)~factor(type),dist="loglogistic",tongue)
summary(fit.llogis)


