# Homework 10 R code

require(survival)

# Problem 1
# take a look at the data - it is not that big
aml

# Problem 2 - I give the result you need
# in the problem but go ahead and run this 
# code as you will use aml.obj below.
# Create the survival object
aml.obj<-Surv(aml$time,aml$status)
# Maintained times
aml.obj[1:11]
# Nonmaintained times
aml.obj[12:23]




# Problem 3
fitKM<-survfit(aml.obj~x,data=aml,conf.type="log-log")
print(fitKM,print.rmean=T)
summary(fitKM)
plot(fitKM,lty=3:2,xlab="time",ylab="survival")
legend(100,0.8,c("Non-Maintained","Maintained"),lty=2:3,lwd=2)
title(main="KM Survival Curves - AML Data")

# Problem 5
survdiff(Surv(time,status)~x,aml)

# Problem 7 
fitNA<-survfit(coxph(aml.obj[1:11]~1,data=aml[1:11,]),type="aalen")
plot(fitNA$time,fitNA$surv,type="s",xlab="Survival Time",
ylab="Suvival Probability")
points(fitNA$time,fitNA$surv,pch=1)
lines(fitKM$time[1:9],fitKM$surv[1:9],type="s",lty=2)
points(fitKM$time[1:9],fitKM$surv[1:9],pch=3)
legend(100,0.8,c("Nelson-Aalen","Kaplan-Meier"),pch=c(1,3))
summary(fitNA)

# Problem 8
fit.cox<-coxph(aml.obj~x,data=aml)
summary(fit.cox)