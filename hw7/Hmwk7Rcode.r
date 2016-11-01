# Chapter 7 R code
# Problem 1
sore.dat<-read.csv(file.choose(),header=T)
sore.fit<-glm(y~type+duration,family=binomial,data=sore.dat)
summary(sore.fit)
exp(confint(sore.fit))
exp(confint.default(sore.fit))
sore.fit2<-glm(y~type*duration,family=binomial,data=sore.dat)
summary(sore.fit2)
vcov(sore.fit2)
anova(sore.fit,sore.fit2,test="Chisq")

# Problem 2
y<-c(rep(1,18),rep(0,22))
age<-c(12,15,42,52,59,73,82,91,96,105,114,120,121,128,130,139,139,157,
1,1,2,8,11,18,22,31,37,61,72,81,97,112,118,127,131,140,151,159,177,206)
fit<-glm(y~age,family=binomial)
fit.q<-glm(y~age +I(age^2),family=binomial)
summary(fit)
summary(fit.q)
anova(fit,fit.q,test="Chisq")
plot(age,fitted(fit.q),xlab="age",ylab="fitted odds")

# problem 3
MD.dat<-ex2012
names(MD.dat)<-c("group","ck","h")
attach(MD.dat)
group<-ifelse(group=="Control",0,1)
fit<-glm(group~log(ck)+h,family=binomial)
pi.hat<-predict(fit.1,type="response")
y.hat<-ifelse(pi.hat>=38/120,1,0)
table(MD.dat$group,y.hat)
require(Epi)
# additive model
ROC(fitted(fit),MD.dat$group,plot="ROC")


