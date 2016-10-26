# Homework 6 Script
TB<-array(c(38,102,12,141,12,136,9,383),dim=c(2,2,2),
dimnames=list(c("BiomassYes","BiomassNo"),c("Case","Control"),c("<1000",">=1000")))
mantelhaen.test(TB)
breslowday.test(TB)

TB<-cbind(c(38,102,12,136),c(12,141,9,383))
biomass<-rep(c("Yes","No"),2)
income<-rep(c("<1000",">1000"),each=2)
TB.data<-data.frame(biomass,income,cases=TB[,1],controls=TB[,2])
rm(biomass,income,TB)
attach(TB.data)
fit1<-glm(cbind(cases,controls)~biomass,family=binomial)
fit2<-glm(cbind(cases,controls)~biomass+income,family=binomial)
fit3<-glm(cbind(cases,controls)~biomass*income,family=binomial)
summary(fit1)
summary(fit2)
summary(fit3)
anova(fit1,fit2,test="Chisq")

chd<-array(c(32,31,50,66,78,558,505,594,501,739),dim=c(5,2),
dimnames=list(c("<150","151-160","161-170","171-180",">180"),c("CHD","no CHD")))
score<-1:5
fit<-glm(cbind(chd[,1],chd[,2])~score,family=binomial)
summary(fit)
