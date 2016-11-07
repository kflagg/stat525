# Homework 8
# Problem 1
diabetes<-cbind(c(15,5),c(50,15))
mcnemar.test(diabetes)

# Problem 2
id<-rep(1:167,each=2)
status<-rep(c(1,0),167)
smk<-c(rep(c(1,1),15),rep(c(1,0),40),rep(c(0,1),22),rep(c(0,0),90))
# write a function to help compute z values
fun<-function(x){x[1] - x[2]}
z1<-by(smk,id,FUN=fun)
resp<-status[seq(1,333,2)]
fit<-glm(resp~z1-1,family=binomial)

#Problem 3
lip.cancer<-read.table(file.choose(),header=T)
names(lip.cancer)<-c("id","obs","exp","aff","lat","long")
fit.a<-glm(obs~aff,family=poisson,offset=log(exp),data=lip.cancer)
fit.b<-glm(obs~aff + lat,family=poisson,offset=log(exp),data=lip.cancer)


