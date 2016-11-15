# Homework 8 R code
require(spatstat)
larynx<-chorley[chorley$marks=="larynx"]
larynx<-unique.ppp(larynx)
plot(larynx) # ignore the lung
# Problem 1
# You need the mean nearest neighbor
# distance in the obsrved data. 
hbar<-mean(nndist(larynx))
hbar
# You need the mean nubmer of observations
# per unit area and you can get that using 
# the intensity function
intensity(larynx)

plot(Gest(larynx))
plot(envelope(larynx,fun="Gest"))

# Problem 2 
plot(longleaf)
fit<-quadrat.test(longleaf,alternative="clustered")
fit
plot(fit)


# observed counts
long.obs<-c(34,33,17,7,3,6)
# expected counts for 0 to 4
long.exp<-100*dpois(0:4,1.43)
# expected count for >=5
long.exp<-c(long.exp,100-cumsum(long.exp)[5])
# so the expected counts are
long.exp
# I will leave the rest up to you.


# Old Problem 2 - Ignore for Homework 9
# get the mean distance
xy.incin<-c(354.5,413.6)
xy.coord<-cbind(larynx$x,larynx$y)
larynx.dist<-pairdist(rbind(xy.incin,xy.coord))
meandist<-mean(larynx.dist[1,-1])
store.vec<-rep(0,1000)
store.vec[1]<-meandist
for(i in 2:1000){
simdata<-rpoispp(0.1840363,win=Window(chorley[chorley$marks=="larynx"]))
xy.simcoord<-cbind(simdata$x,simdata$y)
simdata.dist<-pairdist(rbind(xy.incin,xy.simcoord))
store.vec[i]<-mean(simdata.dist[1,-1])
}
