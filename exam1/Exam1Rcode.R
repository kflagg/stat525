# BioStatExam1Rcode
# Problem 2 Data
# Pooled Data
TB.pool<-array(c(50,238,21,524),dim=c(2,2),
dimnames=list(c("BiomassYes","BiomassNo"),c("Case","Control")))
TB<-array(c(38,102,12,141,12,136,9,383),dim=c(2,2,2),
dimnames=list(c("BiomassYes","BiomassNo"),c("Case","Control"),c("< 1000",">= 1000")))
mantelhaen.test(TB)
breslowday.test(TB)
## Use whatever function you want for odds ratios
## and their intervals

# Problem 4
LC<-array(c(7,55,489,475,293,38,61,129,570,431,154,12),dim=c(6,2),
dimnames=list(c("None","<5","5-14","15-24","25-49","50+"),c("LCYes","LCNo")))
results<-chisq.test(LC)
# row totals
n<-rowSums(LC)
# number of lung cancer cases
d<-LC[,1]
prop.trend.test(d,n)
prop.test(d,n)

# Problem 5
vaccine<-array(c(33,10,78,94),dim=c(2,2),dimnames=list(c("Placebo","Vaccine"),
c("Disease","No Disease")))
