############################################################
# Problem 1: Part c code
############################################################
library(xtable)
Men=read.table('Men.dat', header=T)
Women=read.table('Women.txt', header=T)

M=cbind(Men[,3:7],Men[,10])
W=cbind(Women[,3:7],Women[,9])

# Large sample size implies Anova is Robust to non-normal data
# Consider all common distances for comparison of mean

data=rbind(W,setNames(M,names(W)))
names(data)[5]="Marathon"

gender=factor(gl(2,54,108,labels=c("Women","Men")))

xtable(summary(aov(as.matrix(data[,1])~gender)))
xtable(summary(aov(as.matrix(data[,2])~gender)))
xtable(summary(aov(as.matrix(data[,3])~gender)))
xtable(summary(aov(as.matrix(data[,4])~gender)))
xtable(summary(aov(as.matrix(data[,5])~gender)))
xtable(summary(aov(as.matrix(data[,6])~gender)))
