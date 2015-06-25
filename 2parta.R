##################################################################
# Problem 2 part a
##################################################################

Ms=read.table('MS.txt',header=T)
attach(Ms)

# Seperate Groups by MS indicator
Ms0=Ms[Ms[,"MS"]==0,]
n0=length(Ms0[,1])
Ms1=Ms[Ms[,"MS"]==1,]
n1=length(Ms1[,1])

# suggested power transform for Age for MS=0
newMs0=Ms0
newMs0$Age=Ms0$Age^-0.4937005

# suggested transform: log on non zero elements MS=1
newMs1=Ms1
index=Ms1$S1diff!=0
newMs1$S1diff[index]=log(Ms1$S1diff[index])
index2=Ms1$S2diff!=0
newMs1$S2diff[index]=log(Ms1$S2diff[index2])

vars=c(2,4)
allno=Ms0[,1:5]
allyes=Ms1[,1:5]
no=Ms0[,vars]
yes=Ms1[,vars]
# mean and covariance and Spooled
mu0=colMeans(no)
mu1=colMeans(yes)

s0=cov(no)
s1=cov(yes)

Spooled=((n0-1)*s0)/(n0+n1-2)+((n1-1)*s1)/(n0+n1-2)

# Fishers discriminant function
data=rbind(no,yes) 

a = as.vector(t(mu0-mu1) %*% solve(Spooled))
a
#as.matrix(a)
c1=as.matrix(no) %*% a
c2=as.matrix(yes) %*% a

m=as.numeric(a %*% (mu0 + mu1)/2)
m
# Apparent Error
(sum(c1 < m) + sum(c2 >= m))/(n0+n1)

# Classify Function used for Lachenbruch's "Holdout Procedure"
# returns binary true false as list whose index refs group
Classify <- function(pop1, pop2,i){
    ## returns # misclassification in list. index ref group
    na=nrow(pop1)
    nb=nrow(pop2)
    muA=colMeans(pop1)
    muB=colMeans(pop2)
    Sa=cov(pop1)
    Sb=cov(pop2)
    Sp=((na-1)*Sa)/(na+nb-2)+((nb-1)*Sb)/(na+nb-2)
    a=as.vector(t(muA-muB) %*% solve(Sp))
    m=as.numeric(a %*% (muA + muB)/2)
    if(i<=na){
        c1= as.matrix(pop1[i,]) %*% a
    }else{
        c2= as.matrix(pop2[i,]) %*% a
    }
    c2m=length(which( c2  >= m))
    c1m=length(which(c1 < m))
    return(c(c1m,c2m))
}

# for loop to iterate through and hold out each and classify
#   accumualte how many individuals were miscalssified.
totm=0
for(i in 1:n0+n1){ 
    
    if(i<=n0){
        index=cbind(1:n0)
        index=subset(index,index !=i)
        m1=Classify(no[index,],yes,i)
        totm= totm+m1[1]
        totm
    }else{
        index=cbind(1:n1)
        index=subset(index,index !=i)
        m2=Classify(no,yes[index,],i)
        totm=totm+m1[2]
    }
}

# Returns the Expected Actual Error Rate
EAER=totm/(n0+n1)
EAER



