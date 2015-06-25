#############################################
# Problem1 part d
#############################################
library(xtable)

MenData=read.table('Men.dat', header=T)
attach(MenData)
Men=cbind(MenData[,3:10])
mu=colMeans(Men)
corr=cor(Men)
covv=cov(Men)

CenteredMen=Men-matrix(rep(mu,54),ncol=8,byrow=T)

# find eigenvalue/eigenvector pairs of corr matrix
Eig=eigen(corr)

# first two retain .94 percent of variance
pVar=sum(Eig$values[1:2])/sum(Eig$values)
pVar

# standardize students scores by dividing by diag elements in covv
StandardMen=t(t(CenteredMen) / diag(covv))


# biPlot of P1,P2
p1=as.matrix(StandardMen) %*% Eig$vectors[,1]
p2=as.matrix(StandardMen) %*% Eig$vectors[,2]

png("PCAbiplot.png")
plot(p1,p2, main="PCA BiPlot")
dev.off()

# order by PC1, output top ten in latex table format
x=order(-p1)
x1=p1[x]
x2=MenData$Country[x]
df=data.frame(x2,x1)
df

xtable(df[1:10,])

