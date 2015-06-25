##############################
# Problem 1: Part b code
##############################

Data=read.table('Women.txt', header=T);
attach(Data);
X=cbind(Data[3],Data[6]);

n=length(X[,1])
p=length(X);
mu=c(colMeans(X));
Sigma=cov(X);

# Construct .50 and .95 Probability ellipse
library(ellipse)
png("P-Ellipse.png")
par(mfrow=c(1,2))
e1=ellipse(Sigma,centre=mu,level=.5) 
e2=ellipse(Sigma,centre=mu,level=.95)
plot(e2, type='l',xlim=range(X[,1]),ylim=range(X[,2]))
points(X[,1],X[,2])
plot(e1, type='l',xlim=range(X[,1]),ylim=range(X[,2]))
points(X[,1],X[,2])
dev.off()

# 4-plots for Mean.
# 95% Confidence Ellipse for the mean
c2= qf(.95, p,  n-p)*p*(n-1)/(n-p)
png('95CE.png')
plot(ellipse(Sigma/n, centre=mu, t=sqrt(c2)), type="l", xlab='100m',ylab='800m',main="Confidence Regions for Mean")

# 95% T^2 regions
c1=sqrt(((p*(n-1))/(n*(n-p)))*qf(.95,p,n-p)*Sigma[1,1])
c2=sqrt(((p*(n-1))/(n*(n-p)))*qf(.95,p,n-p)*Sigma[2,2])
v1=c(mu[1]-c1, mu[1] + c1)
v2=c(mu[2]-c2, mu[2] + c2)
abline(v=v1,col="blue")
abline(h=v2,col="blue")

# 95% Bonferroni Confidence
Bon=cbind(mu-qt(1-.05/(2*p), df=n-1)*sqrt(diag(Sigma)/n),
          mu + qt(1-.05/(2*p),df=n-1)*sqrt(diag(Sigma)/n))
abline(v=Bon[1,],col="red")
abline(h=Bon[2,],col="red")

# Individual 95% CI for 100m and 800m
ind1=cbind(mu[1]+qt(.5/2, df=n-1)*sqrt(Sigma[1,1]/n),
           mu[1]-qt(.5/2, df=n-1)*sqrt(Sigma[1,1]/n))

ind2=cbind(mu[2]+qt(.5/2, df=n-1)*sqrt(Sigma[2,2]/n),
           mu[2]-qt(.5/2, df=n-1)*sqrt(Sigma[2,2]/n))

abline(v=ind1, col="green")
abline(h=ind2,col="green")

# Plot sample mean
points(mu[1],mu[2])

dev.off()

