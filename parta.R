##############################
# Problem !:  Part a code
#############################

# used to generate tables for latex format
# install.packages("xtable")
library(xtable)

# save all output to a txt file. 
sink("parta.txt", append=FALSE, split=FALSE)

# Load Data
Data=read.table('Women.txt', header=T);
X=Data[3:9];
attach(X);

# Summary Statistics: Two parts to fit on page
xtable(summary(X[1:4]))
xtable(summary(X[5:7]))

# Correlation Matrix
xtable(cor(X))

# generate png for matrix scatter plot iamge
png("a.scatterMatrix.png", width = 6, height = 6, units = 'in', res=300)
pairs(X)
dev.off()

########################################################################
# Univariate  Normality Assessment 

# Histograms
png("a.histograms.png")
par(mfrow = c(2,4))
hist(X100m)
hist(X200m)
hist(X400m)
hist(X800m)
hist(X1500m)
hist(X3000m)
hist(Marathon)
dev.off()


# Q-Q plots of each race
png("a.QQplots.png")
par(mfrow =c(2,4))
qqnorm(X100m, main ="100m")
qqline(X100m)
qqnorm(X200m, main ="200m")
qqline(X200m)
qqnorm(X400m, main ="400m")
qqline(X400m)
qqnorm(X800m, main ="800m")
qqline(X800m)
qqnorm(X1500m, main ="1500m")
qqline(X1500m)
qqnorm(X3000m, main ="3000m")
qqline(X3000m)
qqnorm(Marathon, main ="Marathon")
qqline(Marathon)
dev.off()


# Q-Q plots of each race: considering log transform on longer races
png("a.lQQplots.png")
par(mfrow =c(2,4))
qqnorm(X100m, main ="100m")
qqline(X100m)
qqnorm(X200m, main ="200m")
qqline(X200m)
qqnorm(X400m, main ="400m")
qqline(X400m)
qqnorm(X800m, main ="800m")
qqline(X800m)
qqnorm(log(X1500m), main ="log1500m")
qqline(log(X1500m))
qqnorm(log(X3000m), main ="log3000m")
qqline(log(X3000m))
qqnorm(log(Marathon), main ="logMarathon")
qqline(log(Marathon))
dev.off()

########################################################################
# Multivariate Normality

# Chi-Square Plot
#   requires chisplot.r in directory
source("chisplot.R")

# note: shorter distances seem normal so far. Consider upto X800m 
png("chisqrShort.png")
chisplot(cbind(X100m,X200m,X400m,X800m))
dev.off()

# Pairwise normality for all shorter distance runs
# Contour plots
library(KernSmooth)
png("ContourPlots.png")
par(mfrow=c(2,3))
densSM <- bkde2D(cbind(X100m,X200m), bandwidth=c(bw.nrd(X100m),bw.nrd(X200m)))
plot(X100m,X200m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)

densSM <- bkde2D(cbind(X100m,X400m), bandwidth=c(bw.nrd(X100m),bw.nrd(X400m)))
plot(X100m,X400m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)

densSM <- bkde2D(cbind(X100m,X800m), bandwidth=c(bw.nrd(X100m),bw.nrd(X800m)))
plot(X100m,X800m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)

densSM <- bkde2D(cbind(X200m,X400m), bandwidth=c(bw.nrd(X200m),bw.nrd(X400m)))
plot(X200m,X400m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)

densSM <- bkde2D(cbind(X200m,X800m), bandwidth=c(bw.nrd(X200m),bw.nrd(X800m)))
plot(X200m,X800m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)

densSM <- bkde2D(cbind(X400m,X800m), bandwidth=c(bw.nrd(X400m),bw.nrd(X800m)))
plot(X400m,X800m)
contour(densSM$x1,densSM$x2,densSM$fhat, lty=2,nlevels=4,add=T)
dev.off()
