par(lwd=2,mgp=c(1,1,0))

xpoint<- 1.8

png(filename=paste0("~/Dropbox/LinearModels/LinearModelstexSlides/Day2_bivariate/CEF/BVN", xpoint, ".png")) 


# Modified to extract diagonal.
bivariate.normal <- function(x, mu, Sigma) 
  exp(-.5 * diag(t(x-mu) %*% solve(Sigma) %*% (x-mu))) / sqrt(2 * pi * det(Sigma))

mu <- c(0,0)
Sigma <- matrix(c(1,.8,.8,1), nrow=2)
x1 <- seq(-3, 3, length.out=50)
x2 <- seq(-3, 3, length.out=50)

plot(1:10,axes=FALSE,frame.plot=TRUE,lwd=1)

# z can now be calculated much easier.
z<-bivariate.normal(t(expand.grid(x1,x2)),mu,Sigma)
dim(z)<-c(length(x1),length(x2))
contour(x1, x2, z, col="#4545FF", drawlabels=FALSE, nlevels=4,
        xlab=expression(x), ylab=expression(y), lwd=2,xlim=range(x1),ylim=range(x2),frame.plot=TRUE,axes=FALSE,xaxs = "i", yaxs = "i")
axis(1,labels=FALSE,lwd.ticks=2)
axis(2,labels=FALSE,lwd.ticks=2)
abline(v=xpoint, col=1, lwd=2, lty=2)

text(2, -2, labels=paste("x=",xpoint))

# Dotted
f<-function(x1,x2) bivariate.normal(t(cbind(x1,x2)),mu,Sigma)
x.s<-seq(from=min(x1),to=max(x1),by=0.1)
vals<-f(x1=xpoint,x2=x.s)
lines(vals-abs(min(x1)),x.s,lty=2,lwd=2)

# Marginal probability distribution: http://mpdc.mae.cornell.edu/Courses/MAE714/biv-normal.pdf
# Please check this, I'm not sure it is correct.
marginal.x1<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[1,2]^2)) / (Sigma[1,2]*sqrt(2*pi))
marginal.x2<-function(x)  exp((-(x-mu[1])^2)/2*(Sigma[2,1]^2)) / (Sigma[2,1]*sqrt(2*pi))

# Left side solid
vals<-marginal.x2(x.s)
lines(vals-abs(min(x1)),x.s,lty=1,lwd=2)

# Bottom side solid
vals<-marginal.x1(x.s)
lines(x.s,vals-abs(min(x2)),lty=1,lwd=2)


dev.off()
