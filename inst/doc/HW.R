## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())
library(TSA)
#  Nonstationarity

## -----------------------------------------------------------------------------
data(oil.price)
plot(log(oil.price), ylab='Price per Barrel',type='l')

## ----echo=FALSE---------------------------------------------------------------
plot(diff(log(oil.price)), ylab='Price per Barrel',type='l')

## -----------------------------------------------------------------------------
knitr::kable(head(trees))

## -----------------------------------------------------------------------------
set.seed(1234)
n <- 500
a<-2
b<-2
u <- runif(n)
x <- b/(1-u)^(1/a)


## -----------------------------------------------------------------------------
hist(x, prob = TRUE) #density histogram of sample
y <- seq(0, 100, 0.01)
lines(y, 8*y^(-3)) #density curve f(x)

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
n <- 200
k <- 0 #counter for accepted
y <- numeric(n)
while (k < n) {
u <- runif(1)
x <- runif(1) #random variate from g
if (27/4*(x^2) * (1-x) > u) {
#we accept x
k <- k + 1
y[k] <- x
} }

## -----------------------------------------------------------------------------
hist(y, prob = TRUE) #density histogram of sample
d <- seq(0, 1, 0.01)
m<-dbeta(d,3,2)
lines(d,m) #density curve f(x)

## -----------------------------------------------------------------------------
set.seed(1234)
#generate a Exponential-Gamma mixture
n <- 200
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta) #lambda is random
#now supply the sample of lambda’s as the Poisson mean
x <- rgamma(n, 1,lambda) #the mixture

## -----------------------------------------------------------------------------
set.seed(1234)
#generate a Exponential-Gamma mixture
n <- 200
r <- 4
beta <- 2
lambda <- rgamma(n, r, beta) #lambda is random
#now supply the sample of lambda’s as the Poisson mean
x <- rgamma(n, 1,lambda) #the mixture
#compare with Pareto
hist(x,breaks=12,prob = TRUE)
lines(density(x))
d <- seq(0, 1000, 0.01)
lines(d,64*(2+d)^(-5),col='red')

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}


t=rep(1,100)
i=1
for(i in 1:100)
 {test<-sample(1:1e3)
  t[i]=system.time(quick_sort(test))[1]
  i=i+1}
t1=mean(t)

## ----echo=FALSE---------------------------------------------------------------
t=rep(1,100)
i=1
for(i in 1:100)
{test<-sample(1:2e3)
t[i]=system.time(quick_sort(test))[1]
i=i+1}
t2=mean(t)

t=rep(1,100)
i=1
for(i in 1:100)
{test<-sample(1:4e3)
t[i]=system.time(quick_sort(test))[1]
i=i+1}
t3=mean(t)

t=rep(1,100)
i=1
for(i in 1:100)
{test<-sample(1:6e3)
t[i]=system.time(quick_sort(test))[1]
i=i+1}
t4=mean(t)

t=rep(1,100)
i=1
for(i in 1:100)
{test<-sample(1:8e3)
t[i]=system.time(quick_sort(test))[1]
i=i+1}
t5=mean(t)

## -----------------------------------------------------------------------------
n=c(1e3,2e3,4e3,6e3,8e3)
a=c(t1,t2,t3,t4,t5)
t=n*log(n)
b0=lm(t~a)$coefficients[1]
b1=lm(t~a)$coefficients[2]
plot(a,t)
x<-seq(0, 0.25, 0.001)
lines(x,b0+b1*x)

## ----echo=FALSE,include=FALSE-------------------------------------------------
v=(exp(1)^2-1)/2-(exp(1)-1)^2
c=exp(1)-(exp(1)-1)^2
2*v+2*c
a1=v/2
a2=(v+c)/2
(a1-a2)/a1

## -----------------------------------------------------------------------------
set.seed(1234)
n=1000
u1=runif(n)
u2=runif(n)
u3=1-u1
t1=(mean(exp(u1))+mean(exp(u2)))/2
t2=(mean(exp(u1))+mean(exp(u3)))/2
se1=sd((exp(u1)+exp(u2))/2)
se2=sd((exp(u1)+exp(u3))/2)
p=(se1^2-se2^2)/se1^2
p

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
 x <- seq(1, 10, 0.01)
 y <- x^2 * exp(-x^2/2)/sqrt(2 * pi)
 plot(x, y, type = "l", ylim = c(0, 1))
 lines(x, 2*dnorm(x, 1,1), lty = 2)#Truncated Normal distribution
 lines(x, df(x - 1, 4, 4), lty = 3)#shifted F distribution
 legend("topright", inset = 0.03, legend = c("g(x)", "f_1",
  "f_2"), lty = 1:3)

## -----------------------------------------------------------------------------
plot(x, y/(2*dnorm(x, 1,1)), type = "l", lty = 2,
 ylab = "",ylim = c(0, 1))
 lines(x, y/df(x - 1, 4, 4), lty = 3)
 legend("topright", inset = 0.03, legend = c("f_1", "f_2"),
 lty = 2:3)


## -----------------------------------------------------------------------------
set.seed(1234)
 n <- 1000
 k <- 5
 m <- n/k
 t <- numeric(k)
 va <- numeric(k)
 g <- function(x) exp(-x)/(1 + x^2)
 f <- function(x) k*(1/(1 - exp(-1))) * exp(-x)
 for (j in 1:k) {
 u <- runif(m, (j-1)/k, j/k)
 x<--log(1-(1-exp(-1))*u)
 gf <- g(x)/f(x)
 t[j] <- mean(gf)
 va[j] <- var(gf)
 }

## -----------------------------------------------------------------------------
sum(t)
mean(va)

## -----------------------------------------------------------------------------
set.seed(234)
 n <- 1000
 k <- 1
 m <- n/k
 t <- numeric(k)
 va <- numeric(k)
 g <- function(x) exp(-x)/(1 + x^2)
 f <- function(x) (1/(exp(-(j-1)/k) - exp(-j/k))) * exp(-x)
 for (j in 1:k) {
 u <- runif(m, (j - 1)/k, j/k)
 x <- -log(exp(-(j-1)/k) - ((exp(-(j-1)/k) - exp(-j/k))) * u)#the inverse transform method
 gf <- g(x)/f(x)
 t[j] <- mean(gf)
 va[j] <- var(gf)
 }

## -----------------------------------------------------------------------------
sum(t)
mean(va)

## ----echo=FALSE---------------------------------------------------------------
rm(list=ls())

## -----------------------------------------------------------------------------
set.seed(123)
n<-100
m<-1000
x<-numeric(n)
y<-numeric(n)
CIL<-numeric(m)
CIU<-numeric(m)
i=1
for (i in 1:m)
{
  x=rlnorm(n)
  y=log(x)
  ybar=mean(y)
  se=sd(y)
  s=se/sqrt(n)
  CIL[i]=ybar+s*qnorm(0.025)
  CIU[i]=ybar+s*qnorm(0.975)
  i=i+1
}


## -----------------------------------------------------------------------------
mean(0>=CIL & 0<=CIU)

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}
#generate samples under H1 to estimate power
m <- 1000
sigma1 <- 1
sigma2 <- 1.5
power <- mean(replicate(m, expr={
x <- rnorm(20, 0, sigma1)
y <- rnorm(20, 0, sigma2)
count5test(x, y)
}))

## -----------------------------------------------------------------------------
print(power)

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}
#generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5

## -----------------------------------------------------------------------------
m <- 1000
sigma1 <- 1
sigma2 <- 1.5
c5<-numeric(5)
i=1
for (n in c(20,50,100,500,1000)){
 power <- mean(replicate(m, expr={
 x <- rnorm(n, 0, sigma1)
 y <- rnorm(n, 0, sigma2)
 count5test(x, y)
 }))
  print(power)
 }

## -----------------------------------------------------------------------------
m <- 1000
sigma1 <- 1
sigma2 <- 1.5
c5<-numeric(5)
i=1
for (n in c(20,50,100,500,1000)){
 Ftest <- mean(replicate(m, expr={
 x <- rnorm(n, 0, sigma1)
 y <- rnorm(n, 0, sigma2)
 Fp <- var.test(x, y)$p.value
 f <- as.integer(Fp <= 0.055)
 }))
  print(Ftest)
  }

## -----------------------------------------------------------------------------
u=(0.676-0.651)/sqrt((0.6635*(1-0.6635)))/sqrt(2/10000)
u

## -----------------------------------------------------------------------------
set.seed(123)
x1=sample(0:651,100000,replace=TRUE)
x2=676-x1
y1=651-x1
z=(x2-y1)^2/(x2+y1)
mean(z)#<3.84 
mean(z>3.84)

## -----------------------------------------------------------------------------
library(boot)
x=c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
lambda=1/mean(x);
B <- 1e4; set.seed(12345);lambdastar <- numeric(B)
for(b in 1:B){
xstar <- sample(x,replace=TRUE)
lambdastar[b] <- 1/mean(xstar)
}
lambda#original statistic
mean(lambdastar)-lambda# the bias
sd(lambdastar)#bootstrap standard error

## -----------------------------------------------------------------------------
library(boot)
x=c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
set.seed(12345)
boot.mean <- function(x,i) mean(x[i])
r <- boot(x, statistic = boot.mean, R = 3000)
r
boot.ci(r, type = c("norm", "perc", "basic", "bca"))


## -----------------------------------------------------------------------------
hist(r$t, prob = TRUE, main = "")
abline(v=r$t0,col='red',lwd=2)

## -----------------------------------------------------------------------------
mu<-0;b<-1;n<-500;m<-1e3;library(boot);set.seed(435)
boot.mean <- function(x,i) mean(x[i])
ci.norm<-ci.basic<-ci.perc<-matrix(NA,m,2)
for(i in 1:m){
R<-rnorm(n,0,1);
de <- boot(data=R,statistic=boot.mean, R = 1000)
ci <- boot.ci(de,type=c("norm","basic","perc"))
ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
ci.perc[i,]<-ci$percent[4:5];
}

## -----------------------------------------------------------------------------
mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu)

## -----------------------------------------------------------------------------
mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu)

## -----------------------------------------------------------------------------
mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu)

## -----------------------------------------------------------------------------
set.seed(123)
 library(bootstrap)
 s <- as.matrix(scor)
 n <- nrow(s)
 theta <- rep(0,n)
 m=cov(s)
 lambda <- eigen(m)$values
 theta.hat <- max(lambda)/sum(lambda)
 for (i in 1:n) {
 y <- s[-i, ]
 x <- cov(y)
 lambda <- eigen(x)$values
 theta[i] <- max(lambda)/sum(lambda)
 }
 bias <- (n - 1) * (mean(theta) - theta.hat)
 se <- sqrt((n - 1) * mean((theta - mean(theta))^2))
 
 bias
 
 se

## -----------------------------------------------------------------------------
library(DAAG, warn.conflict = FALSE)
 attach(ironslag)
 n <- length(chemical)
 N <- choose(n, 2)
 e1 <-  numeric(N)
 m <- 1
 for (i in 1:(n - 1)) 
    for (j in (i + 1):n) 
      {
       k <- c(i, j)
       y <- magnetic[-k]
       x <- chemical[-k]
       M1 <- lm(y ~ x)
       y.h1 <- M1$coef[1] + M1$coef[2] * chemical[k]
       e1[m] <- sum((magnetic[k] - y.h1)^2)
       m=m+1
 }
 mean(e1)

## -----------------------------------------------------------------------------
 n <- length(chemical)
 N <- choose(n, 2)
 e2 <-  numeric(N)
 m <- 1
 for (i in 1:(n - 1)) 
   for (j in (i + 1):n) 
     {
      k <- c(i, j)
      y <- magnetic[-k]
      x <- chemical[-k]
      M2 <- lm(y ~ x + I(x^2))
      y.h2 <- M2$coef[1] + M2$coef[2] * chemical[k] +
      M2$coef[3] * chemical[k]^2
      e2[m] <- sum((magnetic[k] - y.h2)^2)
      m=m+1
 }
 mean(e2)

## -----------------------------------------------------------------------------
 n <- length(chemical)
 N <- choose(n, 2)
 e3 <-  numeric(N)
 m <- 1
 for (i in 1:(n - 1)) 
   for (j in (i + 1):n) 
     {
       k <- c(i, j)
       y <- magnetic[-k]
       x <- chemical[-k]
       M3 <- lm(log(y) ~ x)
       logy.h3 <- M3$coef[1] + M3$coef[2] * chemical[k]
       y.h3 <- exp(logy.h3)
       e3[m] <- sum((magnetic[k] - y.h3)^2)
       m=m+1
 }
 mean(e3)

## -----------------------------------------------------------------------------
 n <- length(chemical)
 N <- choose(n, 2)
 e4 <-  numeric(N)
 m <- 1
 for (i in 1:(n - 1)) 
   for (j in (i + 1):n) 
     {
       k <- c(i, j)
       y <- magnetic[-k]
       x <- chemical[-k]
       M4 <- lm(log(y) ~ log(x))
       logy.h4 <- M4$coef[1] + M4$coef[2]*log(chemical[k])
       y.h4 <- exp(logy.h4)
       e4[m] <- sum((magnetic[k] - y.h4)^2)
       m=m+1
 }
 mean(e4)

## -----------------------------------------------------------------------------
p.spear<-function(x,y)
{
  test<-cor.test(x,y,method='spearman')
  n=length(x)
  retest<-replicate(R, expr = {
                               k <- sample(1:n)
                               cor.test(x[k], y, method =                                "spearman")$estimate
                                })
  b <- c(test$estimate, retest)
  p <- mean(as.integer(test$estimate <=
    b))
  return(list(rho.s = test$estimate, p.value = p))
}

## -----------------------------------------------------------------------------
set.seed(123)
library(MASS)
 mu <- c(0, 0)
 Sigma <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
 n <- 50
 R <- 500
 x <- mvrnorm(n, mu, Sigma)
 

## -----------------------------------------------------------------------------
cor.test(x[, 1], x[, 2], method = "spearman")
p.spear(x[, 1], x[, 2])

## -----------------------------------------------------------------------------
    set.seed(1234)
    f<-function(x){
      d=1/2*exp(-abs(x))
      return(d)
    }
    
    rw.Metropolis <- function(n, sigma, x0, N) {
        x <- numeric(N)
        x[1] <- x0
        u <- runif(N)
        k <- 0
        for (i in 2:N) {
            y <- rnorm(1, x[i-1], sigma)
                if (u[i] <= (f(y) / f(x[i-1])))
                    x[i] <- y  
                else {
                    x[i] <- x[i-1]
                    k <- k + 1
                }
            }
        return(list(x=x, k=k))
    }

    n <- 4  #degrees of freedom for target Student t dist.
    N <- 5000
    sigma <- c( .5,1,3,5)

    x0 <- 25
    rw1 <- rw.Metropolis(n, sigma[1], x0, N)
    rw2 <- rw.Metropolis(n, sigma[2], x0, N)
    rw3 <- rw.Metropolis(n, sigma[3], x0, N)
    rw4 <- rw.Metropolis(n, sigma[4], x0, N)


## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
        # psi[i,j] is the statistic psi(X[i,1:j])
        # for chain in i-th row of X
        psi <- as.matrix(psi)
        n <- ncol(psi)
        k <- nrow(psi)

        psi.means <- rowMeans(psi)     #row means
        B <- n * var(psi.means)        #between variance est.
        psi.w <- apply(psi, 1, "var")  #within variances
        W <- mean(psi.w)               #within est.
        v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
        r.hat <- v.hat / W             #G-R statistic
        return(r.hat)
        }

## -----------------------------------------------------------------------------
s <- c(0.5,1,3,5)     #parameter of proposal distribution
    n <- 4          #number of chains to generate
    N <- 6000     #length of chains
    b <- 1000       #burn-in length

    #choose overdispersed initial values
    x0 <- c(-10, -5, 5, 10)
set.seed(12345)
for(k in 1:n){
    X <- matrix(0, nrow=n, ncol=N)
    for (i in 1:n)
        X[i,] <- rw.Metropolis(n,s[k],x0[i],N)$x
   psi <- t(apply(X, 1, cumsum))
    for (i in 1:nrow(psi))
        psi[i,] <- psi[i,] / (1:ncol(psi))
      #plot the sequence of R-hat statistics
    rhat <- rep(0, N)
    for (j in (b+1):N)
        rhat[j] <- Gelman.Rubin(psi[,1:j])
    plot(rhat[(b+1):N], type="l", xlab=bquote(sigma == .(round(s[k],3))), ylab="R")
    abline(h=1.2, lty=2)
}

## -----------------------------------------------------------------------------
s <- c(0.5,1,3,5)     #parameter of proposal distribution
    n <- 4          #number of chains to generate
    N <- 6000      #length of chains
    b <- 1000       #burn-in length
   par(mfrow=c(2,2))
    x0 <- 10
set.seed(12345)
rw1 <- rw.Metropolis(n, s[1], x0, N)
    rw2 <- rw.Metropolis(n, s[2], x0, N)
    rw3 <- rw.Metropolis(n, s[3], x0, N)
    rw4 <- rw.Metropolis(n, s[4], x0, N)
    refline <- qt(c(.025, .975), df=n)
    rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)

## -----------------------------------------------------------------------------
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- .9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
x01<-mu1 #initialize
x02<-mu2
###### generate the chain #####
NG<-function(N,mu1,mu2,sigma1,sigma2,rho,x01,x02){
  s1 <- sqrt(1-rho^2)*sigma1
  s2 <- sqrt(1-rho^2)*sigma2
  X[1,]<c(x01,x02)
    for (i in 2:N) {
                    x2 <- X[i-1, 2]
                    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
                    X[i, 1] <- rnorm(1, m1, s1)
                    x1 <- X[i, 1]
                    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
                    X[i, 2] <- rnorm(1, m2, s2)
    }
  X1<-X[,1]
X2<-X[,2]
return(list(X1=X1,Y1=X2))
}

## -----------------------------------------------------------------------------
b <- burn + 1
x<-NG(N,mu1,mu2,sigma1,sigma2,rho,x01,x02)$X1
y<-NG(N,mu1,mu2,sigma1,sigma2,rho,x01,x02)$Y1
X<-x[b:N]
Y<-y[b:N]

## -----------------------------------------------------------------------------
plot(X,type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(Y,col=2,lwd=2)
legend('bottomright',c(expression(X),expression(Y)),col=1:2,lwd=2)

## -----------------------------------------------------------------------------
l=lm(Y~X)
summary(lm(Y~X))

## -----------------------------------------------------------------------------
 qqnorm(l$res, cex = 0.25)
 qqline(l$res)

## -----------------------------------------------------------------------------
 plot(l$fit, l$res, cex = 0.25)
 abline(h = 0)

## -----------------------------------------------------------------------------

N <- 6000 #length of chain
b <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- .9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
x01<-c(-1.5,-0.5,0,1,2) #initialize
x02<-c(-1.5,-0.5,0,1,2)
n<-5
    #choose overdispersed initial values
set.seed(23456)
    x <- matrix(0, nrow=n, ncol=N)
    y <- matrix(0, nrow=n, ncol=N)
    for (i in 1:n)
       { 
      x[i,] <- NG(N,mu1,mu2,sigma1,sigma2,rho,x01[i],x02[i])$X1
      y[i,] <- NG(N,mu1,mu2,sigma1,sigma2,rho,x01[i],x02[i])$Y1}
   psi1 <- t(apply(x, 1, cumsum))
   psi2 <- t(apply(y, 1, cumsum))
    for (i in 1:nrow(psi1))
        {psi1[i,] <- psi1[i,] / (1:ncol(psi1))
         psi2[i,] <- psi2[i,] / (1:ncol(psi2))
         }
      #plot the sequence of R-hat statistics
    rhat1 <- rep(0, N)
    rhat2 <- rep(0, N)
    for (j in (b+1):N)
        {rhat1[j] <- Gelman.Rubin(psi1[,1:j])
        rhat2[j] <- Gelman.Rubin(psi2[,1:j])
    }
    plot(rhat1[(b+1):N], type="l", xlab='X', ylab="R")
    abline(h=1.2, lty=2)
    plot(rhat2[(b+1):N], type="l", xlab='Y', ylab="R")
    abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
N <- 6000 #length of chain
b <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
rho <- .9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
x2 <- NG(N,mu1,mu2,sigma1,sigma2,rho,0,0)$X1
y2 <- NG(N,mu1,mu2,sigma1,sigma2,rho,0,0)$Y1
    refline <- qt(c(.025, .975), df=n)
    c1<-x2[(b+1):N]
        plot(c1, type="l",
             ylab="X", ylim=range(c1))
        abline(h=refline)
        
    c2<-y2[(b+1):N]
    plot(c2, type="l",
             ylab="Y", ylim=range(c2))
        abline(h=refline)

## -----------------------------------------------------------------------------
library(mediation)

## -----------------------------------------------------------------------------
set.seed(2345)
N=50
em<-rnorm(N)
ey<-rnorm(N)
X<-rnorm(N)
r=1
am=1
ay=2

per <- function(z, ix, dims) {
p <- dims[1] 
q <- dims[2] 
d <- p + q 
x <- z[ 1:p,] 
y <- z[-(1:p),ix] 
return(c(X1=x,Y1=y)) }

#for only X independent with M;
perm1<-function(X,M,Y,n){
  a<-lm(M~X)
  b<-lm(Y~M+X)
  re=mediate(a,b,treat='X',mediator='M',sims=20)
  c=re$d0
  se=sd(re$d0.sim)
  T0=c/se
  i=1
  t<-numeric(n)
  for(i in 1:n){
    X<- sample(X);
    k <- sample(1:length(Y))
    Y <- Y[k]; 
    M <- M[k];
    a<-lm(M~X)
    b<-lm(Y~M+X)
    re=mediate(a,b,treat='X',mediator='M',sims=20)
    c=re$d0
    se=sd(re$d0.sim)
    t[i]=c/se
  }
  p=mean(abs(c(T0,t))>=abs(T0))
  return(p)
}


#for only M independent with Y;
perm2<-function(X,M,Y,n){
  a<-lm(M~X)
  b<-lm(Y~M+X)
  re=mediate(a,b,treat='X',mediator='M',sims=20)
  c=re$d0
  se=sd(re$d0.sim)
  T0=c/se
  i=1
  t<-numeric(n)
  for(i in 1:n){
    k <- sample(1:length(X))
    X <- X[k]; 
    M <- M[k];
    Y<- sample(Y);
    a<-lm(M~X)
    b<-lm(Y~M+X)
    re=mediate(a,b,treat='X',mediator='M',sims=100)
    c=re$d0
    se=sd(re$d0.sim)
    t[i]=c/se
  }
  p=mean(abs(c(T0,t))>=abs(T0))
  return(p)
}


#for X independent with M;M independent with y
perm3<-function(X,M,Y,n){
  a<-lm(M~X)
  b<-lm(Y~M+X)
  re=mediate(a,b,treat='X',mediator='M',sims=20)
  c=re$d0
  se=sd(re$d0.sim)
  T0=c/se
  i=1
  t<-numeric(n)
  for(i in 1:n){
    X<- sample(X);
    M<-M
    Y<-sample(Y);
    a<-lm(M~X)
    b<-lm(Y~M+X)
    re=mediate(a,b,treat='X',mediator='M',sims=20)
    c=re$d0
    se=sd(re$d0.sim)
    t[i]=c/se
  }
  p=mean(abs(c(T0,t))>=abs(T0))
  return(p)
}




## -----------------------------------------------------------------------------
n=100
alpha=0;beta=1;
M<-am+alpha*X+em
Y<-ay+beta*M+r*X+ey
p1=perm1(X,M,Y,n)#model1
p2=perm2(X,M,Y,n)#model2
p3=perm3(X,M,Y,n)#model3
c(p1,p2,p3)

## -----------------------------------------------------------------------------
n=50
alpha=1;beta=0;
M<-am+alpha*X+em
Y<-ay+beta*M+r*X+ey
p1=perm1(X,M,Y,n)#model1
p2=perm2(X,M,Y,n)#model2
p3=perm3(X,M,Y,n)#model3
c(p1,p2,p3)

## -----------------------------------------------------------------------------
n=50
alpha=0;beta=0;
M<-am+alpha*X+em
Y<-ay+beta*M+r*X+ey
p1=perm1(X,M,Y,n)#model1
p2=perm2(X,M,Y,n)#model2
p3=perm3(X,M,Y,n)#model3
c(p1,p2,p3)

## -----------------------------------------------------------------------------
set.seed(12345)
x1 <- rpois(N,1);x2<-rexp(N,1); x3 <- rbinom(N,1,0.5)
s<-function(x1,x2,x3,N,f0){
g <- function(alpha){
tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
p <- 1/(1+tmp)
mean(p) - f0
}
solution <- uniroot(g,c(-15,5))
round(unlist(solution),5)[1:3]
return(solution$root)
}

## -----------------------------------------------------------------------------
N <- 1e6; b1 <- 0; b2 <- 1;b3<- -1;
alpha<-numeric(4)
f1<-c(0.1,0.01,0.001,0.0001)
i=1
for(i in 1:4)
{
  f0<-f1[i]
alpha[i] <- s(b1,b2,b3,N,f0)
i=i+1
}
alpha

## -----------------------------------------------------------------------------
plot(f1,alpha,xlab='f0')
plot(-log(f1),alpha,xlab='-log(f0)')

## -----------------------------------------------------------------------------
library(stats4)
u=c(11,8,27,13,16,0,23,10,24,2)
v=c(12,9,28,14,17,1,24,11,25,3)
lo<-function(lambda){
  sum=0
  i=1
  for(i in 1:10){
    sum=sum+(v[i]*exp(-lambda*v[i])-u[i]*exp(-lambda*u[i]))/(exp(-lambda*u[i])-exp(-lambda*v[i]))
    i=i+1
  }
  sum
}
sol <- uniroot(lo,c(0,5))
sol

## -----------------------------------------------------------------------------
lambda0=1
e=1
n=10

while (e>10^(-6)){
  lambda=n/(-lo(lambda0)+n/lambda0)
  e=abs(lambda-lambda0)
  lambda0=lambda
}
lambda0

## -----------------------------------------------------------------------------
rm(list=ls()) 

## -----------------------------------------------------------------------------
iris[FALSE,]#0 rows

iris[FALSE,]#0 columns

iris[FALSE,FALSE]#0 columns and 0 rows


## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
car<-data.frame(lapply(cars, function(x) if (is.numeric(x)) scale01(x) else x))
head(car)
tail(car)

## -----------------------------------------------------------------------------
rm(list=ls()) 

## -----------------------------------------------------------------------------
vapply(trees, sd, numeric(1))

## -----------------------------------------------------------------------------
vapply(iris[vapply(iris, is.numeric, logical(1))],
       sd, 
       numeric(1))

## -----------------------------------------------------------------------------
rm(list=ls()) 

## -----------------------------------------------------------------------------
library(Rcpp)

## -----------------------------------------------------------------------------
gibbsR <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rnorm(1, 0.9*y, sqrt(1-0.9^2))
      y <- rnorm(1, 0.9*x, sqrt(1-0.9^2))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}

## -----------------------------------------------------------------------------
set.seed(2345)
library(microbenchmark)
library(StatComp22001)
gibbR=gibbsR(200,1)
gibbC=gibbsC(200,1)
X1=gibbR[,1]
Y1=gibbR[,2]
X2=gibbC[,1]
Y2=gibbC[,2]
qqplot(X1,Y1)
qqplot(X2,Y2)
qqplot(X1,X2)
qqplot(Y1,Y2)


## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(gibbR=gibbsR(200,1),
gibbC=gibbsC(200,1))
summary(ts)[,c(1,3,5,6)]

