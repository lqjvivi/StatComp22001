## ----eval=FALSE---------------------------------------------------------------
#  function(X,d){
#    n=length(X)
#    a=numeric(n)
#    for (i in 1:n) {
#      if(X[i]-d>=0)
#        a[i]=X[i]-d
#      else
#        a[i]=0
#    }
#    return(mean(a))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  function(X,alpha,measure=c('VaR','CVaR','Distortion','Shortfall'),lambda){
#    if(measure=='VaR')
#    {VaR<-unname(quantile(X,1-alpha))
#    return(abs(VaR))
#    }
#    if(measure=='CVaR')
#    {f=function(x)
#      x+1/(1-alpha)*positive.mean(X,x)
#    CVaR=optimize(f,lower = quantile(X,1-alpha),upper = max(X))
#    return(CVaR$objective)
#    }
#    if(measure=='Distortion')
#    {
#      f=function(x)
#        quantile(X,1-x)*3*x^2
#      D=integrate(f,0,1)$value
#      return(abs(D))
#    }
#    if(measure=='Shortfall')
#    {
#      f=function(x)
#        mean(-exp(-(X+x))>=-lambda)
#      S=optimize(f,lower = 0,upper = max(X))
#      return(S$objective)
#    }
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp22001)
set.seed(1234)
X<-rnorm(100,0,4)
RM(X,0.9,measure='VaR')
RM(X,0.9,measure='CVaR')
RM(X,measure='Distortion')
RM(X,measure='Shortfall',lambda=20)

## ----eval=FALSE---------------------------------------------------------------
#  NumericMatrix gibbsC(int N, int thin) {
#    NumericMatrix mat(N, 2);
#    double x = 0, y = 0;
#    for(int i = 0; i < N; i++) {
#      for(int j = 0; j < thin; j++) {
#        x = rgamma(1, 3, 1 / (y * y + 4))[0];
#        y = rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))[0];
#      }
#      mat(i, 0) = x;
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }

