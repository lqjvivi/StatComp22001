#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description  the useage of C functions \code{gibbsC} .
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' gibbC=gibbsC(200,1)
#' ts <- microbenchmark(gibbR=gibbsR(200,1),
#'gibbC=gibbsC(200,1))
#'summary(ts)[,c(1,3,5,6)]
#' }
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm 
#' @useDynLib StatComp22001
NULL

#' @title Calculate positive mean  using R.
#' @description the mean of X-d 's positive part.
#' @param X the observed data (numeric)
#' @param d the deductible (numeric)
#' @return the mean of X-d 's positive part\code{n}
#' @examples
#' \dontrun{
#' set.seed(123)
#' X<-rnorm(100)
#' positive.mean(X,3)
#' }
#' @export
positive.mean=function(X,d){
  n=length(X)
  a=numeric(n)
  for (i in 1:n) {
    if(X[i]-d>=0)
      a[i]=X[i]-d
    else
      a[i]=0
  }
  return(mean(a))
}



#' @title Risk measure.
#' @description The function can be used to calculate the risk measure such as 'VaR','CVaR','Distortion','Shortfall'.
#' @param X the Observed data (numeric)
#' @param alpha the confidence level (numeric)
#' @param measure the type of risk measure with 'VaR','CVaR','Distortion','Shortfall'  (logical)
#' @param lambda the risky parameter(numeric)
#' @return a random sample of size \code{n}
#' @importFrom stats integrate optimize quantile
#' @examples
#' \dontrun{
#' set.seed(1234)
#'X<-rnorm(100,0,4)
#'RM(X,0.9,measure='VaR')
#'RM(X,0.9,measure='CVaR')
#'RM(X,measure='Distortion')
#'RM(X,measure='Shortfall',lambda=20)
#' }
#' @export
RM<-function(X,alpha,measure=c('VaR','CVaR','Distortion','Shortfall'),lambda){
  if(measure=='VaR')
  {VaR<-unname(quantile(X,1-alpha))
  return(abs(VaR[2]))
  }
  if(measure=='CVaR')
  {f=function(x)
    x+1/(1-alpha)*positive.mean(X,x)
  CVaR=optimize(f,lower = quantile(X,1-alpha),upper = max(X))
  return(CVaR$objective)
  }
  if(measure=='Distortion')
  {
    f=function(x)
      quantile(X,1-x)*3*x^2
    D=integrate(f,0,1)$value
    return(abs(D))
  }
  if(measure=='Shortfall')
  {
    f=function(x)
      mean(-exp(-(X+x))>=-lambda)
    S=optimize(f,lower = 0,upper = max(X))
    return(S$objective)
  }
}

