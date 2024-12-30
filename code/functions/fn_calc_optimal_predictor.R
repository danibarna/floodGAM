##
##
##
##
##
##
## 
##
## functions to calculate:
##  - optimal predictor value for relative error and absolute percent error
##    These are taken from Gneiting (2011)
##
##  - permutation test implementation
##
##  - output of permutation test as table for all models and metrics
##
## -----------------------------------------------------------------------------

optimal.predictor.re <- function(sigma,mu,y){
  ## calculate ypred.re, the optimal predictor for the relative error.
  ## ypred.re is med^{(1)}(F) from Gneiting (2011).
  ## this is the median of the distribution with density proportional to xf(x).
  ## y = ypred and is fed to the function to window the grid search 
  ## and speed up the approx.
  ## --------------------------------------
  if(is.na(sigma)|is.na(mu)){return(NA)}
  ## calculate the normalizing constant. integral done with mathematica:
  ## input: integral_0^∞ exp(-(log(x) - μ)^2/(2 s^2))/(s sqrt(2 π)) dx
  ## output: (abs(s) e^(μ + s^2/2))/s
  A = abs(sigma)*exp(mu + sigma^2/2) / sigma
  
  ## now we have density given by g(x) = 1/A*x*f(x).
  ## approximate the integral \int_0^m g(x) dx:
  gx <- function(x,mu,sigma,A){
    return( 1/A*(1/(sigma*sqrt(2*pi)))*exp(-(log(x) - mu)^2 / (2*sigma^2)) )
  }
  
  ## find where this integral == 0.5.
  ## Give optimize an interval between 0 and 5 times the predicted value.
  ## This upper limit is important to stop optimize from wandering into
  ## the long flat region of the function (beyond any reasonable values)
  ## Because of this, should always manually check the large relative errors
  med <- optimize(function(m) abs(integrate(gx, lower = 0, upper = m,
                                            mu = mu,
                                            sigma = sigma,
                                            A = A)$value - 0.5),
                  interval = c(0,5*y))$minimum
  
  return(med)
} 


optimal.predictor.ape <- function(sigma,mu,y){
  ## calculate ypred.ape, the optimal predictor for the absolute percent error.
  ## ypred.ape is med^{(-1)}(F) from Gneiting (2011).
  ## this is the median of the distribution with density proportional to f(x)/x.
  ## y = ypred and is fed to the function to window the grid search 
  ## and speed up the approx.
  ## --------------------------------------
  start.time <- Sys.time()
  if(is.na(sigma)|is.na(mu)){return(NA)}
  ## calculate the normalizing constant. integral done with mathematica:
  ## input: integral_0^∞ exp(-(log(x) - μ)^2/(2 s^2))/(x^2 (s sqrt(2 π))) dx
  ## output: (abs(s) e^(s^2/2 - μ))/s
  A = abs(sigma)*exp(sigma^2/2-mu) / sigma
  
  ## now we have density given by g(x) = 1/A*x*f(x).
  ## approximate the integral \int_0^m g(x) dx:
  gx <- function(x,mu,sigma,A){
    return(1/A*(1/x^2)*(1/(sigma*sqrt(2*pi)))*exp(-(log(x) - mu)^2/(2*sigma^2)))
  }
  
  ## find where this integral == 0.5.
  ## Give optimize an interval between 0 and 5 times the predicted value.
  ## This upper limit is important to stop optimize from wandering into
  ## the long flat region of the function (beyond any reasonable values)
  ## Because of this, should always manually check the large percent errors
  med <- optimize(function(m) abs(integrate(gx, lower = 0, upper = m,
                                            mu = mu,
                                            sigma = sigma,
                                            A = A)$value - 0.5),
                  interval = c(0,5*y))$minimum
  
  return(med)
} 



permutationTest <- function(ms,modelA,modelB,n,type,di){
  ## From Thorarinsdottir (2020): Evaluation of CMIP5 and CMIP6...
  avec <- ms[model==modelA&d==di,get(type)] 
  bvec <- ms[model==modelB&d==di,get(type)]
  svec = avec-bvec
  permvec = rep(NA,n)
  for(i in 1:n){
    nvec <- sample(c(1,-1),length(avec),replace=T)
    permvec[i] <- mean(nvec*svec)
  }
  s = mean(svec)
  percentile <- ecdf(permvec)
  quantS <- percentile(s)
  return(quantS)
}


