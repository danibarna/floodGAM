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
  ## y is fed to the function to window the grid search and speed up the approx
  ## --------------------------------------
  if(is.na(sigma)|is.na(mu)){return(NA)}
  ## calculate the normalizing constant. integral done with mathematica:
  ## input: integral_0^∞ exp(-(log(x) - μ)^2/(2 s^2))/(s sqrt(2 π)) dx
  ## output: (abs(s) e^(μ + s^2/2))/s
  A = abs(sigma)*exp(mu + sigma^2/2) / sigma
  
  ## now we have density given by g(x) = 1/A*x*f(x).
  ## approximate the integral \int_0^m g(x) dx:
  integrand <- function(x,sigma,mu,A){
    return(1/A*(1/(sigma*sqrt(2*pi)))*exp(-(log(x) - mu)^2 / (2*sigma^2)))
  }
  myintegral <- Vectorize(function(m) integrate(integrand,lower = 0,upper = m,
                                                sigma = sigma, 
                                                mu = mu, 
                                                A = A)$value)
  ## find where this integral == 0.5:
  #x <- seq(1,3500,by=0.01) # NB! this range is tied to range of data
  x <- seq(y-0.90*y,y+0.90*y,by=0.01) # NB! this assumes est is in window
  out <- myintegral(x)
  
  ## this is the predicted value for the inputted mu and sigma:
  return(x[which.min(abs(0.5-out))])
} 



optimal.predictor.ape <- function(sigma,mu,y){
  ## calculate ypred.ape, the optimal predictor for the absolute percent error.
  ## ypred.ape is med^{(-1)}(F) from Gneiting (2011).
  ## this is the median of the distribution with density proportional to f(x)/x.
  ## y is fed to the function to window the grid search and speed up the approx
  ## --------------------------------------
  start.time <- Sys.time()
  if(is.na(sigma)|is.na(mu)){return(NA)}
  ## calculate the normalizing constant. integral done with mathematica:
  ## input: integral_0^∞ exp(-(log(x) - μ)^2/(2 s^2))/(x^2 (s sqrt(2 π))) dx
  ## output: (abs(s) e^(s^2/2 - μ))/s
  A = abs(sigma)*exp(sigma^2/2-mu) / sigma
  
  ## now we have density given by g(x) = 1/A*f(x)/x.
  ## approximate the integral \int_0^m g(x) dx:
  integrand <- function(x,sigma,mu,A){
    return(1/A*(1/x^2)*(1/(sigma*sqrt(2*pi)))*exp(-(log(x) - mu)^2/(2*sigma^2)))
  }
  myintegral <- Vectorize(function(m) integrate(integrand,lower = 0,upper = m,
                                                sigma = sigma, 
                                                mu = mu, 
                                                A = A)$value)
  ## find where this integral == 0.5:
  #x <- seq(1,3500,by=0.01) # NB! this range is tied to range of data
  x <- seq(y-0.90*y,y+0.90*y,by=0.01) # NB! this assumes est is in window
  out <- myintegral(x)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  ## this is the predicted value for the inputted mu and sigma:
  return(x[which.min(abs(0.5-out))])
} 



permutationTest <- function(modelscores,modelA,modelB,n,type){
  ## From Thorarinsdottir (2020): Evaluation of CMIP5 and CMIP6...
  ms <- modelscores
  avec <- ms[model==modelA,get(type)]; bvec <- ms[model==modelB,get(type)]
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


