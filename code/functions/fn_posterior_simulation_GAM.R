simulateFromPosterior <- function(b,param,testdata){
  # Simulate from the posterior of the predicted value (using "asymptotic
  # bootstrap"; relies on asymptotic normality of the estimates rather
  # than the data).
  # 
  # See sec 6.10 in Wood (2017) 'Generalized Additive Models: An Introduction
  # With R'.
  # 
  # If the predicted value is eta, need to exponentially transform the 
  # estimates to get them on the response scale bc of the log link
  # -------------------------------
  # b - (mgcv object) fitted GAM
  # param - (character) name of parameter being predicted (eta, beta or xi)
  # testdata - (data.table) - test data to predict out of sample
  # -------------------------------
  # output: data.table 'posterior.draws'
  #
  #         draws - 5000 draws from the posterior for each station
  #         ID - station ID
  # --------------------------------------------------
  
  ## first get prediction matrix:
  Xp <- predict(b, newdata=testdata, type="lpmatrix")
  
  ## then simulate from posterior distribution of parameters
  br1 <- rmvn(n=5000,coef(b),vcov(b,unconditional = T))
  ## where unconditional = T adds smoothing parameter uncertainty correction
  
  ## compute the desired quantity:
  draws <- Xp%*%t(br1)
  
  ## if param = eta, use exp(.) to get draws on the response scale:
  if(param == "eta"){ draws <- exp(draws) }
  
  posterior.draws <- data.table(draws=as.vector(t(draws)),
                                ID=rep(testdata[,get("ID")],each=5000))
  return(posterior.draws)
}