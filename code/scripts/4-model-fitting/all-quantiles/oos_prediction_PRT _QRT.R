##
##
##
##
##
## 
## 
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)
library(caret)

## ----- source small function to simulate from the posterior of the
## predicted parameter values:
source("~/floodGAM/code/functions/fn_posterior_simulation_GAM.R")


# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
# response variable is from at-site GEV fits
gevp <- readRDS("~/floodGAM/results/output/all-quantiles/gamfeltstanresult.rds")

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID + d ~ param, value.var = "mean")

# merge the response variable with the predictors (gamfelt covariate matrix)
gamdat <- merge(gevp,gfcov,by="ID")


# Define the data folds ---------------------------------------------------
set.seed(8)
k = 10
# use the 24 hour duration
fidx <- createFolds(gamdat[d==24,get("qind")],k)


# Fit the GAMs on the folds and save the predicted values ------------

prt.predictions <- data.table(val=numeric(), # the predicted value
                              obs.val=numeric(), # the at-site freq param value
                              param=character(), # indicator column for which parameter is predicted (eta,beta,xi)
                              mu.gam=numeric(),sigma.gam=numeric(), # here mu and sigma are from the predictive distribution (not the GEV distribution)
                              fold=numeric(),d=numeric(),
                              ID=character())

qrt.predictions <- data.table(val=numeric(), 
                              obs.val=numeric(), # return level from at-site freq analysis
                              rp=numeric(), # indicator column for which return period is predicted
                              mu.gam=numeric(),sigma.gam=numeric(), 
                              fold=numeric(),d=numeric(),
                              ID=character())

for(di in dd){
  
  gamdat.d <- gamdat[d==di]
  
  
  for(i in 1:k){
    
    train.gamdat.d <- gamdat.d[-fidx[[i]]]
    test.gamdat.d <- gamdat.d[fidx[[i]]]
    
    n = dim(test.gamdat.d)[1]
    
    ######################################
    ## PARAMETER REGRESSION TECHNIQUE
    ######################################

    # Eta parameter -----------------------------------------------------------
    eta.gam <- gam(qind ~ s(Q_N,k=6)+
                 s(A_LE,k=6)+
                 s(A_P,k=6)+
                 s(H_F,k=6)+
                 s(log_R_G_1085,k=6)+
                 s(W_Apr,k=3)+s(P_Sep,k=3),
               method = "REML",
               data = train.gamdat.d,
               family = gaussian(link=log))
    
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.gam,newdata=test.gamdat.d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.gam$y) - eta.gam$linear.predictor)
    
    prt.predictions <- rbind(prt.predictions,
                             data.table(
                               val = predict.gam(eta.gam,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("qind")],
                               param = rep("eta",n),
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    

    # Beta parameter ----------------------------------------------------------
    beta.gam <- gam(beta ~ s(Q_N,k=6)+
                  s(log_R_G_1085,k=6)+
                  s(P_Sep,k=6),
                method = "REML",
                data = train.gamdat.d,
                family = gaussian(link=identity))
    
    mu.gam.beta <- predict(beta.gam,newdata=test.gamdat.d,type="link")
    sigma.gam.beta <- sd(beta.gam$y - beta.gam$linear.predictor)
    
    prt.predictions <- rbind(prt.predictions,
                             data.table(
                               val = predict.gam(beta.gam,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("beta")],
                               param = rep("beta",n),
                               mu.gam = mu.gam.beta, sigma.gam = sigma.gam.beta,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    

    # Xi parameter ------------------------------------------------------------
    xi.gam <- gam(xi ~ s(A_LE,k=6)+
                s(log_R_G_1085,k=6) +
                s(W_Apr,k=6),
              method = "REML",
              data = train.gamdat.d,
              family = gaussian(link=identity))
    
    mu.gam.xi <- predict(xi.gam,newdata=test.gamdat.d,type="link")
    sigma.gam.xi <- sd(xi.gam$y - xi.gam$linear.predictor)
    
    prt.predictions <- rbind(prt.predictions,
                             data.table(
                               val = predict.gam(xi.gam,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("xi")],
                               param = rep("xi",n),
                               mu.gam = mu.gam.xi, sigma.gam = sigma.gam.xi,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    
    ######################################
    ## QUANTILE REGRESSION TECHNIQUE
    ######################################
    
    # for loop over quantiles 2-1050
    
    # convert to rl
    
    for(rp in c(1.5,2:9,seq(10,1050,by=10))){ #seq of return periods (years)
      
      # convert to the rp-year return level:
      qrt.test.gamdat.d <- test.gamdat.d[,rl:=mu+sigma/xi*((-log(1-1/rp))^(-xi)-1)]
      qrt.train.gamdat.d <- train.gamdat.d[,rl:=mu+sigma/xi*((-log(1-1/rp))^(-xi)-1)]
      
      # fit floodGAM on this return level:
      
      qrt.gam <- gam(rl ~ s(Q_N,k=6)+
                       s(A_LE,k=6)+
                       s(A_P,k=6)+
                       s(H_F,k=6)+
                       s(log_R_G_1085,k=6)+
                       s(W_Apr,k=3)+s(P_Sep,k=3),
                     method = "REML",
                     data = qrt.train.gamdat.d,
                     family = gaussian(link=log))
      
      mu.gam.qrt <- predict(qrt.gam,newdata=qrt.test.gamdat.d,type="link")
      sigma.gam.qrt <- sd(log(qrt.gam$y) - qrt.gam$linear.predictor)
      
      qrt.predictions <- rbind(qrt.predictions,
                               data.table(
                                 val = predict.gam(qrt.gam,
                                                   newdata=qrt.test.gamdat.d,
                                                   type="response"),
                                 obs.val = qrt.test.gamdat.d[,get("rl")],
                                 rp = rep(rp,n),
                                 mu.gam = mu.gam.qrt, sigma.gam = sigma.gam.qrt,
                                 fold=rep(i,n),d=rep(di,n),
                                 ID = test.gamdat.d[,get("ID")]))
      
    }
    
  }

}


save(prt.predictions,qrt.predictions,
     file="~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")






