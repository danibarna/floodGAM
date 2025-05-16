##
##
##
##
##
## 
## OOS prediction on same folds as floodGAM (see 'oos_prediction_PRT_QRT.R')
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


dd <- unique(gamdat[,get("d")])


# Fit RFFA2018 on the folds and save the predicted values ------------

rffa.predictions <- data.table(val=numeric(), # the predicted value
                              obs.val=numeric(), # the at-site freq param value
                              param=character(), # indicator column for which parameter is predicted (eta,beta,xi)
                              mu.gam=numeric(),sigma.gam=numeric(), # here mu and sigma are from the predictive distribution (not the GEV distribution)
                              fold=numeric(),d=numeric(),
                              ID=character())

posterior.draws <- data.table(eta.draws=numeric(),
                              beta.draws=numeric(),
                              xi.draws=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())


for(di in dd){
  
  gamdat.d <- gamdat[d==di]
  
  
  for(i in 1:k){
    
    train.gamdat.d <- gamdat.d[-fidx[[i]]]
    test.gamdat.d <- gamdat.d[fidx[[i]]]
    
    n = dim(test.gamdat.d)[1]
    
    ######################################
    ## RFFA2018
    ######################################
    
    # Eta parameter -----------------------------------------------------------
    eta.RFFA2018 <- gam(qind ~ I(Q_N_cuberoot) + I(R_L_sqrt) + A_LE + 
                          I(T_Feb_sqrd) + I(T_Mar_cubed) + I(W_Mai_sqrt),
                        method = "REML",
                        data = train.gamdat.d,
                        family = gaussian(link=log))
    
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.RFFA2018,newdata=test.gamdat.d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.RFFA2018$y) - eta.RFFA2018$linear.predictor)
    
    rffa.predictions <- rbind(rffa.predictions,
                             data.table(
                               val = predict.gam(eta.RFFA2018,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("qind")],
                               param = rep("eta",n),
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    
    
    # Beta parameter ----------------------------------------------------------
    beta.RFFA2018 <- gam(beta ~ I(A_Glac) + I(A_For) + I(H_10) + 
                           I(P_Jul) + I(W_Jun),
                         method = "REML",
                    data = train.gamdat.d,
                    family = gaussian(link=identity))
    
    mu.gam.beta <- predict(beta.RFFA2018,newdata=test.gamdat.d,type="link")
    sigma.gam.beta <- sd(beta.RFFA2018$y - beta.RFFA2018$linear.predictor)
    
    rffa.predictions <- rbind(rffa.predictions,
                             data.table(
                               val = predict.gam(beta.RFFA2018,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("beta")],
                               param = rep("beta",n),
                               mu.gam = mu.gam.beta, sigma.gam = sigma.gam.beta,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    
    # Xi parameter ------------------------------------------------------------
    xi.RFFA2018 <- gam(xi ~ I(A_LE) + I(R_TL_net),
                       method = "REML",
                  data = train.gamdat.d,
                  family = gaussian(link=identity))
    
    mu.gam.xi <- predict(xi.RFFA2018,newdata=test.gamdat.d,type="link")
    sigma.gam.xi <- sd(xi.RFFA2018$y - xi.RFFA2018$linear.predictor)
    
    rffa.predictions <- rbind(rffa.predictions,
                             data.table(
                               val = predict.gam(xi.RFFA2018,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               obs.val = test.gamdat.d[,get("xi")],
                               param = rep("xi",n),
                               mu.gam = mu.gam.xi, sigma.gam = sigma.gam.xi,
                               fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    
    ## ------- generate and save the predictions & predictive uncertainty ------
    
    posterior.draws <- rbind(posterior.draws,
                             data.table(
                               eta.draws=simulateFromPosterior(eta.RFFA2018,"eta",test.gamdat.d)$draws,
                               beta.draws=simulateFromPosterior(beta.RFFA2018,"beta",test.gamdat.d)$draws,
                               xi.draws=simulateFromPosterior(xi.RFFA2018,"xi",test.gamdat.d)$draws,
                               model=rep("RFFA2018",n*5000),
                               fold=rep(i,n*5000),
                               d=rep(di,n*5000),
                               ID=rep(test.gamdat.d[,get("ID")],each=5000)))
    
    
  }
  
}


save(rffa.predictions,posterior.draws,
     file="~/floodGAM/results/output/all-quantiles/rffa2018-oos.rda")






