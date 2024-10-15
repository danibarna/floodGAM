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
library(ggplot2)

## ----- source small function to simulate from the posterior of the
## predicted parameter values:
source("~/floodGAM/code/functions/fn_posterior_simulation_GAM.R")


# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "durations_gamfelt_annual_maxima.rds"))

gfam0 <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                       "gamfelt_annual_maxima.rds"))
gfam0[,d:=0]
gfam <- rbind(gfam0[,c("ID","d","Qm3_s","decimaldate","year_key")], gfam[d!=0,])
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# select only the floodGAM & RFFA_2018 covariates & plotting things
gfcov <- gfcov[,c("ID","Q_N","A_LE","A_P","H_F", #floodGAM
                  "R_G_1085","log_R_G_1085","W_Apr","P_Sep", #floodGAM
                  "Q_N_cuberoot","R_L_sqrt","T_Feb_sqrd", #RFFA_2018 eta
                  "T_Mar_cubed","W_Mai_sqrt", #RFFA_2018 eta
                  "A_Glac","A_For","H_10","P_Jul","W_Jun", #RFFA_2018 beta
                  "R_TL_net",#RFFA_2018 xi
                  "A","QD_fgp")] # for plotting 

# standardize cov values by centering and dividing by 2 standard deviations
coltab = names(gfcov)[-1]
gfcov[, 
      (coltab) := lapply(.SD, function(Xw) (Xw - mean(Xw)) / (sd(Xw) * 2)), 
      .SDcols = coltab]

## ----- the response variable here is the median of the annual max

gfam <- gfam[,.(qind = median(specQ)),by=c("ID","d")]

gamdat <- merge(gfcov,gfam,by="ID")



# Define the data folds ---------------------------------------------------
set.seed(42)
k = 10
# use the median to sort the stations into folds
fidx <- createFolds(gamdat[d==0,get("qind")],k) 


# Fit the GAMs on the folds and save the predicted values ------------

oos.predictions <- data.table(eta=numeric(),
                              eta.obs=numeric(),
                              mu.gam=numeric(),sigma.gam=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())
posterior.draws <- data.table(eta.draws=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())

for(di in unique(gamdat[,get("d")])){
  
  gamdat_d <- gamdat[d==di]
  
  for(i in 1:k){
    
    train.gamdat_d <- gamdat_d[-fidx[[i]]]
    test.gamdat_d <- gamdat_d[fidx[[i]]]
    
    ## ------- eta --------
    eta.floodGAM <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                          s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                        method = "REML",
                        data = gamdat_d,
                        family = gaussian(link=log))
    
    eta.RFFA2018 <- gam(qind ~ I(Q_N_cuberoot) + I(R_L_sqrt) + A_LE + 
                          I(T_Feb_sqrd) + I(T_Mar_cubed) + I(W_Mai_sqrt),
                        method = "REML",
                        data = gamdat_d,
                        family = gaussian(link=log))
    
    ## ------- generate and save the predictions & predictive uncertainty ------
    n = dim(test.gamdat_d)[1]
    
    ## --- predictions
    
    ## ------------------ floodGAM
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.floodGAM,newdata=test.gamdat_d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.floodGAM$y) - eta.floodGAM$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.floodGAM,
                                                 newdata=test.gamdat_d,
                                                 type="response"),
                               eta.obs = test.gamdat_d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model = rep("floodGAM",n),fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat_d[,get("ID")]))
    
    ## ------------------ RFFA2018
    mu.gam <- predict(eta.RFFA2018,newdata=test.gamdat_d,type="link")
    sigma.gam <- sd(log(eta.RFFA2018$y) - eta.RFFA2018$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.RFFA2018,
                                                 newdata = test.gamdat_d,
                                                 type="response"),
                               eta.obs = test.gamdat_d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model=rep("RFFA2018",n),fold=rep(i,n),d=rep(di,n),
                               ID=test.gamdat_d[,get("ID")]))
    
    ## --- simulate from the posterior 
    
    ## ------------------ floodGAM
    posterior.draws <- rbind(posterior.draws,
                             data.table(
                               eta.draws=simulateFromPosterior(eta.floodGAM,
                                                               "eta",
                                                               test.gamdat_d)$draws,
                               model=rep("floodGAM",n*5000),
                               fold=rep(i,n*5000),
                               d=rep(di,n*5000),
                               ID=rep(test.gamdat_d[,get("ID")],each=5000)))
    
    ## ------------------ RFFA2018
    posterior.draws <- rbind(posterior.draws,
                             data.table(
                               eta.draws=simulateFromPosterior(eta.RFFA2018,
                                                               "eta",
                                                               test.gamdat_d)$draws,
                               model=rep("RFFA2018",n*5000),
                               fold=rep(i,n*5000),
                               d=rep(di,n*5000),
                               ID=rep(test.gamdat_d[,get("ID")],each=5000)))
    
  }
}







