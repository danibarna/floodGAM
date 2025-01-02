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
library(xgboost)
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
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))


# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID") 
gfam[,specQ:=Qm3_s/A*1000]

# remove the id and lat/long columns
gfcov <- gfcov[,-c("RN","HN","Y_lat","X_long","Y_utm","X_utm",
                   "Y_G_Lat","X_G_Long","Y_G_UTM","X_G_UTM")]

# standardize cov values by centering and dividing by 2 standard deviations
coltab = names(gfcov)[-which(names(gfcov)=="ID")]
gfcov[, 
      (coltab) := lapply(.SD, function(Xw) (Xw - mean(Xw)) / (sd(Xw) * 2)), 
      .SDcols = coltab]

## ----- the response variable here is the median of the annual max

gfam <- gfam[,.(qind = median(specQ)),by=c("ID","d")]

gamdat <- merge(gfcov,gfam,by="ID")



# Define the data folds ---------------------------------------------------
set.seed(8)
k = 10
# use the 24 hour duration
fidx <- createFolds(gamdat[d==24,get("qind")],k) 


# Fit the GAMs on the folds and save the predicted values ------------

oos.predictions <- data.table(eta=numeric(),
                              eta.obs=numeric(),
                              mu.gam=numeric(),sigma.gam=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())
posterior.draws <- data.table(eta.draws=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())

for(di in unique(gfam[,get("d")])){
  
  gamdat.d <- gamdat[d==di]
  
  # load XGBoost hyperparameters for this duration.
  hpXGBmae = readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "xgboost-hyperparameters/","mae/",
                             "xgbpurehp_mae_",di,".rds"))
  
  for(i in 1:k){
    
    train.gamdat.d <- gamdat.d[-fidx[[i]]]
    test.gamdat.d <- gamdat.d[fidx[[i]]]
    
    ## XGBoost prep
    trainXGB.d <- xgb.DMatrix(as.matrix(train.gamdat.d[,!c("ID","d","qind")]),
                              label = log(train.gamdat.d[,get("qind")]))
    testXGB.d <- xgb.DMatrix(as.matrix(test.gamdat.d[,!c("ID","d","qind")]),
                             label = log(test.gamdat.d[,get("qind")]))
    
    
    ## ------- eta --------
    eta.floodGAM <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                          s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                        method = "REML",
                        data = train.gamdat.d,
                        family = gaussian(link=log))
    
    eta.RFFA2018 <- gam(qind ~ I(Q_N_cuberoot) + I(R_L_sqrt) + A_LE + 
                          I(T_Feb_sqrd) + I(T_Mar_cubed) + I(W_Mai_sqrt),
                        method = "REML",
                        data = train.gamdat.d,
                        family = gaussian(link=log))
    
    
    eta.xgboost <- xgb.train(data = trainXGB.d,
                                  param = hpXGBmae[[1]],
                                  nrounds = hpXGBmae[[3]],
                                  objective = "reg:squarederror")
    
    eta.datadrive <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+
                           s(A_L,k=3)+
                           s(A_P,k=6)+s(H_F,k=6)+
                           s(log_R_G_1085,k=6)+s(H_MIN,k=3)+
                           s(A_Agr,k=3)+s(A_For,k=3)+
                           s(P_Apr,k=3)+s(W_Jul,k=3),
                         method = "REML",
                         data = train.gamdat.d,
                         family = gaussian(link=log))
    
    
    ## ------- generate and save the predictions & predictive uncertainty ------
    n = dim(test.gamdat.d)[1]
    
    ## --- store the predictions
    
    ## ------------------ floodGAM
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.floodGAM,newdata=test.gamdat.d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.floodGAM$y) - eta.floodGAM$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.floodGAM,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               eta.obs = test.gamdat.d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model = rep("floodGAM",n),fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    ## save predictions for floodGAM predicting across durations:
    if(di==1|di==24){
     
      if(di == 1){
        test.across.d <- gamdat[d==24][fidx[[i]]]
        navn <- "floodGAM.1to24"
      } else{
        test.across.d <- gamdat[d==1][fidx[[i]]]
        navn <- "floodGAM.24to1"
      }
      
      oos.predictions <- rbind(oos.predictions,
                               data.table(
                                 eta = predict.gam(eta.floodGAM,
                                                   newdata=test.across.d,
                                                   type="response"),
                                 eta.obs = test.across.d[,get("qind")],
                                 mu.gam = mu.gam, sigma.gam = sigma.gam,
                                 model = rep(navn,n),fold=rep(i,n),d=rep(di,n),
                                 ID = test.across.d[,get("ID")]))
    }
    
    
    ## ------------------ RFFA2018
    mu.gam <- predict(eta.RFFA2018,newdata=test.gamdat.d,type="link")
    sigma.gam <- sd(log(eta.RFFA2018$y) - eta.RFFA2018$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.RFFA2018,
                                                 newdata = test.gamdat.d,
                                                 type="response"),
                               eta.obs = test.gamdat.d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model=rep("RFFA2018",n),fold=rep(i,n),d=rep(di,n),
                               ID=test.gamdat.d[,get("ID")]))
    
    
    ## ------------------ XGBoost, mae
    ## no mu and sigma from xgboost, so these fields are null
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = exp(predict(eta.xgboost,testXGB.d)),
                               eta.obs = test.gamdat.d[,get("qind")],
                               mu.gam = NA, sigma.gam = NA,
                               model=rep("xgboost",n),fold=rep(i,n),d=rep(di,n),
                               ID=test.gamdat.d[,get("ID")]))

    ## ------------------ auto-data-driven
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.datadrive,newdata=test.gamdat.d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.datadrive$y) - eta.datadrive$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.datadrive,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               eta.obs = test.gamdat.d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model = rep("datadrive",n),fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    
    
    ## --- store the simulations from the posterior 
    
    ## ------------------ floodGAM
    posterior.draws <- rbind(posterior.draws,
                             data.table(
                               eta.draws=simulateFromPosterior(eta.floodGAM,
                                                               "eta",
                                                               test.gamdat.d)$draws,
                               model=rep("floodGAM",n*5000),
                               fold=rep(i,n*5000),
                               d=rep(di,n*5000),
                               ID=rep(test.gamdat.d[,get("ID")],each=5000)))
    
    ## ------------------ RFFA2018
    posterior.draws <- rbind(posterior.draws,
                             data.table(
                               eta.draws=simulateFromPosterior(eta.RFFA2018,
                                                               "eta",
                                                               test.gamdat.d)$draws,
                               model=rep("RFFA2018",n*5000),
                               fold=rep(i,n*5000),
                               d=rep(di,n*5000),
                               ID=rep(test.gamdat.d[,get("ID")],each=5000)))
    
  }
}


## save the data objects oos.predictions and posterior.draws:

saveRDS(oos.predictions,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_oos_pred.rds"))
# this one has to be in gitignore because it is too large:
saveRDS(posterior.draws,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "gamfelt_hydagsupp_median_index_flood_posterior_draws.rds"))




# Quick stitch to test an alt datadriven GAM ------------------------------

iis.models <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_hydagsupp_featuresFromIIS_gam.rds"))

iis.models <- iis.models[edf>0.001]


#iis.models[,Feature:=ifelse(Feature=="R_G","log_R_G",Feature)]

# iis.models[,maediff:=c(-100,diff(maeFeat)),by=c("d","fold")]
# 
# tt <- iis.models[which(iis.models$maediff>0, arr.ind=TRUE),]
# 
# tt<- tt[,min(ordFeat),by=c("d","fold")]
# 
# tv <- merge(iis.models,tt,all.x=T,by=c("d","fold"))
# 
# tv[,V1 := lapply(.SD, nafill, fill = 50), .SDcols = "V1"]
# 
# iis.models.tt <- tv[ordFeat < V1,]



oos.predictions <- data.table(eta=numeric(),
                              eta.obs=numeric(),
                              mu.gam=numeric(),sigma.gam=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())


for(di in unique(gfam[,get("d")])){
  
  gamdat.d <- gamdat[d==di]
  
  for(i in 1:k){
    
    train.gamdat.d <- gamdat.d[-fidx[[i]]]
    test.gamdat.d <- gamdat.d[fidx[[i]]]
    
    
    ## auto-data-driven prep
    stack <- iis.models[d==di&fold==i,get("Feature")]
    rhs <- paste('s(', stack, ',k=3)', sep = '', collapse = ' + ')
    # let the first three have 6 degrees of freedom, everything else
    # gets 3 degrees of freedom:
    aa <- unlist(gregexpr(pattern ='3',rhs))[1:3]
    for(j in 1:3){substr(rhs,aa[j],aa[j]) <- "6"}
    
    
    fml <- paste('qind', '~', rhs, collapse = ' ')
    print(fml)
    
    fml <- as.formula(fml)
    
    
    ## ------- eta --------
    
    eta.auto <- gam(fml,
                    method = "REML",
                         data = train.gamdat.d,
                         family = gaussian(link=log))
    
    
    
    ## ------- generate and save the predictions & predictive uncertainty ------
    n = dim(test.gamdat.d)[1]
    
    ## --- store the predictions
    
    ## ------------------ floodGAM
    ## from the GAM: get mu (prediction for the test set on the log scale)
    mu.gam <- predict(eta.auto,newdata=test.gamdat.d,type="link")
    ## then estimate standard dev based on in-sample residuals for training data
    sigma.gam <- sd(log(eta.auto$y) - eta.auto$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.auto,
                                                 newdata=test.gamdat.d,
                                                 type="response"),
                               eta.obs = test.gamdat.d[,get("qind")],
                               mu.gam = mu.gam, sigma.gam = sigma.gam,
                               model = rep("auto",n),fold=rep(i,n),d=rep(di,n),
                               ID = test.gamdat.d[,get("ID")]))
    

    
  }
  print(paste0("******",di))
}


op1 <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "gamfelt_hydagsupp_median_flood_oos_pred.rds"))

oos.pred <- rbind(oos.predictions,op1[model!="auto"])

saveRDS(oos.pred,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "gamfelt_hydagsupp_median_flood_oos_pred.rds"))

