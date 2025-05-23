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


  iis.models <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                               "gamfelt_hydagsupp_featuresFromIIS_gam",".rds"))
  
  iis.models <- iis.models[edf>0.001]


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
    
    ## auto select prep
    stack <- iis.models[d==di&fold==i,get("Feature")]
    rhs <- paste('s(', stack, ',k=4)', sep = '', collapse = ' + ')
    # let the first three have 6 degrees of freedom, everything else
    # gets 4 degrees of freedom:
    aa <- unlist(gregexpr(pattern ='4',rhs))[1:3]
    for(j in 1:3){substr(rhs,aa[j],aa[j]) <- "6"}
    fml <- paste('qind', '~', rhs, collapse = ' ')
    fml <- as.formula(fml)

    
    
    ## ------- eta --------
    eta.floodGAM <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                          s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                        method = "REML",
                        select = T,
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
                         select = T,
                         data = train.gamdat.d,
                         family = gaussian(link=log))
    
    eta.auto <- gam(fml,
                    method = "REML",
                    select = T,
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

    ## ------------------ data-driven
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
    
    ## -------------------- full auto
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
                           "gamfelt_hydagsupp_median_flood_oos_pred","_T_", ".rds"))
# this one has to be in gitignore because it is too large:
saveRDS(posterior.draws,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "gamfelt_hydagsupp_median_index_flood_posterior_draws.rds"))





# Test floodGAM, regions --------------------------------------------------

gamdat[, c("RN", "HN") := tstrsplit(ID, "-", fixed=TRUE)]

gamdat[,RN:=as.numeric(RN)]


midsouthwest <- gamdat[(RN>=17&RN<=189)|RN==307|RN==308,]

finmarkeast <- gamdat[(RN<=16|RN>=190)&RN!=307&RN!=308,]


## define two different 5 fold cross validations
set.seed(8)
k = 5
# use the 24 hour duration
fidx.msw <- createFolds(midsouthwest[d==24,get("qind")],k)
fidx.fme <- createFolds(finmarkeast[d==24,get("qind")],k)


oos.predictions <- data.table(eta=numeric(),
                              eta.obs=numeric(),
                              mu.gam=numeric(),sigma.gam=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())


for(di in unique(gfam[,get("d")])){
  
  midsouthwest.d <- midsouthwest[d==di]
  finmarkeast.d <- finmarkeast[d==di]
  
  for(i in 1:k){
    
    train.midsouthwest.d <- midsouthwest.d[-fidx.msw[[i]]]
    test.midsouthwest.d <- midsouthwest.d[fidx.msw[[i]]]
    
    train.finmarkeast.d <- finmarkeast.d[-fidx.fme[[i]]]
    test.finmarkeast.d <- finmarkeast.d[fidx.fme[[i]]]
    
    
    ## ------- eta --------
    
    eta.midsouthwest <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                          s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                        method = "REML",
                        select = T,
                        data = train.midsouthwest.d,
                        family = gaussian(link=log))
    
    eta.finmarkeast <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                              s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                            method = "REML",
                            select = T,
                            data = train.finmarkeast.d,
                            family = gaussian(link=log))
    
    
    ## ------- generate and save the predictions & predictive uncertainty ------
    n.msw = dim(test.midsouthwest.d)[1]
    n.fme = dim(test.finmarkeast.d)[1]  
    
    ## --- store the predictions
    
    ## ------------------ floodGAM
    mu.gam.msw <- predict(eta.midsouthwest,newdata=test.midsouthwest.d,type="link")
    sigma.gam.msw <- sd(log(eta.midsouthwest$y) - eta.midsouthwest$linear.predictor)
    
    mu.gam.fme <- predict(eta.finmarkeast,newdata=test.finmarkeast.d,type="link")
    sigma.gam.fme <- sd(log(eta.finmarkeast$y) - eta.finmarkeast$linear.predictor)
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.midsouthwest,
                                                 newdata=test.midsouthwest.d,
                                                 type="response"),
                               eta.obs = test.midsouthwest.d[,get("qind")],
                               mu.gam = mu.gam.msw, sigma.gam = sigma.gam.msw,
                               model = rep("midsouthwest",n.msw),fold=rep(i,n.msw),d=rep(di,n.msw),
                               ID = test.midsouthwest.d[,get("ID")]))
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(
                               eta = predict.gam(eta.finmarkeast,
                                                 newdata=test.finmarkeast.d,
                                                 type="response"),
                               eta.obs = test.finmarkeast.d[,get("qind")],
                               mu.gam = mu.gam.fme, sigma.gam = sigma.gam.fme,
                               model = rep("finmarkeast",n.fme),fold=rep(i,n.fme),d=rep(di,n.fme),
                               ID = test.finmarkeast.d[,get("ID")]))
    
    

    
  }
  print(paste0("******",di))
}


saveRDS(oos.predictions,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "regions_median_flood_oos_pred.rds"))

