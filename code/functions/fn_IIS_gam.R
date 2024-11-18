##
##
##
##
##
## Functions for 
## Iterative Input Selection algorithm with XGBoost as base learner
##
## 
## -----------------------------------------------------------------------------

## 1. Outer (wrapper) model: rank the features 
## we use the embedded feature scores of xgboost 
## this step of the algorithm is always trained on *all* features. 
## for robustness: compute variable ranking over 25 bootstrap samples.
##                 Final variable ranking is the average gain over 25 samples.
variable_ranking <- function(y,X,
                             idealnrounds,tunedhyperparams){
  # computes variable ranking
  # ranking method: XGBoost
  # ----
  # outputs a [n x 2] matrix where n = number of features
  # here 'bGain' is average gain over 25 bootstrap samples
  # ---------------
  # y = response variable
  # X = feature matrix
  # idealnrounds,tunedhyperparams = hyperparameters for xgboost
  # ----------------------------------------
  set.seed(42)
  boot_importance <- data.table(Feature=character(),Gain=numeric(),
                                Cover=numeric(),Frequency=numeric())

  for(bootiter in 1:25){
    idx <- sample(1:nrow(X),0.682*nrow(X),replace=F)
    B <- X[idx,]
    y_b <- y[idx]
    bdat = xgb.DMatrix(data = as.matrix(B), 
                       label = y_b)
    obj <- xgb.train(data = bdat,
                     param = tunedhyperparams,
                     nrounds = idealnrounds,
                     objective = "reg:squarederror")
    importance <- xgb.importance(model = obj)
    boot_importance <- rbind(boot_importance,importance)
  }
  boot_importance <- boot_importance[,mean(Gain),by=Feature]
  setorder(boot_importance,-V1); setnames(boot_importance,"V1","bGain")
  
  return(boot_importance)
}


## 2. Inner model: evaluate predictive performance 
## using p (user-chosen number) features from step 1
##   following Galelli & Castelletti (2013) 
##   this also called MISO (multi-input single output) step
miso <- function(y,X,boot_importance,fidx,k,p,stack){
  # proposes a parameter to add to stack
  # inner model: varies ((running w xgboost as of 20.01.2023))
  # ----
  # outputs the new feature to add to stack and newD
  # newD is MAE computed over stack + newFeature
  # ---------------
  # y = response variable
  # X = feature matrix
  # boot_importance = matrix w. average gain for each feature
  # k = number of folds in cross-validation
  # p = we evaluate the top p features in this step (in order of avg gain)
  # stack = current stack of chosen features
  # idealnrounds,tunedhyperparams = hyperparameters for xgboost
  # ----------------------------------------
  Xyr <- boot_importance[1:p,Feature] #these are the features we will eval
  
  # The estimated prediction accuracy of a specific feature 
  # is the average value of the MAE computed over k folds
  DyHat <- data.table(xstar = character(),
                      Dj = numeric(),
                      fold = numeric()) #store the mae values
  # -- begin k fold cross validation
  for(kk in 1:k){
    for(j in 1:sum(!is.na(Xyr))){
      if(length(stack) == 0){
        callname <- Xyr[j]
      }else{
        callname <- stack
        callname[(length(stack)+1)] <- Xyr[j]
        callname <- unique(callname)
      }
      
      
      train.X <- X[-fidx[[kk]],..callname]; train.y <- y[-fidx[[kk]]]
      test.X <- X[fidx[[kk]],..callname]; test.y <- y[fidx[[kk]]]
      
      # fit the model
      rhs <- paste('s(', callname, ',k=6)', sep = '', collapse = ' + ')
      fml <- paste('train.y', '~', rhs, collapse = ' ')
      fml <- as.formula(fml)
      
      b <- gam(fml,
               method = "REML",
               data = train.X,
               family = gaussian(link=log))
      
      # compute CRPS
      ## from the GAM: get mu (prediction for the test set on the log scale)
      mu.gam <- predict(b,newdata=test.X,type="link")
      ## then estimate standard dev based on in-sample residuals for training data
      sigma.gam <- sd(log(b$y) - b$linear.predictor)
      
      crps <- scoringRules::crps_lnorm(test.y, mu.gam, sigma.gam)
      
      DyHat <- rbind(DyHat,data.table(xstar = Xyr[j], Dj = crps, fold = kk))
    }
  }
  # -- end k fold cross validation
  DyHat <- DyHat[,mean(Dj),by=xstar]
  newFeature <- DyHat$xstar[which.min(DyHat$V1)]
  error.metric <- min(DyHat$V1)
  
  print(paste0(Xyr[which(Xyr %in% newFeature)]," ", which(Xyr %in% newFeature)))
  print(error.metric)
  
  return(list(newFeature,error.metric))
}



run_inner_model <- function(y,X,stack,newFeature){
  # run the inner model with the stack + the proposed new feature
  # to generate model statistics used in termination test and next iter of alg
  # inner model: varies ((running w xgboost as of 20.01.2023))
  # ----
  # outputs:
  #   the residuals from the inner model run w. stack + new feature
  #   the mae from the inner model run w. stack + new feature
  # ---------------
  # y = response variable
  # X = feature matrix
  # stack = current stack of chosen features
  # newFeature = feature proposed from miso step
  # idealnrounds,tunedhyperparams = hyperparameters for xgboost
  # ----------------------------------------
  if(!(newFeature %in% stack)){stack <- c(stack,newFeature)}
  
  # fit the model
  rhs <- paste('s(', stack, ',k=6)', sep = '', collapse = ' + ')
  fml <- paste('y', '~', rhs, collapse = ' ')
  fml <- as.formula(fml)
  
  b <- gam(fml,
           method = "REML",
           data = X,
           family = gaussian(link=log))
  
  # work with the in-sample residuals on the response scale 
  pred.y <- predict.gam(b,
                        newdata = X,
                        type="response")
  res <- y - pred.y
  return(res)
}

termination_test <- function(stack,newFeature,oldD,newD,eps){
  # boolean function to check termination test for IIS
  # as of 17.02.2023 "D" is MAE
  # 1. if the difference between oldD and newD < eps (eps user chosen)
  # then algorithm terminates
  # or
  # 2. if a feature is selected twice algorithm terminates
  # ----
  # outputs:
  #   TRUE / FALSE
  # ---------------
  # stack = current stack of chosen features
  # newFeature = feature proposed from latest miso step
  # oldD, newD = mae measures
  # eps = bound for difference between oldD & newD
  # ----------------------------------------
  terminate = FALSE
  # 1. a feature is selected twice
  if(newFeature %in% stack){terminate = T}
  # 2. the performance of the inner model as measured by D
  # does not significantly improve
  if(abs(oldD - newD) < eps){terminate = T}
  return(terminate)
}


IIS <- function(y,X,k,p,eps,
                outernrounds,outerhyperparams,
                linadd = FALSE){
  # wrapper function to run IIS
  # ----
  # outputs:
  #   stack, order of features in stack, mae from total model
  # ---------------
  # y = response variable
  # X = feature matrix
  # k = number of folds in cross-validation
  # p = we evaluate the top p features in this step (in order of avg gain)
  # eps = bound for difference between oldD & newD
  # outernrounds,outerhyperparams = params for ranking method (outer model)
  # innernrounds,innerhyperparams = params for inner model (predictive perform)
  # linadd = option to run a linear additive model as inner model
  # ----------------------------------------
  tt <- data.table(Feature=character(),
                   ord = numeric(), errormetric=numeric())
  stack = vector()
  storeD = vector()
  check = F
  
  fidx <- createFolds(y,k) 
  
  ## initialize everything in first round -------------
  ranked <- variable_ranking(y,X,outernrounds,outerhyperparams)
  
  ## change this here:
  fullobj <- miso(y,X,ranked,fidx,k,p,stack)
  
  newFeature <- fullobj[[1]]
  oldD <- fullobj[[2]]
  
  stack <- c(stack, newFeature)
  yHat <- run_inner_model(y,X,stack,newFeature)
  
  storeD <- c(storeD,oldD)
  ## -------------------------------------------------
  
  while(check == FALSE){
    ranked <- variable_ranking(yHat,X,outernrounds,outerhyperparams)
    
    fullobj <- miso(y,X,ranked,fidx,k,p,stack)
    newFeature <- fullobj[[1]]
    newD <- fullobj[[2]]
    
    yHat <- run_inner_model(y,X,stack,newFeature)
    
    check <- termination_test(stack,newFeature,oldD,newD,eps)
    if(check == F){
      stack <- c(stack, newFeature)
      oldD <- newD
      storeD <- c(storeD,newD)}
  }
  
  tt <- rbind(tt,data.table(Feature = stack,
                            ord = seq(1:length(stack)), 
                            errormetric = storeD))
  return(tt)
}