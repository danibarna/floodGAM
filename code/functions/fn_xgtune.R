##
##
## functions to tune hyperparameters for xgboost 'xgtune.mae' & 'xgtune.mape'
## 
## relies on built-in function 'xgb.cv'
## then we defined this extra wrapper function because we were running
## iterations with different treedepths, eval metrics, etc.
## -----------------------------------------------------------------------------

xgtune.mae <- function(y,X,nbr,esr,treedepths,mcw,subsamp,eta,nfolds){
  # pure grid search hyperparameter tuning for XGBoost
  # ----
  # outputs:
  #   a list of hyperparameters
  #   a vector of best_nrounds
  #   a vector of min mape
  # ---------------
  # y = response variable
  # X = feature matrix
  # nbr = max number of boosting rounds allowed
  # esr = num of round w/out improvement we stop after
  # treedepths = [vector] maximum number of nodes allowed from the root 
  #              to the farthest leaf of a tree.
  # mcw = [vector] minimum child weight; number of samples required in order to 
  #       create a new node in the tree. 
  # subsamp = [vector] the fraction of observations 
  #           to subsample at each step
  # eta = [vector] controls the learning rate (lower eta, lower learning rate)
  # ----------------------------------------
  
  ## --- create XGBoost object
  xgbdat = xgb.DMatrix(data = as.matrix(X), 
                       label = y)
  ## --- initialize data objects
  best_params = matrix(NA,nrow = length(treedepths), 
                       ncol = 4)    # number of hyperparameters changed
  ideal_rounds = vector()
  min_emt_vec = vector()
  set.seed(42)
  
  ## --- start hyperparameter grid search
  min_emt = Inf
  hypergrid = expand.grid(max_depth = treedepths,
                          min_child_weight = mcw,
                          subsample = subsamp,
                          eta = eta)
  for(i in 1:nrow(hypergrid)){
    cvout <- xgb.cv(data = xgbdat, 
                    max_depth = hypergrid$max_depth[i],
                    min_child_weight = hypergrid$min_child_weight[i],
                    subsample = hypergrid$subsample[i],
                    eta = hypergrid$eta[i], 
                    nrounds = nbr,
                    nfold = nfolds,
                    seed = 42,
                    eval_metric = "mae",
                    maximize = FALSE,
                    early_stopping_rounds = esr,
                    objective = "reg:squarederror")
    
    mean_emt <- cvout$evaluation_log[cvout$best_iteration]$test_mae_mean
    
    boost_rounds <- cvout$evaluation_log[cvout$best_iteration]$iter
    
    if(mean_emt < min_emt){
      min_emt = mean_emt
      best_params = c(cvout$params$max_depth,
                      cvout$params$min_child_weight,
                      cvout$params$subsample,
                      cvout$params$eta)
      ideal_rounds = boost_rounds + 1
    }
    
  }
  
  tunedparams <- list(max_depth = best_params[1],
                      min_child_weight = best_params[2],
                      subsample = best_params[3],
                      eta = best_params[4])
  
  return(list(tunedparams,min_emt,ideal_rounds))
}


xgtune.mape <- function(y,X,nbr,esr,treedepths,mcw,subsamp,eta,nfolds){
  # pure grid search hyperparameter tuning for XGBoost
  # ----
  # outputs:
  #   a list of hyperparameters
  #   a vector of best_nrounds
  #   a vector of min mape
  # ---------------
  # y = response variable
  # X = feature matrix
  # nbr = max number of boosting rounds allowed
  # esr = num of round w/out improvement we stop after
  # treedepths = [vector] maximum number of nodes allowed from the root 
  #              to the farthest leaf of a tree.
  # mcw = [vector] minimum child weight; number of samples required in order to 
  #       create a new node in the tree. 
  # subsamp = [vector] the fraction of observations 
  #           to subsample at each step
  # eta = [vector] controls the learning rate (lower eta, lower learning rate)
  # ----------------------------------------
  
  ## --- create XGBoost object
  xgbdat = xgb.DMatrix(data = as.matrix(X), 
                       label = y)
  ## --- initialize data objects
  best_params = matrix(NA,nrow = length(treedepths), 
                       ncol = 4)    # number of hyperparameters changed
  ideal_rounds = vector()
  min_emt_vec = vector()
  set.seed(42)
  
  ## --- start hyperparameter grid search
  min_emt = Inf
  hypergrid = expand.grid(max_depth = treedepths,
                          min_child_weight = mcw,
                          subsample = subsamp,
                          eta = eta)
  for(i in 1:nrow(hypergrid)){
    cvout <- xgb.cv(data = xgbdat, 
                    max_depth = hypergrid$max_depth[i],
                    min_child_weight = hypergrid$min_child_weight[i],
                    subsample = hypergrid$subsample[i],
                    eta = hypergrid$eta[i], 
                    nrounds = nbr,
                    nfold = nfolds,
                    seed = 42,
                    eval_metric = "mape",
                    maximize = FALSE,
                    early_stopping_rounds = esr,
                    objective = "reg:squarederror")
    
    mean_emt <- cvout$evaluation_log[cvout$best_iteration]$test_mape_mean
    
    boost_rounds <- cvout$evaluation_log[cvout$best_iteration]$iter
    
    if(mean_emt < min_emt){
      min_emt = mean_emt
      best_params = c(cvout$params$max_depth,
                      cvout$params$min_child_weight,
                      cvout$params$subsample,
                      cvout$params$eta)
      ideal_rounds = boost_rounds + 1
    }
    
  }
  
  tunedparams <- list(max_depth = best_params[1],
                      min_child_weight = best_params[2],
                      subsample = best_params[3],
                      eta = best_params[4])
  
  return(list(tunedparams,min_emt,ideal_rounds))
}
