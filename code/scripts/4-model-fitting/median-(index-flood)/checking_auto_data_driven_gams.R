##
##
##
##
##
##
##
## oos prediction for the auto-data-driven GAMs (the predictors naievly selected
## from IIS_gam)
##
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)
library(caret)

# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_durations_annual_maxima.rds"))

## ---- load in the selected covariates from the IIS runs

iis.models <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_featuresFromIIS.rds"))


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


# Define the GAM and check select = T -------------------------------------

b.edf <- vector()

for(di in unique(iis.models$d)){
  
  stack <- iis.models[d==di,get("Feature")]
  gamdat.d <- gamdat[d==di]
  
  # fit the model
  rhs <- paste('s(', stack, ',k=6)', sep = '', collapse = ' + ')
  fml <- paste('qind', '~', rhs, collapse = ' ')
  fml <- as.formula(fml)
  
  b <- gam(fml,
           method = "REML",
           select = T,
           data = gamdat.d,
           family = gaussian(link=log))
  
  print(summary(b))
  
  b.edf <- c(b.edf,summary(b)$edf)
  
}

## what to do when select = T shrinks out one of the covariates? 
## f. eks. W_summer, 24 hours

## maybe just take it out of the stack

iis.models[,edf:=b.edf]


saveRDS(iis.models, paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_featuresFromIIS.rds"))
