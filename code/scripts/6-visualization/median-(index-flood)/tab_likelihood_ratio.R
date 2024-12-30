##
##
##
##
## predictor importance ranking
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)

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



## ----- define the covariates we are testing:
stack <- c("Q_N","A_LE","A_P","H_F","log_R_G_1085","W_Apr","P_Sep")



# Fit the GAM with all parameters ------------------------------------------

ll <- data.table(var=character(),
                 loglikgam=numeric(),
                 loglikall=numeric(),
                 d=numeric())

## gam with all the parameters
for(di in c(1,24)){
  
  gamdat.d <- gamdat[d==di]
  
  ## fit the GAM with all parameters
  b.all <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                 s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
               method = "REML",
               data = gamdat.d,
               family = gaussian(link=log))
  
  ## remove one parameter at a time
  for(i in 1:length(stack)){
    
    rhs <- paste('s(', stack[-i], ',k=3)', sep = '', collapse = ' + ')
    aa <- unlist(gregexpr(pattern ='3',rhs))[1:5]
    if(i < 6){
      for(j in 1:4){substr(rhs,aa[j],aa[j]) <- "6"}
    }else{
      for(j in 1:5){substr(rhs,aa[j],aa[j]) <- "6"}
    }
    fml <- paste('qind', '~', rhs, collapse = ' ')
    fml <- as.formula(fml)
    
    
    b <- gam(fml,
             method = "REML",
             data = gamdat.d,
             select=T,
             family = gaussian(link=log))
    
    ll <- rbind(ll, data.table(var=stack[i],
                               loglikgam=logLik.gam(b)[1],
                               loglikall=logLik.gam(b.all)[1],
                               d=di))
  }
}


## absolute gain in likelihood value:

ll[,agl:=-1*(loglikgam-loglikall)]


setkey(ll,agl)


ll[d==1]

ll[d==24]







