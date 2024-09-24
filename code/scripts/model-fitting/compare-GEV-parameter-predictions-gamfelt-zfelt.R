##
##
##
##
##
## Use the gamfelt dataset to fit GAMs for each GEV parameter.
## 
## Compare the range of the in-sample predictions from the gamfelt
## to the out-of-sample predictions for zfelt
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)


## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                       "gamfelt_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# select only the floodGAM covariates
gfcov <- gfcov[,c("ID","Q_N","A_LE","A_P","H_F",
                  "R_G_1085","log_R_G_1085","W_Apr","P_Sep")]

gamfelt <- merge(gfcov,gfam,by="ID")


## ----- load in the zfelt dataset
zfcov <- readRDS(paste0("~/floodGAM/data/processed-data/zfelt/",
                        "zfelt_catchment_covariates.rds"))


## ----- load in the response variables (GEV parameters from Stan fits to each 
##       station in gfam)

gevp <- readRDS("~/floodGAM/results/output/gamfeltstanresult.rds")

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID ~ param, value.var = "mean")

# merge the response variable with the predictors (gamfelt covariate matrix)
gamdat <- merge(gevp,gfcov,by="ID")



# Fit the GAMs on the gamfelt data ----------------------------------------

## ---- eta
eta <- gam(qind ~ s(Q_N,k=6)+
             s(A_LE,k=6)+
             s(A_P,k=6)+
             s(H_F,k=6)+
             s(log_R_G_1085,k=6)+
             s(W_Apr,k=3)+s(P_Sep,k=3),
           method = "REML",
           data = gamdat,
           family = gaussian(link=log))
plot(eta)


## ---- beta
beta <- gam(beta ~ s(Q_N,k=6)+
              s(log_R_G_1085,k=6)+
              s(P_Sep,k=6),
            method = "REML",
            data = gamdat,
            family = gaussian(link=identity))

plot(beta)

## ---- xi
xi <- gam(xi ~ s(A_LE,k=6)+
            s(log_R_G_1085,k=6) +
            s(W_Apr,k=6),
          method = "REML",
          data = gamdat,
          family = gaussian(link=identity))

plot(xi)



# Check the support of the eta, beta, xi combos ---------------------------




# Use the fitted models to predict at zfelt locations ---------------------

## make the predictions
zfelt.eta <- predict.gam(eta, newdata = zfcov, type="response")
zfelt.beta <- predict.gam(beta, newdata = zfcov, type="response")
zfelt.xi <- predict.gam(xi, newdata = zfcov, type="response")









