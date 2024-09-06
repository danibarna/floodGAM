##
##
##
##
##
## compare the ranges of the zfelt covariates to the gamfelt covariates
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(mgcv)

# load the gamfelt covariates
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

# load the zfelt covariates
zfcov <- readRDS(paste0("~/floodGAM/data/processed-data/zfelt/",
                        "zfelt_catchment_covariates.rds"))


# select only the floodGAM covariates
gfcov <- gfcov[,c("ID","Q_N","A_LE","A_P","H_F","R_G_1085","W_Apr","P_Sep")]

# Plot the covariate ranges ---------------------------------

# make long format
gfcov <- melt(setDT(gfcov), id.vars = c("Q_N","ID"), variable.name = "cov_val")
gfcov[,type:="pIIcov"]

zfcov <- melt(setDT(zfcov), id.vars = c("Q_N","ID"), variable.name = "cov_val")
zfcov[,type:="zfelt"]

ggdt <- rbind(gfcov,zfcov)

## how do the zfelt compare to the covariates we used in the analysis?
g1 <- ggplot() +
  geom_point(data=ggdt,aes(value,Q_N,color=type,alpha=type)) +
  geom_point(data=ggdt[type=="pIIcov"],aes(value,Q_N,color=type))+
  scale_alpha_manual(values=c(1,0.2))+
  facet_wrap(vars(cov_val),scales="free") +
  theme_bw()

ggsave(g1, file="~/floodGAM/results/figures/compare_gamfelt_zfelt_covariate_ranges.pdf")

# Compare the GEV parameter values  ---------------------------------------

# need: GAMs for each GEV parameter
# gamfelt streamflow data and gamfelt covariate data
# response variables are from at-site frequency analysis
# 1. compare gamfelt with amcov?
# 2. at-site frequency analysis for gamfelt
# fit the gams on the full gamfelt dataset
# check the support at each of the gamfelt stations
# then use the fitted models to predict at zfelts


## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]
# find the median flood for each catchment
gfam <- gfam[,.(medianSpecQ=median(specQ)),by="ID"]

# select only the floodGAM covariates
gfcov <- gfcov[,c("ID","Q_N","A_LE","A_P","H_F",
                  "R_G_1085","log_R_G_1085","W_Apr","P_Sep")]

gamfelt <- merge(gfcov,gfam,by="ID")


## ----- fit the gams on the entire gamfelt dataset

eta <- gam(gamfelt[,get("medianSpecQ")] ~ s(Q_N,k=6)+
           s(A_LE,k=6)+
           s(A_P,k=6)+
           s(H_F,k=6)+
           s(log_R_G_1085,k=6)+
           s(W_Apr,k=3)+s(P_Sep,k=3),
         method = "REML",
         data = gamfelt,
         family = gaussian(link=log))

plot(eta)

















dataPath <- paste0("~/ClimDesign_PhD/","XGBoost-GAM index flood model/",
                   "data/","Rdata files/")
fnPath <- paste0("~/ClimDesign_PhD/","XGBoost-GAM index flood model/",
                 "fns/")

## Load data ---
amcov <- readRDS(paste0(dataPath,"amcov_1_6_12_18_24_36_48.rds")) 
X <- readRDS(paste0(dataPath,"processed_covariates.rds"))


tt <- merge(amcov[d==1,c("ID","specQ")],gfam,by="ID")

plot(tt$specQ,tt$medianSpecQ)



