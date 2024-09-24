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

ggsave(g1, 
       file="~/floodGAM/results/figures/compare_gamfelt_zfelt_covariate_ranges.pdf",
       width = 9)

