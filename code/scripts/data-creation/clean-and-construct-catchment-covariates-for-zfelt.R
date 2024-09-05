##
##
## zfelt catchment covariate data processing
##
## output: zfelt covariates in processed-data folder
## -----------------------------------------------------------------------------

library(data.table)

# load the zfelt covariates
zfcov <- fread("~/floodGAM/data/raw-data/zfelt_catchment_covariates.csv")

## remove all z felt with R_G_1085 < 0 (171 zfelt)
zfcov <- zfcov[grad1085>0]

## remove all z felt with A_LE > 30 (13 zfelt)
## this matches what Kolbjørn and Trond did in the 2018 report
zfcov <- zfcov[effsjoPro <= 30]

## remove all z felt with NA for W_Apr (378 zfelt)
zfcov <- zfcov[!is.na("qtt_Apr (mm)")]

## compute covariates H_F and A_P
zfcov[,H_F:=heightMax-heightMin]
zfcov[,A_P:=`Ap (km)`/1000]

## change naming convention to match gamfelt
zfcov <- zfcov[,c("OBJECTID","QNormal6190_lskm2","effsjoPro","A_P","H_F",
                "grad1085","qtt_Apr (mm)","P_Sep (mm)")]
setnames(zfcov,
         c("OBJECTID","QNormal6190_lskm2","effsjoPro","H_F","grad1085",
           "qtt_Apr (mm)","P_Sep (mm)"),
         c("ID","Q_N","A_LE","H_F","R_G_1085","W_Apr","P_Sep"))

## save zfelt in processed-data folder
saveRDS(zfcov,file=paste0("~/floodGAM/data/processed-data/zfelt/",
                          "zfelt_catchment_covariates.rds"))

