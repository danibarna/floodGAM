##
##
##
##
## The catchment covariates are those taken from 
## https://zenodo.org/record/5382146 
## 
## In addition we make some seasonal covariates (aggregates of precip, temperature,
## rain+snowmelt over both summer and winter)
##
## See description_catchment_properties_zenodo_5382146.txt
##
## This file 
## - selects covariates for the gamfelt stations
## - adds some variable transformations needed to run RFFA_2018
## - saves the final covariate file as an .rds file.
##
## NOTE 19.07.2024: missing two stations: 46.7 and 25.30
## -----------------------------------------------------------------------------

library(data.table)
library(stringr)

gfcov <- fread("~/floodGAM/data/raw-data/catchment_properties_zenodo_5382146.csv")

qdfgp <- fread("~/floodGAM/data/raw-data/cov-qd-fgp.csv")

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/","gamfelt/",
                       "gamfelt_1hour_annual_maxima.rds"))

## make the "RN" and "HN" (regime nummer og hovednummer) to "ID" format:
gfcov[,ID:=paste0(regine,"-",main)]

## merge with the flood-generating process covariate
gfcov <- merge(gfcov,qdfgp[,c("QD_fgp","ID")],by="ID")

## select only covariates for stations that exist in gamfelt set:
gfcov <- gfcov[ID %in% unique(gfam$ID)]

## get rid of the commas (protected character) in column names:
setnames(gfcov,names(gfcov),gsub(",","_", names(gfcov)))

# still missing those two stations:
A <- unique(gfam[,get("ID")])
B <- gfcov[,get("ID")]
setdiff(A,B)

## sanity check: check the class of each column
sapply(gfcov,class)

## create some variable transforms. These are needed for RFFA_2018 and
## for floodGAM (see Table 2 in paper II "Regional median flood estimation 
## with generalized additive models: model selection across durations")
gfcov[,R_L_sqrt:=sqrt(exp(`ln(R_L)`))]
gfcov[,log_R_G_1085:=log(R_G_1085+1)]
gfcov[,log_R_G:=log(R_G+1)]
gfcov[,Q_N_cuberoot:=exp(`ln(Q_N)`)^(1/3)]
gfcov[,T_Feb_sqrd:=T_Feb^2]
gfcov[,T_Mar_cubed:=T_Mar^3]
gfcov[,W_Mai_sqrt:=sqrt(W_Mai)]

## remove Q_median since we predicting that:
gfcov[,Q_median:=NULL]

## for consistency with earlier analyses, remove the log transforms:
vtab <- names(gfcov)[grepl("ln",names(gfcov))]
coltab <- gsub("\\(","",gsub("\\)","",gsub("ln","",vtab)))

tt <- gfcov[,exp(.SD),.SDcols=vtab]
setnames(tt,names(tt),coltab)

gfcov <- cbind(gfcov[,!..vtab],tt)


setnames(gfcov,c("regine","main"),c("RN","HN"))

saveRDS(gfcov,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                         "gamfelt_catchment_covariates.rds"))

fwrite(gfcov,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.csv"))

