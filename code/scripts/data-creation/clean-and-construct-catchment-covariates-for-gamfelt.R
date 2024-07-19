##
##
##
##
## The catchment covariates are those taken from 
## https://zenodo.org/record/5382146 (sent in an email from Kolbjørn 22.02.2022)
## as well as seasonal covariates (aggregates of precip, temperature,
## rain+snowmelt over both summer and winter)
##
## See /data/raw-data/README-gamfelt-covariates.txt
##
## This file 
## - selects covariates for the gamfelt stations
## - adds some variable transformations needed to run RFFA_2018
## - saves the final covariate file as an .rds file.
##
## NOTE 19.07.2024: missing two stations: 2.457 and 25.30
## -----------------------------------------------------------------------------

library(data.table)
library(stringr)

gfcov <- fread("~/floodGAM/data/raw-data/raw_gamfelt_catchment_covariates.csv")

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/","gamfelt/",
                       "gamfelt_annual_maxima.rds"))

## make the "RN" and "HN" (regime nummer og hovednummer) to "ID" format:
gfcov[,HNpad:=lapply(.SD,str_pad,width=5,pad="0"),.SDcols="HN"]
gfcov[,ID:=paste0(as.character(RN),HNpad)]

gfcov[,HNpad:=NULL]

## select only covariates for stations that exist in gamfelt set:
gfcov <- gfcov[ID %in% unique(gfam$ID)]

# still missing those two stations:
A <- unique(gfam[,get("ID")])
B <- gfcov[,get("ID")]
setdiff(A,B)

## create some variable transforms. These are needed for RFFA_2018 and
## for floodGAM (see Table 2 in paper II "Regional median flood estimation 
## with generalized additive models: model selection across durations")
gfcov[,R_L_sqrt:=sqrt(R_L)]
gfcov[,log_R_G_1085:=log(R_G_1085+1)]
gfcov[,Q_N_cuberoot:=Q_N^(1/3)]
gfcov[,T_Feb_sqrd:=T_Feb^2]
gfcov[,T_Mar_cubed:=T_Mar^3]
gfcov[,W_Mai_sqrt:=sqrt(W_Mai)]


saveRDS(gfcov,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                         "gamfelt_catchment_covariates.rds"))

fwrite(gfcov,file = paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.csv"))

