##
##
##
##
##
## check internal model consistency for PRT and QRT oos predictions. 
## 
## Consistency for the QRT means oos predicted return levels are monotonically
## increasing for increasing return period
##
## Consistency for the PRT means oos predicted parameters fulfill support 
## requirements for the GEV
## -----------------------------------------------------------------------------

library(data.table)
library(quantreg)

load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")

## load in the min/max data values for each station to check the gev support:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# Parameter regression technique ------------------------------------------

## go from long to wide format
prt <- dcast(prt.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the sigma and mu for the GEV, not the
# sigma and mu in data.table prt.predictions--those sigma and mu
# come from the GAM predictions
prt[,sigma:=eta*exp(beta)]
prt[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# find the min and max data point for each station
dp <- gfam[, list(min(specQ), max(specQ)), by=c("ID","d")]
setnames(dp,c("V1","V2"),c("miny","maxy"))

# add the data points to the data table:
prt <- merge(prt,dp,by=c("ID","d"))


## test the support conditions:
which(prt[,(1 + xi*(maxy-mu)/sigma)]<=0)

which(prt[,(1 + xi*(miny-mu)/sigma)]<=0)

## none are outside the support. good. 



# Quantile regression technique -------------------------------------------

adjust.fn <- function(x,y){c(NA,quantreg::rearrange(stepfun(x[-1],y))(x[-1]))}

qrt.predictions[,adj.val := lapply(.SD,function(col) adjust.fn(x=rp,y=val))$rp,
                .SDcols = c("rp","val"),
                by = c("d","ID")]

# how many were adjusted?
qrt.predictions[,isadj:=ifelse(val!=adj.val,1,0)]

qrt.count <- qrt.predictions[,sum(isadj,na.rm = T),by=c("d","rp")]

# to match the table in paper III...
tt <- qrt.count[V1!=0 & rp%in%c(10,20,50,100)]

tt[,V2:=round((V1/239),3) * 100 ]

tt

# Save the objects --------------------------------------------------------

save(prt.predictions,qrt.predictions,file="~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")


