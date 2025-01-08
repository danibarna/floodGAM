##
##
##
##
##
## Make the big table listing off all the autoGAM models
## -----------------------------------------------------------------------------

library(data.table)



iis.models <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_hydagsupp_featuresFromIIS_gam.rds"))


iis.models <- iis.models[edf>0.001]

# Function to keep the first underscore and remove the rest
remove_extra_underscores <- function(x) {
  parts <- unlist(strsplit(x, "_"))
  if (length(parts) > 1) {
    return(paste(parts[1], paste(parts[-1], collapse = ""), sep = "_"))
  } else {
    return(x)
  }
}


iis.models[, rowpos:= .I]
iis.models[,Feature:=lapply(.SD,remove_extra_underscores),
           .SDcols="Feature",by=rowpos]
iis.models[,Feature:=gsub("_", "_{", Feature)]
iis.models[,Feature:=ifelse(grepl("_",Feature,fixed=T),
                            paste0(Feature,"}"),
                            Feature)]
iis.models[,Feature:=paste0("$",Feature,"$")]

iis.models[,Feature:=ifelse(Feature=="$log_{RG}$","log$(R_G)$",Feature)]

iis.models[,Feature:=ifelse(Feature=="$log_{RG1085}$","log$(R_{G1085})$",Feature)]


## get into wide format
ag <- dcast(iis.models, fold + d ~ ordFeat , value.var = "Feature")


setkey(ag,d)

fwrite(ag,
       file = paste0("~/floodGAM/results/output/",
                     "median-(index-flood)/",
                     "autoGAM_covariates.csv"))








