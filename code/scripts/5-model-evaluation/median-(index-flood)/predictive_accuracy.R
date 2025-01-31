##
##
##
##
##
## Evaluate fitted models on:
## - predictive accuracy at oos locations (5 metrics)
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)
library(scico)

## source function permutation test:
source("~/floodGAM/code/functions/fn_calc_optimal_predictor.R")
## source functions to quickly plot and check results:
source("~/floodGAM/code/functions/fn_check_results_with_plots.R")

## ----- load in the predictions and error metrics:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_predictive_accuracy_T_.rds"))

## ----- for plotting of evaluation metrics:
gfcov <- fread(paste0("C:/Users/daba/downloads/",
                        "raw_gamfelt_catchment_covariates.csv"))
gfcov[,ID:=paste0(RN,"-",HN)]


# Predictive accuracy -----------------------------------------------------

scre <- oos.pred[,
                 lapply(.SD,mean),.SDcols = c("crps","ae","re","ape"),
                 by=c("model","d")]

scre <- merge(scre,
              oos.pred[,sqrt(mean(se)),by=c("model","d")],
              by=c("model","d"))

setnames(scre,"V1","rmse")

scre[,re:=re*100]
scre[,ape:=ape*100]

scre[,c("rmse","crps","ae","re","ape") := lapply(.SD,round,1),
     .SDcols= c("rmse","crps","ae","re","ape")]

setkey(scre,d,re)

## TABLE 4 - error metrics
scre[model%in%c("floodGAM","RFFA2018","auto","xgboost"),c("model","d","rmse","crps","ae","re","ape")]

## statistical significance of different error metrics:
for(m in c("se","crps","ae","re","ape")){
  print(paste0("********",m))
  for(di in unique(oos.pred$d)){
    print(paste0(di," - ",permutationTest(oos.pred,
                                          "auto","RFFA2018",
                                          1000,
                                          m,di)) )
  }
}



## ------- Dotplots - predictive accuracy check 

oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

## save the data object for use in figure scripts:
saveRDS(oos.pred,file=paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_hydagsupp_predictive_accuracy_dotplotobj.rds"))



iis.models <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                     "gamfelt_hydagsupp_featuresFromIIS_gam.rds"))


iis.models <- iis.models[edf>0.001]


## cRPS
glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp + fold ~ model, value.var = "crps")
  
  ggdat[,outlineme:=ifelse(fold==3,"y",NA)]
  
  print(max(ggdat$auto))
  
  gi <- dotplotautofloodGAM(ggdat,2,900,paste0("CRPS, d = ",di))
  
  glist[[i]] <- gi
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure

## load gfam
ggplot(ttam[d>24]) +
  geom_point(aes(year_key,Qm3_s,color=as.factor(d))) +
  facet_wrap(vars(ID),scales="free_y")

#,"212-10","246-9","311-460"


## SE
glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp + fold ~ model, value.var = "se")
  
  print(max(ggdat$auto))
  
  print(ggdat[which.max(auto),])
  
  ggdat[,outlineme:=ifelse(fold==3,"y",NA)]
  
  gi <- dotplotautofloodGAM(ggdat,0,6.5*10^5,paste0("SE, d = ",di))
  
  
  glist[[i]] <- gi
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure


tt <- oos.pred[d==1&model=="floodGAM",]
setkey(tt,se)

### APE
glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp + fold ~ model, value.var = "ape")
  
  ggdat[,outlineme:=ifelse(fold==3,"y",NA)]
  
  gi <- dotplotautofloodGAM(ggdat,0,2,paste0("APE, d = ",di))
  
  glist[[i]] <- gi
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure


tt <- oos.pred[d==48&model=="floodGAM",]
setkey(tt,ape)

### AE
glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp + fold ~ model, value.var = "ae")
  
  ggdat[,outlineme:=ifelse(fold==3,"y",NA)]
  
  gi <- dotplotautofloodGAM(ggdat,0,2000,paste0("APE, d = ",di))
  
  glist[[i]] <- gi

  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure





### RE
glist <- list()
i = 1
for(di in unique(oos.pred$d)){
  
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp + fold ~ model, value.var = "re")
  
  ggdat[,outlineme:=ifelse(fold==3,"y",NA)]
  
  gi <- dotplotautofloodGAM(ggdat,0,5,paste0("RE, d = ",di))
  
  glist[[i]] <- gi
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure






tt <- merge(oos.pred[model=="RFFA2018",c("ID","d","crps","re","ape","eta","eta.obs")],
            oos.pred[model=="floodGAM",c("ID","d","crps","re","ape","eta")],by=c("ID","d"))

tt[,dr:=ape.x-ape.y]

tt[,dr.crps:=crps.x-crps.y]

tt[,dr.re:=re.x-re.y]

setkey(tt,dr.crps)

tt[d==48]


tt[ID=="311-6" & d==1]

oos.pred[ID=="311-4"]

tg <- gfam[ID=="311-4"]

ggplot(tg) +
  geom_point(aes(year_key,Qm3_s,group=d,color=as.factor(d))) +
  scale_x_continuous(breaks=1989:2024)


oos.pred[oos.pred[,.I[which.max(ape)],by=c("d","model")]$V1]


oos.pred[ID=="23400018"&model=="RFFA2018"]
