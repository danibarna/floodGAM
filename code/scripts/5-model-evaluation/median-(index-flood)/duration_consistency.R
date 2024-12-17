##
##
##
##
##
## Evaluate fitted models on:
## duration consistency 
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)
library(scico)


## source functions to quickly plot and check results:
source("~/floodGAM/code/functions/fn_check_results_with_plots.R")

## ----- load in the predictions and error metrics:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_median_flood_oos_pred.rds"))

pred.draws <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_median_index_flood_posterior_draws.rds"))

# order the columns properly
setkey(oos.pred,ID,model,d)

ics <- which(oos.pred[,.(difc = c(diff(eta),0), d=d),by=c("ID","model")]$difc>0)

oos.ics <- oos.pred[ics,] 

## (can also check the predictive uncertainty for the inconsistent stations)
## how many are *significantly* inconsistent? 

pred.draws <- pred.draws[ID%in%unique(oos.ics$ID)]

tt <- pred.draws[,list(quantile(eta.draws,0.05),
                       quantile(eta.draws,0.95)),by=c("ID","model","d")]

tv <- merge(tt,
            oos.pred[ID%in%unique(oos.ics$ID),c("ID","eta","eta.obs","model","d")],
            by=c("ID","model","d"))

ct <- merge(dcast(tv, ID + model ~ d, value.var = "V1"),
            dcast(tv, ID + model ~ d, value.var = "V2"),
            by=c("ID","model"))

## check for instances where d1.x > d2.y

ct[`12.x`>`1.y`,.N,by=c("model")]

ct[`24.x`>`1.y`,.N,by=c("model")]

ct[`48.x`>`1.y`,.N,by=c("model")]

dvec <- unique(oos.pred$d)
dcgrid <- data.table(i=c(rep( dvec, each = length(dvec)),
                         rep( dvec, each = length(dvec))),
                     j=c(rep( dvec, length(dvec)),
                         rep( dvec, length(dvec))),
                     model=rep(c("RFFA2018","floodGAM"),
                               each=length(dvec)*length(dvec)))

zvec <- rep(NA,dim(dcgrid)[1])
zi = 1

for(navn in c("RFFA2018","floodGAM")){
  for(i in dvec){
    for(j in dvec){
      if(i<=j){
        zvec[zi] = NA
        zi=zi+1
      }else{
        zvec[zi] = ct[model == navn & 
                        get(paste0(i,".x")) > get(paste0(j,".y")), .N] 
        zi=zi+1
      }
    }
  }
}


dcgrid[,z:=zvec]


ggplot(dcgrid[i!=0&j!=0], 
       aes(as.factor(j),as.factor(i),fill=as.factor(z))) +
  geom_tile() +
  scale_fill_scico_d(name = "Num. inconsistent",
                     palette = "batlowW",direction=-1) +
  labs(x = "duration (hours)", y = "duration (hours)") +
  facet_wrap(vars(model)) +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(nrow=1)) 

dcgrid[,sum(z,na.rm=T),by="model"]


## if we want to plot a few...

ggplot(pred.draws[ID=="157-3"&d%in%c(1,12,24,48)]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  geom_vline(data=oos.pred[model!="xgboost"&ID=="15700003"&d%in%c(1,12,24,48,72,168)],
             aes(xintercept = eta.obs,color=as.factor(d))) +
  facet_wrap(vars(model)) +
  theme_bw()


station = "196-11"
ggplot(pred.draws[ID==station&d%in%c(1,12,24,48,72,168)]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  geom_vline(data=oos.pred[model!="xgboost"&ID==station&d%in%c(1,12,24,48,72,168)],
             aes(xintercept = eta.obs,color=as.factor(d))) +
  facet_wrap(vars(model)) +
  theme_bw()
