##
##
##
##
##
## Evaluate fitted models on:
## - predictive accuracy at oos locations (5 metrics)
## - duration consistency
## - reliability (PIT)
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
                           "median-index-flood-predictive-accuracy.rds"))

## ----- for plotting of evaluation metrics:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))


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

setkey(scre,d)

## TABLE 4 - error metrics
scre[,c("model","d","rmse","crps","ae","re","ape")]

## statistical significance of different error metrics:
for(di in unique(oos.pred$d)){
  print(paste0(di," - ",permutationTest(oos.pred,
                                        "floodGAM","RFFA2018",
                                        1000,
                                        "ape",di)) )
}


## ------- Dotplots - predictive accuracy

oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

glist <- list()
i = 1
for(di in unique(oos.pred$d)){
 
  ggdat <- dcast(oos.pred[d==di], ID + A + QD_fgp ~ model, value.var = "crps")
  
  glist[[i]] <- dotplotRFFA2018floodGAM(ggdat,0,1000,paste0("CRPS, d = ",di))
  
  i = i+1
}

figure <- ggarrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],
                    glist[[5]],glist[[6]],glist[[7]],glist[[8]],
                    nrow=2,ncol = 4,
                    common.legend = T, legend = "bottom")

figure

glist[[6]]



# Reliability -------------------------------------------------------------

# use the model mu and model sigma to PIT the eta.obs:

pit <- oos.pred[,pnorm(log(eta.obs),mu.gam,sigma.gam),by=c("model","d")]

par(mfrow=c(1,2),mar = c(3,3,3,1))

PITplot(pit,"floodGAM",1,24)

PITplot(pit,"RFFA2018",1,24)


## TABLE 5 - coverage and width

N <- length(unique(oos.pred$ID)) # number of stations

## there's a more elegant way to do this...
coverage <- pit[, .(round(sum(V1 > 0.25 & V1 < 0.75) / N * 100, 2),
        round(sum(V1 > 0.10 & V1 < 0.90) / N * 100, 2),
        round(sum(V1 > 0.05 & V1 < 0.95) / N * 100, 2)), 
    by = c("d","model")]
setnames(coverage,c("V1","V2","V3"),c("cov.50","cov.80","cov.90"))

width <- oos.pred[, .(round(mean(qlnorm(0.75, mu.gam, sigma.gam) - qlnorm(0.25, mu.gam, sigma.gam)),0),
             round(mean(qlnorm(0.90, mu.gam, sigma.gam) - qlnorm(0.10, mu.gam, sigma.gam)),0),
             round(mean(qlnorm(0.95, mu.gam, sigma.gam) - qlnorm(0.05, mu.gam, sigma.gam)),0)),
         by=c("d","model")]
setnames(width,c("V1","V2","V3"),c("wid.50","wid.80","wid.90"))

covwid <- merge(coverage,width,by=c("d","model"))

## print the table
covwid[model!="xgboost",c("d","model","cov.50","wid.50","cov.80","wid.80","cov.90","wid.90")]




# Duration consistency ----------------------------------------------------

# order the columns properly
setkey(oos.pred,ID,model,d)

ics <- which(oos.pred[,.(difc = c(diff(eta),0), d=d),by=c("ID","model")]$difc>0)

oos.ics <- oos.pred[ics,] 

## (can also check the predictive uncertainty for the inconsistent stations)
## how many are *significantly* inconsistent? 

pred.draws <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "median-index-flood-posterior-draws.rds"))

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

ct[`72.x`>`1.y`,.N,by=c("model")]

ct[`168.x`>`1.y`,.N,by=c("model")]

ct[`72.x`>`24.y`,.N,by=c("model")]

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
        zvec[zi] = ct[model == navn & get(paste0(i,".x")) > get(paste0(j,".y")), .N] 
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











ggplot(pred.draws[ID=="10100001"]) +
  geom_density(aes(eta.draws,group=d,fill=d),alpha=0.2)+
  facet_wrap(vars(model))

gt <- oos.pred[model!="xgboost"&ID=="8900001"&d%in%c(1,12,24,48,72,168)]

ggplot(pred.draws[ID=="8900001"&d%in%c(1,12,24,48,72,168)]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  geom_vline(data=gt,aes(xintercept = eta.obs,color=as.factor(d))) +
  facet_wrap(vars(model)) +
  theme_bw()



##..what? need to check this station more...
ggplot(pred.draws[ID=="23400018"]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  lims(x=c(0,250)) +
  facet_wrap(vars(model))


ggplot(pred.draws[ID=="15700003"&d%in%c(1,12,24,48,72,168)]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  geom_vline(data=oos.pred[model!="xgboost"&ID=="15700003"&d%in%c(1,12,24,48,72,168)],
             aes(xintercept = eta.obs,color=as.factor(d))) +
  facet_wrap(vars(model)) +
  theme_bw()


station = "19600011"
ggplot(pred.draws[ID==station&d%in%c(1,12,24,48,72,168)]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  geom_vline(data=oos.pred[model!="xgboost"&ID==station&d%in%c(1,12,24,48,72,168)],
             aes(xintercept = eta.obs,color=as.factor(d))) +
  facet_wrap(vars(model)) +
  theme_bw()


171.833/170.4

oos.pred[model=="RFFA2018",which.max(re)]
oos.pred[model=="RFFA2018"][1897]

pred.draws[ID=="23400018",max(eta.draws),by="model"]

# scrap -------------------------------------------------------------------



oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")],by="ID")

library(scico)

# go long to wide:
ggcrps <- dcast(oos.pred[d==0], ID + A + QD_fgp ~ model, value.var = "re")









