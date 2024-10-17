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
library(scoringRules)

## ----- source function to calculate optimal predictor for APE and RE:
source("~/floodGAM/code/functions/fn_calc_optimal_predictor.R")

## ----- load in the predictions from the models:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "median-index-flood-oos-predictions.rds"))

## ----- for plotting of evaluation metrics:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))


# Predictive accuracy -----------------------------------------------------

## Squared Error (SE)
oos.pred[model!="xgboost",se:= (eta.obs-eta)^2 ]
## CRPS -- requires package scoringRules
oos.pred[,crps:=scoringRules::crps_lnorm(eta.obs, mu.gam, sigma.gam)]
## Absolute Error (AE)
oos.pred[,ae:=abs( (eta.obs-eta) )]

## Proportional error metrics: RE and APE
## These take a while to get the approx to the optimal predictor...
oos.pred[, rowpos:= .I]
## Relative error
oos.pred[model!="xgboost"&d>6,
         eta.re:=optimal.predictor.re(sigma.gam,mu.gam,eta.obs),
         by=rowpos]
oos.pred[,re:=abs( (eta.obs-eta.re)/eta.re )]
## Absolute percent error
oos.pred[model!="xgboost"&d>6,
         eta.ape:=optimal.predictor.ape(sigma.gam,mu.gam,eta.obs),
         by=rowpos]
oos.pred[,ape:=abs( (eta.obs-eta.ape)/eta.obs )]

saveRDS(oos.pred,
        file = paste0("~/floodGAM/results/output/median-(index-flood)/",
                      "median-index-flood-predictive-accuracy.rds"))


oos.pred[,
         lapply(.SD,mean),.SDcols = c("crps","ae","re","ape"),
         by=c("model","d")]
oos.pred[,sqrt(mean(se)),by=c("model","d")]


permutationTest(oos.pred,"floodGAM","RFFA2018",1000,"ape",24)

permutationTest(oos.pred,"floodGAM","RFFA2018",1000,"ae",36)


# Duration consistency ----------------------------------------------------

# order the columns properly
setkey(oos.pred,ID,model,d)

ics <- which(oos.pred[,.(difc = c(diff(eta),0), d=d),by=c("ID","model")]$difc>0)

oos.ics <- oos.pred[ics,] # so 801 out of 5904 are inconsistent. how many are repeats?

oos.ics[,.N,by=c("ID","model")]
oos.ics[,max(d),by=c("ID","model")]

oos.ics[,.N,by="model"] # 463 = RFFA2018, 338 = floodGAM

tt <- oos.ics[,.N,by=c("d","model")]
setkey(tt,d,model)
tt

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


## where are the stations (if any) where there is no overlap in CI between durations?

ggplot(pred.draws[ID=="9900017"]) +
  geom_density(aes(eta.draws,group=d,fill=d),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="10100001"]) +
  geom_density(aes(eta.draws,group=d,fill=d),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="8900001"]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  facet_wrap(vars(model))

ggplot(pred.draws[ID=="15700003"]) +
  geom_density(aes(eta.draws,group=d,fill=as.factor(d)),alpha=0.2)+
  facet_wrap(vars(model))



# Reliability -------------------------------------------------------------

# use the model mu and model sigma to PIT the eta.obs:

pit <- oos.pred[,pnorm(log(eta.obs),mu.gam,sigma.gam),by=c("model","d")]

N <- dim(pit[d==0])[1]/2 # number of stations
NC <- 10

ctab = scico(2, palette = "turku",
             begin=0.3,end=0.5,direction=1)

p1 <- hist(pit[d==168&model=="floodGAM",get("V1")], nclass=NC, plot=F)
p2 <- hist(pit[d==720&model=="floodGAM",get("V1")], nclass=NC, plot=F)
plot(0,0,type="n",xlim=c(0,1),ylim=c(0,40),xlab="",ylab="")
title("floodGAM",line=0.5)
plot(p1,col=ctab[1],add=TRUE)
plot(p2,col=ctab[2],density=10,angle=135,add=TRUE)
abline(a=N/NC, b=0)
legend("topright", 
       legend = c("1 hour", "24 hours"), 
       col = ctab, 
       pch = c(15,15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(-0.05, 0.02))


ggplot(pit) + geom_histogram(aes(V1,group=d,color=d),alpha=0.2) +
  facet_wrap(vars(model))




# scrap -------------------------------------------------------------------



oos.pred <- merge(oos.pred,gfcov[,c("ID","A","QD_fgp")])

library(scico)

# go long to wide:
ggcrps <- dcast(oos.pred[d==0], ID + A + QD_fgp ~ model, value.var = "re")


scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(ggcrps) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=5,alpha=0.5) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  
  geom_abline(slope=1,size=0.6) +
  scale_x_sqrt(limits = c(0,7)) + 
  scale_y_sqrt(limits = c(0,7)) + 
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,7),
                        breaks = c(50,1000,2000))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018",
                  expression(km^2)," ]","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM",
                  expression(km^2)," ]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) 






