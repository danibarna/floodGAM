##
##
##
##
##
##
## Create figure for section 4.3: duration consistency
## consistency gridplot
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)
library(scico)

# Data processing ---------------------------------------------------------

## ----- load in the predictions and draws from the posterior
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_oos_pred.rds"))

pred.draws <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_hydagsupp_median_index_flood_posterior_draws.rds"))

# order the columns properly
setkey(oos.pred,ID,model,d)

ics <- which(oos.pred[,.(difc = c(diff(eta),0), d=d),by=c("ID","model")]$difc>0)

oos.ics <- oos.pred[ics,] 

## (can also check the predictive uncertainty for the inconsistent stations)
## how many are *significantly* inconsistent? 

pred.draws <- pred.draws[ID%in%unique(oos.ics$ID)]

tt <- pred.draws[,list(quantile(eta.draws,0.1),
                       quantile(eta.draws,0.9)),by=c("ID","model","d")]

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


ggplot(dcgrid, 
       aes(as.factor(j),as.factor(i),fill=z)) +
  geom_tile() +
  scale_fill_scico(name = "Number of inconsistent stations  ",
                     palette = "vik",direction=1,
                     begin = 0.49,end=1,
                     na.value="transparent",
                   breaks = c(0,5,15,25)) +
  labs(x = "duration (hours)", y = "duration (hours)") +
  facet_wrap(vars(model)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_rect(fill = "white"),
        legend.key.width=unit(1,"cm"),
        legend.spacing = unit(4,"cm")) +
  guides(fill=guide_colourbar(byrow=T)) 


# saved manually as duration_inconsistency_grid.pdf, 
# landscape, 11 x 7 in


dcgrid[,sum(z,na.rm=T),by="model"]






