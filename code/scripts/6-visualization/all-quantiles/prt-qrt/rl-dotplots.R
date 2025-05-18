##
##
##
##
##
##
## Make figure 4 from paper III
##
## Fig 4 - Model to model comparison at the 1 hour duration for the 
## out-of-sample predicted return levels from the QRT and PRT, colored by xi
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggh4x)
library(ggpubr)
library(scico)


## load in the GAM-predicted return levels
load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")

## load in the local Stan fits:
gevp <- readRDS("~/floodGAM/results/output/all-quantiles/gamfeltstanresult.rds")

## load in the covariates
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

lookrp = 100


## ---- QRT

# make sure the quantiles are consistent (monotonically increasing)
adjust.fn <- function(x,y){c(NA,quantreg::rearrange(stepfun(x[-1],y))(x[-1]))}

qrt.predictions[,adj.val := lapply(.SD,function(col) adjust.fn(x=rp,y=val))$rp,
                .SDcols = c("rp","val"),
                by = c("d","ID")]

qrt <- qrt.predictions[rp %in% lookrp]
setnames(qrt,"adj.val","rl")
qrt[,pan:="QRT"]
qrt[,group:="Compared to local GEV fit"]

## ---- PRT 

## go from long to wide format
prt <- dcast(prt.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
# note these are the GEV location-scale parameterization sigma and mu,
# not the sigma mu from the predictive distribution (GAM)
prt[,sigma:=eta*exp(beta)]
prt[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# compute the return periods from the GEV parameters
prt <- prt[,list(mu+sigma/xi * ((-log(1-1/lookrp))^(-xi)-1),lookrp),
              by=c("d","ID")]
setnames(prt,"V1","rl")
prt[,pan:="PRT"]
prt[,group:="Compared to local GEV fit"]


## ---- Local fit 

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID + d ~ param, value.var = "mean")

# compute the return periods from the GEV parameters
gevp.rl <- gevp[,list(mu+sigma/xi * ((-log(1-1/lookrp))^(-xi)-1),lookrp),
                by=c("d","ID")]


gevp.rl <- merge(gevp[,c("ID","d","xi")],gevp.rl,by=c("ID","d"))
setnames(gevp.rl,"V1","rl")
gevp.rl[,pan:=" "]
gevp.rl[,group:="Compared to local GEV fit"]

# rbind things together ---------------------------------------------------

# messy code here...
setkey(prt,ID)
setkey(qrt,ID)
setkey(gevp.rl,ID)

thisd = 1

n = dim(gevp.rl[d==thisd])[1]

gdat.rl.100 <- data.table(x = c(gevp.rl[d==thisd,get("rl")], #239
                                gevp.rl[d==thisd,get("rl")], #239
                                prt[d==thisd,get("rl")]), #239
                          y = c(qrt[d==thisd,get("rl")],
                                prt[d==thisd,get("rl")],
                                qrt[d==thisd,get("rl")]),
                          ID = rep(gevp.rl[d==thisd,get("ID")],3),
                          xicolor = rep(gevp.rl[d==thisd,get("xi")],3),
                          pan = rep(c("QRT","PRT"," "),each=n),
                          group = rep(c("Compared to local GEV fit",
                                        "Compared to local GEV fit",
                                        " "),each=n))

gdat.rl.100 <- merge(gdat.rl.100,gfcov[,c("ID","A")],by="ID")

scico.limit <- max(abs(gdat.rl.100$xicolor)) * c(-1, 1)


g1 <- ggplot(gdat.rl.100[pan==" "]) +
  geom_point(aes(x,y,color=xicolor,size=A)) +
  scale_color_scico(name = expression(xi~parameter), 
                    palette = "bamO",direction = -1, limit=scico.limit,
                    breaks = c(-0.2,0,0.2)) +
  geom_abline(slope=1,size=0.6) +
  scale_x_sqrt(limits = c(50,6500)) + 
  scale_y_sqrt(limits = c(50,6500)) + 
  scale_shape_manual(values = 22, name="") +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,9),
                        breaks = c(50,1000,2000))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "QRT at T = 100",
                  " </span><span style='font-size: 12pt'> [ l/s/",
                  expression(km^2)," ]","</span>"), 
       x=paste0("<span style='font-size: 18pt'>",
                "PRT at T = 100",
                " </span><span style='font-size: 12pt'> [ l/s/",
                expression(km^2)," ]","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ group + pan, nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))


g2 <- ggplot(gdat.rl.100[pan!=" "]) +
  geom_point(aes(x,y,color=xicolor,size=A)) +
  scale_color_scico(name = expression(xi~parameter), 
                    palette = "bamO",direction = -1, limit=scico.limit,
                    breaks = c(-0.2,0,0.2)) +
  geom_abline(slope=1,size=0.6) +
  scale_x_sqrt(limits = c(50,6500)) + 
  scale_y_sqrt(limits = c(50,6500)) + 
  scale_shape_manual(values = 22, name="") +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,9),
                        breaks = c(50,1000,2000))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "Regional estimate at T = 100",
                  " </span><span style='font-size: 12pt'> [ l/s/",
                  expression(km^2)," ]","</span>"), 
       x=paste0("<span style='font-size: 18pt'>",
                "Local estimate at T = 100",
                " </span><span style='font-size: 12pt'> [ l/s/",
                expression(km^2)," ]","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +  
  facet_nested(~ group + pan, nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))


figure <- ggarrange(g1, g2,
                    labels = c("(a)","(b)"),
                    widths=c(1,2),
                    nrow=1,
                    common.legend = T,legend="bottom")

figure

# landscpae, 15 x 6 in

## make numerical summary for xi values in fig caption

tt <- gdat.rl.100[pan!=" "]

tt[y>x,mean(xicolor),by="pan"]
tt[y<x,mean(xicolor),by="pan"]



# Appendix figures --------------------------------------------------------

figlist <- list()

ii=1

for(thisd in c(6,12,18,24,36,48)){
  
  n = dim(gevp.rl[d==thisd])[1]
  
 
  
  gdat.rl.100 <- data.table(x = c(gevp.rl[d==thisd,get("rl")], #239
                                  gevp.rl[d==thisd,get("rl")], #239
                                  prt[d==thisd,get("rl")]), #239
                            y = c(qrt[d==thisd,get("rl")],
                                  prt[d==thisd,get("rl")],
                                  qrt[d==thisd,get("rl")]),
                            ID = rep(gevp.rl[d==thisd,get("ID")],3),
                            xicolor = rep(gevp.rl[d==thisd,get("xi")],3),
                            pan = rep(c("QRT","PRT"," "),each=n),
                            group = rep(c("Compared to local GEV fit",
                                          "Compared to local GEV fit",
                                          " "),each=n))
  
  upr = max(gdat.rl.100$x)*1.01
  lwr = min(gdat.rl.100$x)*0.09
  
  gdat.rl.100 <- merge(gdat.rl.100,gfcov[,c("ID","A")],by="ID")
  
  scico.limit <- max(abs(gdat.rl.100$xicolor)) * c(-1, 1)
  
  
  g1 <- ggplot(gdat.rl.100[pan==" "]) +
    geom_point(aes(x,y,color=xicolor,size=A)) +
    scale_color_scico(name = expression(xi~parameter), 
                      palette = "bamO",direction = -1, limit=scico.limit,
                      breaks = c(-0.2,0,0.2)) +
    geom_abline(slope=1,size=0.6) +
    scale_x_sqrt(limits = c(lwr,upr)) + 
    scale_y_sqrt(limits = c(lwr,upr)) + 
    scale_shape_manual(values = 22, name="") +
    scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                  sep = "")) ,
                          range=c(1.5,9),
                          breaks = c(50,1000,2000))+
    labs(y = paste0("<span style='font-size: 12pt'>",
                    "QRT at T = 100",
                    " </span><span style='font-size: 9pt'> [ l/s/",
                    expression(km^2)," ]","</span>"), 
         x=paste0("<span style='font-size: 12pt'>",
                  "PRT at T = 100",
                  " </span><span style='font-size: 9pt'> [ l/s/",
                  expression(km^2)," ]","</span>")) +
    theme_bw() +
    theme(text = element_text(family="serif",size = 12),
          aspect.ratio = 1,
          legend.position = "bottom",
          legend.spacing.x = unit(1.0, 'cm'),
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown(),
          strip.background = element_blank()) +
    facet_nested(~ group + pan, nest_line = element_line(linetype = 1)) +
    theme(strip.background = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "lightgrey"))
  
  
  g2 <- ggplot(gdat.rl.100[pan!=" "]) +
    geom_point(aes(x,y,color=xicolor,size=A)) +
    scale_color_scico(name = expression(xi~parameter), 
                      palette = "bamO",direction = -1, limit=scico.limit,
                      breaks = c(-0.2,0,0.2)) +
    geom_abline(slope=1,size=0.6) +
    scale_x_sqrt(limits = c(lwr,upr)) + 
    scale_y_sqrt(limits = c(lwr,upr)) + 
    scale_shape_manual(values = 22, name="") +
    scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                  sep = "")) ,
                          range=c(1.5,9),
                          breaks = c(50,1000,2000))+
    labs(y = paste0("<span style='font-size: 12pt'>",
                    "Regional estimate at T = 100",
                    " </span><span style='font-size: 9pt'> [ l/s/",
                    expression(km^2)," ]","</span>"), 
         x=paste0("<span style='font-size: 12pt'>",
                  "Local estimate at T = 100",
                  " </span><span style='font-size: 9pt'> [ l/s/",
                  expression(km^2)," ]","</span>")) +
    theme_bw() +
    theme(text = element_text(family="serif",size = 12),
          aspect.ratio = 1,
          legend.position = "bottom",
          legend.spacing.x = unit(1.0, 'cm'),
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown(),
          strip.background = element_blank()) +  
    facet_nested(~ group + pan, nest_line = element_line(linetype = 1)) +
    theme(strip.background = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "lightgrey"))
  
  
  figure <- ggarrange(g1, g2,
                      widths=c(1,2),
                      labels = c(paste0("d = ",thisd," hours")," "),
                      nrow=1,
                      common.legend = T,legend="bottom")
  
  figlist[[ii]] <- figure
  
  ii = ii+1
  
}


ggarrange(figlist[[1]],figlist[[2]],figlist[[3]],
          nrow=3,
          common.legend = T,legend="bottom")



ggarrange(figlist[[4]],figlist[[5]],figlist[[6]],
          nrow=3,
          common.legend = T,legend="bottom")


## each saved manually 10 x 13 in, portrait

