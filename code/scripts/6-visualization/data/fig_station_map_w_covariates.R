##
##
##
##
##
##
##
## create the maps for the data section, paper III: 
## - location of stations colored by 
## - flood generating process
## - mean annual runoff
## 
## -----------------------------------------------------------------------------

library(raster)
library(ncdf4)
library(data.table)
library(ggplot2)
library(ggpubr)
library(scico)
library(scales)
library(patchwork)

# Data preparation --------------------------------------------------------

## ----- load in the locations / covariates of gamfelt 
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

## ----- load in norway map object and define object for ggplot
load(paste0("~/floodGAM/data/raw-data/","map_files/",
            'station_selection_map_datafiles.rda'))
NorgeDF <- fortify(Norge)


## --- convert to l/s/km2
gfcov[,specQ:=Q_N/A*1000]



scaleFUN <- function(x) sprintf("%.1f", x) # to get good fraction of rain labels

cub<-function(x){
  x^(1/3)
}

cubr<-function(x){
  x<-ifelse(x<0, 0, x) # workaround, ggplot bug. purely aesthetic limit setting
  x^(3)
}



## QD_fgp


g1 <- ggplot() +
  geom_polygon(data=NorgeDF,aes(long,lat,group=group),
               fill = "white",color="grey",alpha=0.3) +
  geom_point(data=gfcov,aes(X_utm,Y_utm,
                            fill=QD_fgp,
                            size=A),
             color="black",
             inherit.aes = FALSE,
             pch=21,stroke=0.1) +
  scale_fill_scico(name = "Fraction of rain\ncontribution to\nflood generating process [%]",
                   palette = "lapaz",end=0.9,
                   alpha=1) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,13),
                        breaks = c(10,500,2000))+
  guides(fill="none") +
  coord_equal()+
  theme_void() +
  theme(legend.position = c(0.15,0.85),
        text = element_text(family="serif",size = 14),
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(c(4,4,4,4)))



g1h <- ggplot(gfcov, aes(x=QD_fgp)) +
  geom_histogram(aes(fill = after_stat((x))),
                 binwidth = 0.1,boundary = 0, color="white")+
  scale_fill_scico(name = "Fraction of rain\ncontribution to\nflood generating process",
                   palette = "lapaz",end=0.9,
                   alpha=1) +
  guides(fill="none") +
  labs(x = "Fraction of rain contribution to flood generating process") +
  theme_void() +
  theme(text = element_text(family="serif",size = 14),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(),
        axis.ticks.x = element_line(),
        axis.ticks.length=unit(.15, "cm"))


g1all <- g1 + inset_element(g1h, left = 0.4, bottom = 0.05, right = 0.99, top = 0.4)


## mean annual runoff?

g2 <- ggplot() +
  geom_polygon(data=NorgeDF,aes(long,lat,group=group),
               fill = "white",color="grey",alpha=0.3) +
  geom_point(data=gfcov,aes(X_utm,Y_utm,
                            fill=Q_N_SN,
                            size=A),
             color="black",
             inherit.aes = FALSE,
             pch=21,stroke=0.1) +
  scale_fill_scico(name = "Mean annual runoff 1961-1990 [mm/year]",
                   palette = "lipari",end=0.9,begin = 0.1,
                   alpha=1,
                   transform=scales::trans_new("cub",
                                               cub,
                                               cubr)) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,13),
                        breaks = c(10,500,2000))+
  guides(fill="none",size="none") +
  coord_equal()+
  theme_void() +
  theme(legend.position = c(0.75,0.29),
        text = element_text(family="sans"))


g2h <- ggplot(gfcov, aes(x=Q_N_SN)) +
  geom_histogram(aes(fill = after_stat((x))),
                  binwidth = 500,boundary = 0, color="white")+
  scale_fill_scico(name = "Mean annual runoff 1961-1990 [mm/year]",
                   palette = "lipari",end=0.9,begin=0.1,
                   alpha=1,
                   transform=scales::trans_new("cub",
                                               cub,
                                               cubr)) +
  guides(fill="none") +
  labs(x = "Mean annual runoff 1961-1990 [mm/year]") +
  theme_void() +
  theme(text = element_text(family="serif",size = 14),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(),
        axis.ticks.x = element_line(),
        axis.ticks.length=unit(.15, "cm"))


g2all <- g2 + inset_element(g2h, left = 0.4, bottom = 0.05, right = 0.99, top = 0.4)



figure <- ggarrange(g1all,g2all,
                    labels = c("(a)","(b)"),
                    nrow=1,
                    common.legend = T, legend = "bottom")

figure

ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "covariate_maps_hist.pdf"),
       width=14,height=7,units="in")











