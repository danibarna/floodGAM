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

# Data preparation --------------------------------------------------------

## ----- load in the locations / covariates of gamfelt 
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

## ----- load in norway map object and define object for ggplot
load(paste0("~/floodGAM/data/raw-data/","map_files/",
            'station_selection_map_datafiles.rda'))
NorgeDF <- fortify(Norge)





scaleFUN <- function(x) sprintf("%.1f", x) # to get good fraction of rain labels

g <- ggplot() +
  geom_polygon(data=NorgeDF,aes(long,lat,group=group),
               fill = "white",color="grey",alpha=0.3) +
  geom_point(data=gfcov,aes(X_utm,Y_utm,
                            fill=QD_fgp,
                            size=A),
             color="black",
             inherit.aes = FALSE,
             pch=21,stroke=0.1) +
  scale_fill_scico(name = "Fraction of rain\ncontribution to\nflood generating process",
                   palette = "lapaz",end=0.9,
                   alpha=1) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,7),
                        breaks = c(10,500,2000))+
  coord_equal()+
  theme_void() +
  theme(legend.position = c(0.75,0.29),
        text = element_text(family="sans"))
return(g)