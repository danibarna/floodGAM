##
##
##
##
##
##
##
## create the maps for the data section: 
## - average total rainfall
## - location of stations colored by flood generating process
## - average temperature
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

## ----- load in preciptiation and temperature maps
r <- raster(paste0("~/floodGAM/data/raw-data/","map_files/",
                   "rr_normal_ann_1991-2020.nc"),  varname = "RR")

tam <- raster(paste0("~/floodGAM/data/raw-data/","map_files/",
                     "tm_normal_ann_1991-2020.nc"),  varname = "TAM")


# Functions to plot temp and precip and stations -------------------------------

plot_precip <- function(r){
  test_spdf <- as(r, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  
  test_df <- as.data.table(test_df)
  
  koectab = c("#D7FFFB","#80EBFF","#009AFE","#0119FF","#00009B",
              "#9A00FF","#FE00FF","#FF66FF")
  
  koebreaks <- c(7000,4000,3000,2000,1500,1000,750,500,0)
  
  koebreaks2 <- rev(c(7000,6000,4000,3000,2000,1500,1000,750,500,0))
  
  ltext=c(paste('>',koebreaks[2]),
          paste(koebreaks[3:(length(koebreaks)-1)],
                '-',
                koebreaks[2:(length(koebreaks)-2)]),
          paste('<',koebreaks[length(koebreaks)-1]))
  
  g <- ggplot(test_df) + 
    geom_raster(aes(x,y,fill=value)) + 
    scale_fill_gradientn(colors = koectab,
                         name = "mm/year",
                         values = scales::rescale(rev(koebreaks)),
                         breaks = koebreaks2,
                         guide = "legend",
                         labels = rev(c("b",ltext,"a")))+
    coord_equal() +
    theme_void() +
    theme(legend.position = c(0.62,0.28),
          text = element_text(family="sans"))
  return(g)
}

plot_temp <- function(r){
  test_spdf <- as(r, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  
  test_df <- as.data.table(test_df)
  
  koectab=c('#010F94','#002CF4','#409BF8', '#6CCDF8', '#AAEFFF', '#C3FDFE', 
            '#FFFFFF', '#FFFEC5','#FFFE65','#FBE669','#F3B643',
            '#EF6F30','#EA3A23','#C80F2E')
  
  koectab=c('#010F94','#002CF4','#409BF8', '#6CCDF8', '#AAEFFF', '#C3FDFE', 
            '#FFFFFF', '#FFFEC5','#FFFE65','#FBE669','#F3B643','#EF6F30')
  
  koebreaks=c(15,14,13,12,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-12) 
  
  koebreaks=c(14,13,12,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-12) 
  
  g <- ggplot(test_df) + 
    geom_raster(aes(x,y,fill=value)) + 
    scale_fill_gradientn(colors = rev(koectab),
                         name = "oC",
                         values = scales::rescale((koebreaks)),
                         breaks = koebreaks,
                         guide = "legend")+
    coord_equal() +
    theme_void() +
    theme(legend.position = c(0.6,0.33),
          text = element_text(family="sans"))
  return(g)
}

plot_stations <- function(gfcov,NorgeDF){
  
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
    theme(legend.position = c(0.65,0.29),
          text = element_text(family="sans"))
  return(g)
}

# Put all the plots together ---------------------------------------------

gprecip <- plot_precip(r)

gtemperature <- plot_temp(tam)

gstations <- plot_stations(gfcov,NorgeDF)

figure <- ggarrange(gprecip, gstations, gtemperature,
                    labels = c("(a)","(b)","(c)"),
                    widths=c(1,1,1),
                    nrow=1)

figure

## manually saved as catch_descript_map.pdf, landscape, 18 x 7 in











