
scaleFUN <- function(x) sprintf("%.1f", x)

dotplotRFFA2018floodGAM <- function(ggdat,lwr,upr,titleobj){
  
  gobj <- ggplot(ggdat) + 
    stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                       fill = after_stat(level)),
                    bins=5,alpha=0.5) +
    geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
    scale_color_scico(name = "Fraction of rain",
                      palette = "lapaz",end=0.95,
                      labels=scaleFUN) +
    geom_abline(slope=1,linewidth=0.6) +
    scale_x_sqrt(limits = c(lwr,upr)) + 
    scale_y_sqrt(limits = c(lwr,upr)) + 
    scale_shape_manual(values = 22, name="") +
    scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
    scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                  sep = "")) ,
                          range=c(1.5,7),
                          breaks = c(50,1000,2000))+
    guides(fill="none",
           size=guide_legend(override.aes=list(fill=NA)))+
    labs(y = paste0("<span style='font-size: 18pt'>",
                    "RFFA2018","</span>"),
         x = paste0("<span style='font-size: 18pt'>",
                    "floodGAM","</span>"),
         title = titleobj) +
    theme_bw() +
    theme(text = element_text(family="serif",size = 18),
          aspect.ratio = 1,
          legend.position = "bottom",
          legend.spacing.x = unit(1.0, 'cm'),
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown(),
          strip.background = element_blank()) 
  
  return(gobj)
}



