##
##
##
##
##
##
## Make the appendix figures showing mode-to-model comparison on all durations
## and errror metrics
## -----------------------------------------------------------------------------

library(data.table)
library(ggh4x)
library(ggpubr)
library(scico)



# Load the data -----------------------------------------------------------

op <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                     "gamfelt_hydagsupp_predictive_accuracy_dotplotobj.rds"))



# Custom functions --------------------------------------------------------

scaleFUN <- function(x) sprintf("%.1f", x)

cub<-function(x){
  x^(1/3)
}

cubr<-function(x){
  x<-ifelse(x<0, 0, x) # workaround, ggplot bug. purely aesthetic limit setting
  x^(3)
}


sev<-function(x){
  x^(1/5)
}

sevr<-function(x){
  x<-ifelse(x<0, 0, x) # workaround, ggplot bug. purely aesthetic limit setting
  x^(5)
}



# dcast the data to wide format --------------------------------------------

re <- dcast(op, ID + A + QD_fgp + d ~ model, value.var = "re")

ape <- dcast(op, ID + A + QD_fgp + d ~ model, value.var = "ape")

crps <- dcast(op, ID + A + QD_fgp + d ~ model, value.var = "crps")

ae <- dcast(op, ID + A + QD_fgp+ d ~ model, value.var = "ae")

se <- dcast(op, ID + A + QD_fgp + d ~ model, value.var = "se")

# Squared error dotplots --------------------------------------------------

lwr = 0
upr = 1600000

## RFFA

se.rffa <- ggplot(se) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr),
                     breaks = c(0,5*10^5,2*10^6),
                     labels = function(x) format(x, scientific = TRUE)) +
  scale_y_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr),
                     breaks = c(0,5*10^5,2*10^6),
                     labels = function(x) format(x, scientific = TRUE)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



## autoGAM

se.auto <- ggplot(se) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr),
                     breaks = c(0,5*10^5,2*10^6),
                     labels = function(x) format(x, scientific = TRUE)) +
  scale_y_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr),
                     breaks = c(0,5*10^5,2*10^6),
                     labels = function(x) format(x, scientific = TRUE)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



figure <- ggarrange(se.rffa,
                    se.auto,
                    align='h',
                    labels = c("(a)","(b)"),
                    nrow=2,
                    common.legend = T, legend = "bottom")

figure # landscape, 24 x 10.5


# CRPS dotplots ---------------------------------------------------------------


lwr = 2
upr = 880

## RFFA

crps.rffa <- ggplot(crps) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr))+
  scale_y_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



## autoGAM

crps.auto <- ggplot(crps) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr)) +
  scale_y_continuous(transform=scales::trans_new("sev",
                                                 sev,
                                                 sevr),
                     limits=c(lwr,upr)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



figure <- ggarrange(crps.rffa,
                    crps.auto,
                    align='h',
                    labels = c("(a)","(b)"),
                    nrow=2,
                    common.legend = T, legend = "bottom")

figure # landscape, 24 x 10.5



# Absolute Error dotplots -----------------------------------------------------


lwr = 0
upr = 1300

## RFFA

ae.rffa <- ggplot(ae) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



## autoGAM

ae.auto <- ggplot(ae) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))


## XGBoost

ae.xgb <- ggplot(ae) + 
  stat_density_2d(geom="polygon",aes(floodGAM,xgboost,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,xgboost,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr))+
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "XGBoost","</span><span style='font-size: 12pt'>    [l/s/km2]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [l/s/km2]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



figure <- ggarrange(ae.rffa,
                    ae.auto,
                    ae.xgb,
                    align='h',
                    labels = c("(a)","(b)","(c)"),
                    nrow=3,
                    common.legend = T, legend = "bottom")

figure # landscape, 24 x 15.75


# Relative error dotplots --------------------------------------------------

lwr = 0
upr = 5

## RFFA

re.rffa <- ggplot(re) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span><span style='font-size: 12pt'>    [%]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [%]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



## autoGAM

re.auto <- ggplot(re) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span><span style='font-size: 12pt'>    [%]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [%]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



figure <- ggarrange(re.rffa,
                    re.auto,
                    align='h',
                    labels = c("(a)","(b)"),
                    nrow=2,
                    common.legend = T, legend = "bottom")

figure # landscape, 24 x 10.5



# Relative error dotplots --------------------------------------------------

lwr = 0
upr = 1.5

## RFFA

ape.rffa <- ggplot(ape) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span><span style='font-size: 12pt'>    [%]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [%]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



## autoGAM

ape.auto <- ggplot(ape) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=10,alpha=0.75) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(transform=scales::trans_new("cub",
                                                 cub,
                                                 cubr),
                     limits=c(lwr,upr),
                     breaks = c(0.1,0.5,1,upr),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,7),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span><span style='font-size: 12pt'>    [%]</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span><span style='font-size: 12pt'>    [%]</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ d, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))



figure <- ggarrange(ape.rffa,
                    ape.auto,
                    align='h',
                    labels = c("(a)","(b)"),
                    nrow=2,
                    common.legend = T, legend = "bottom")

figure # landscape, 24 x 10.5


