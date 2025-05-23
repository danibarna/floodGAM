##
##
##
##
##
##
## Make the in-text dotplot showing RFFA2018 systematically underestimates
## the median flood at large, snowmelt driven catchments. 
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



# dcast the data to wide format --------------------------------------------

re <- dcast(op[d==1], ID + A + QD_fgp ~ model, value.var = "re")

ape <- dcast(op[d==1], ID + A + QD_fgp ~ model, value.var = "ape")

crps <- dcast(op[d==1], ID + A + QD_fgp ~ model, value.var = "crps")

ae <- dcast(op[d==1], ID + A + QD_fgp ~ model, value.var = "ae")

re[,type:="Relative error |(y.obs-y.pred)/y.pred|"]
re[,is:="Proportional error metric"]

ape[,type:="Absolute percent error |(y.obs-y.pred)/y.obs|"] 
ape[,is:="Proportional error metric"]

crps[,type:="Continuous ranked probability score"]
crps[,is:=" "]

ae[,type:="Absolute error"]
ae[,is:=" "]

ggdat <- rbind(re,ape,crps)

ggdat <- rbind(re,ape,ae)



# Make the relative error plot --------------------------------------------

lwr = 0.00025
upr = 5

#square function
cub<-function(x){
  x^(1/3)
}
#inverse square function (square root)
cubr<-function(x){
  x<-ifelse(x<0, 0, x) # workaround, ggplot bug. purely aesthetic limit setting
  x^(3)
}


g.proportional.rffa <- ggplot(ggdat[is!=" "]) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=7,alpha=0.5) +
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
                        range=c(0.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ is + type, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))


g.ae.rffa <- ggplot(ggdat[is==" "]) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=5,alpha=0.5) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_sqrt(limits = c(0,650)) + 
  scale_y_sqrt(limits = c(0,650)) + 
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA2018","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank()) +
  facet_nested(~ is + type, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))

g.ae.rffa



# floodGAM vs autoGAM -----------------------------------------------------

g.proportional.auto <- ggplot(ggdat[is!=" "]) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=7,alpha=0.5) +
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
                        range=c(0.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = 'white')) +
  facet_nested(~ is + type, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "white"))

g.proportional.auto


g.ae.auto <- ggplot(ggdat[is==" "]) + 
  stat_density_2d(geom="polygon",aes(floodGAM,auto,
                                     fill = after_stat(level)),
                  bins=5,alpha=0.5) +
  geom_point(aes(floodGAM,auto,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_sqrt(limits = c(0,650)) + 
  scale_y_sqrt(limits = c(0,650)) + 
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(0.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "autoGAM","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "floodGAM","</span>")) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank(),
        strip.text.x = element_text(colour = 'white')) +
  facet_nested(~ is + type, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"),
        theme(plot.margin = unit(c(0,0.2,0,1), 'lines')))


g.ae.auto



figure <- ggarrange(g.proportional.rffa, g.ae.rffa,
                    g.proportional.auto, g.ae.auto,
                    align='h',
                    labels = c("(a)","(b)","(c)","(d)"),
                    widths=c(1,0.52,1,0.52),
                    nrow=2,ncol = 2,
                    common.legend = T, legend = "bottom")



figure
# saved manually as intext_dotplot_1_hr_floodGAM_v_RFFA.pdf, landscape, 16 x 7 in






