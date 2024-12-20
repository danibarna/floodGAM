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

re[,type:="Relative error |(y.obs-y.pred)/y.pred|"]
re[,is:="Proportional error metric"]

ape[,type:="Absolute percent error |(y.obs-y.pred)/y.obs|"] 
ape[,is:="Proportional error metric"]

crps[,type:="Continuous ranked probability score"]
crps[,is:=" "]

ggdat <- rbind(re,ape,crps)




# Make the relative error plot --------------------------------------------

lwr = 0
upr = 5


g.proportional <- ggplot(ggdat[is!=" "]) + 
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
                        range=c(1.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA_2018","</span>"),
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



g.crps <- ggplot(ggdat[is==" "]) + 
  stat_density_2d(geom="polygon",aes(floodGAM,RFFA2018,
                                     fill = after_stat(level)),
                  bins=5,alpha=0.5) +
  geom_point(aes(floodGAM,RFFA2018,size=A,color=QD_fgp)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,linewidth=0.6) +
  scale_x_sqrt(limits = c(2,400)) + 
  scale_y_sqrt(limits = c(2,400)) + 
  scale_shape_manual(values = 22, name="") +
  scale_fill_scico(palette = "oslo",direction=-1,begin=0.4,end=0.95) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,13),
                        breaks = c(50,1000,2000))+
  guides(fill="none",
         size=guide_legend(override.aes=list(fill=NA)))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "RFFA_2018","</span>"),
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



figure <- ggarrange(g.proportional, g.crps,
                    labels = c("(a)","(b)"),
                    widths=c(1,0.52),
                    nrow=1,
                    common.legend = T, legend = "bottom")



figure
# saved manually as intext_dotplot_1_hr_floodGAM_v_RFFA.pdf, landscape, 16 x 7 in






