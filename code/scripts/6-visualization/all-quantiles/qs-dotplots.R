##
##
##
##
##
##
##
## Make figure 3 from paper III
##
## Fig 3 - Model to model comparison at the 1 hour duration of the average 
## quantile score at each station for return periods T = 10, 20 and 50 year
##
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(scico)

## load in the plotting data (from script 1-prtqrt_copmute_evaluation_metrics)
load("~/floodGAM/results/output/all-quantiles/objects-qs-and-rl-dotplots.rda")


qsg <- merge(prt.am,qrt.am,by=c("ID","d","rp"))

## update on join to recode return levels:
qsg[,rp:=as.character(rp)] # to make join types match
qsg[.(rp = c("10","20","50"), 
      to = c("T = 10 years", "T = 20 years", "T = 50 years")), 
    on = "rp", rp := i.to]



# Quantile score dotplot --------------------------------------------------



# for fraction of rain percentage labeling
scaleFUN <- function(x) sprintf("%.1f", x)

# make the quantile score plot
ggplot(qsg[d==1]) +
  geom_point(aes(prt.qs,qrt.qs,color=QD_fgp.x,size=A.x)) +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  geom_abline(slope=1,size=0.6) +
  scale_x_sqrt(limits = c(1,300)) + 
  scale_y_sqrt(limits = c(1,300)) + 
  scale_shape_manual(values = 22, name="") +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(1.5,7),
                        breaks = c(50,1000,2000))+
  labs(y = paste0("<span style='font-size: 18pt'>",
                  "QRT",
                  " </span><span style='font-size: 12pt'>  Quantile score [ l/s/",
                  expression(km^2)," ]","</span>"),
       x = paste0("<span style='font-size: 18pt'>",
                  "PRT",
                  " </span><span style='font-size: 12pt'>  Quantile score [ l/s/",
                  expression(km^2)," ]</span>")) +
  theme_bw() +
  facet_wrap(vars(rp),nrow = 1) +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        strip.background = element_blank())


ggsave(paste0("~/floodGAM/results/figures/all-quantiles/",
              "quantile_score_PRT_v_QRT.pdf"),
       width=12,height=6,units="in")




