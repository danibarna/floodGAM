##
##
##
##
##
##
## covariates for point color and size
## which stations are 1-24 inconsistent
## local estimates for the parameter values
## 
## -----------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggpubr)
library(ggh4x)
library(scico)
library(scales)
library(gridExtra)

## load in the GAM-predicted values:
load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")

## load in the local Stan fits:
gevp <- readRDS("~/floodGAM/results/output/all-quantiles/gamfeltstanresult.rds")

## load in the covariates for each station:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

## load in which stations are 1-24 hour inconsistent:
load("~/floodGAM/results/output/all-quantiles/plotobj-duration-incon.rda")
## we want the object 'alltt'. Then:
these <- alltt[i==24&j==1&type=="PRT",unique(ID)]



# Some data processing ----------------------------------------------------

# local stan fits, go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp[d%in%c(1,24)], ID + d ~ param, value.var = "mean")


# GAM-predicted PRT, go from long to wide format,
prt <- dcast(prt.predictions[d%in%c(1,24)], ID + d ~ param, value.var = "val")



# For legend scales -------------------------------------------------------

scaleFUN <- function(x) sprintf("%.1f", x)


# Parameter dotplots for xi -----------------------------------------------

xiGEV <- dcast(gevp, ID ~ d, value.var = "xi")
            
xi <- dcast(prt, ID ~ d, value.var = "xi")
setnames(xi,c("1","24"),c("xi1","xi24"))

xi <- merge(xi,gfcov[,c("ID","A","QD_fgp")])

xi[,isincon:=ifelse(ID%in%these,1,0)]



## make the xi plot with zoomed in panel
g1 <- ggplot(xi) +
  annotate("point",xiGEV[,get("1")],xiGEV[,get("24")],color="darkgoldenrod2") +
  geom_point(aes(xi1,xi24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=xi[isincon==1],
             aes(xi1,xi24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  annotate("text", x= -0.008, y = 0.09, label= "(b)", fontface = 2) +
  annotate("rect", xmin = -0.019, xmax = 0.1, ymin = -0.019, ymax = 0.1, 
           alpha=0, color="blue", fill="blue", linetype = 2) +
  annotate("point",-0.18,0.23,color="darkgoldenrod2") +
  annotate("text", -0.175,0.231, label= "= local GEV fit",family = "serif",
           hjust = 0) +
  annotate("rect", xmin = -0.19, xmax = -0.086, ymin = 0.219, ymax = 0.24, 
           alpha=0, color="black", fill="blue") +
  guides(size="none",color="none",fill="none",shape="none") +
  lims(x=c(-0.2,0.25),y=c(-0.2,0.25)) +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "xi"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "xi"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.spacing.x = unit(1.0, 'cm'),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(),
        strip.background = element_blank()) 
  
  
g2 <- ggplot(xi) +
  annotate("point",xiGEV[,get("1")],xiGEV[,get("24")],color="darkgoldenrod2") +
  geom_point(aes(xi1,xi24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=xi[isincon==1],
             aes(xi1,xi24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  guides(size = guide_legend(override.aes = list(shape = 24)),
         shape = guide_legend(override.aes = list(size = 2)),
         color = "none") +
  coord_cartesian(xlim=c(-0.016,0.09),ylim=c(-0.016,0.09)) +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "xi"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "xi"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = c(0.2,0.71),
        legend.direction="vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(family="serif",size = 9),
        legend.title = element_text(family="serif",size = 9),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(),) 


figure <- ggarrange(g1,g2,
                    labels=c("(a)","(b)"),
                    widths=c(1,1),
                    nrow=1)
figure

ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "xi_param_est.pdf"),
       width=14,height=7,units="in")
ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "figure8.pdf"),
       width=14,height=7,units="in")



# Parameter plots for BETA ------------------------------------------------

betaGEV <- dcast(gevp, ID ~ d, value.var = "beta")

beta <- dcast(prt, ID ~ d, value.var = "beta")
setnames(beta,c("1","24"),c("beta1","beta24"))

beta <- merge(beta,gfcov[,c("ID","A","QD_fgp")])

beta[,isincon:=ifelse(ID%in%these,1,0)]



## make the beta plot
g3 <- ggplot(beta) +
  annotate("point",betaGEV[,get("1")],betaGEV[,get("24")],color="darkgoldenrod2") +
  geom_point(aes(beta1,beta24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=beta[isincon==1],
             aes(beta1,beta24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  annotate("text", x= -1.56, y = -1.04, label= "(b)", fontface = 2) +
  annotate("rect", xmin = -1.6, xmax = -1.01, ymin = -1.6, ymax = -1.01, 
           alpha=0, color="blue", fill="blue", linetype = 2) +
  annotate("point",-1.9,-0.75,color="darkgoldenrod2") +
  annotate("text", -1.86,-0.75, label= "= local GEV fit",family = "serif",
           hjust = 0) +
  annotate("rect", xmin = -1.95, xmax = -1.5, ymin = -0.8, ymax = -0.7, 
           alpha=0, color="black") +
  lims(x=c(-2.01,-0.6),y=c(-2.01,-0.6)) +
  guides(size="none",color="none",fill="none",shape="none") +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "beta"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "beta"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = c(0.2,0.71),
        legend.direction="vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(family="serif",size = 9),
        legend.title = element_text(family="serif",size = 9),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown()) 



g4 <- ggplot(beta) +
  annotate("point",betaGEV[,get("1")],betaGEV[,get("24")],color="darkgoldenrod2") +
  geom_point(aes(beta1,beta24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=beta[isincon==1],
             aes(beta1,beta24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  guides(size = guide_legend(override.aes = list(shape = 24)),
         shape = guide_legend(override.aes = list(size = 2)),
         color = "none") +
  coord_cartesian(xlim=c(-1.6,-1.01),ylim=c(-1.6,-1.01)) +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "beta"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "beta"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = c(0.2,0.71),
        legend.direction="vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(family="serif",size = 9),
        legend.title = element_text(family="serif",size = 9),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown()) 


figure <- ggarrange(g3,g4,
                    labels=c("(a)","(b)"),
                    widths=c(1,1),
                    nrow=1)
figure

ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "beta_param_est.pdf"),
       width=14,height=7,units="in")
ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "figure9.pdf"),
       width=14,height=7,units="in")


# Parameter plots for QIND ------------------------------------------------

qindGEV <- dcast(gevp, ID ~ d, value.var = "qind")

qind <- dcast(prt, ID ~ d, value.var = "eta")
setnames(qind,c("1","24"),c("qind1","qind24"))

qind <- merge(qind,gfcov[,c("ID","A","QD_fgp")])

qind[,isincon:=ifelse(ID%in%these,1,0)]


g5 <- ggplot(qind) +
  annotate("point",qindGEV[,get("1")],qindGEV[,get("24")],
           color="darkgoldenrod2") +
  geom_point(aes(qind1,qind24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=qind[isincon==1],
             aes(qind1,qind24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  annotate("point",63,2100,color="darkgoldenrod2") +
  annotate("text", 70,2095, label= "= local GEV fit",family = "serif",
           hjust = 0) +
  annotate("rect", xmin = 50, xmax = 275, ymin = 2020, ymax = 2160, 
           alpha=0, color="black", fill="blue") +
  annotate("rect", xmin = 35.1, xmax = 499, ymin = 40, ymax = 515,
           alpha=0, color="blue", fill="blue",linetype=2) +
  annotate("text", x= 50, y = 470, label= "(b)", fontface = 2) +
  scale_x_sqrt(limits = c(35,2575)) +
  scale_y_sqrt(limits = c(35,2575)) +
  guides(size="none",color="none",fill="none",shape="none") +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "qind"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "qind"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = c(0.2,0.71),
        legend.direction="vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(family="serif",size = 9),
        legend.title = element_text(family="serif",size = 9),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown()) 



g6 <- ggplot(qind) +
  annotate("point",qindGEV[,get("1")],qindGEV[,get("24")],
           color="darkgoldenrod2") +
  geom_point(aes(qind1,qind24,
                 shape = as.factor(isincon)),size=2) +
  geom_point(data=qind[isincon==1],
             aes(qind1,qind24,
                 size=A,
                 fill=QD_fgp,
                 shape = as.factor(isincon))) +
  geom_abline(slope=1,size=0.6) +
  coord_cartesian(xlim = c(40,515), ylim = c(40,515)) +
  guides(size = guide_legend(override.aes = list(shape = 24)),
         shape = guide_legend(override.aes = list(size = 2)),
         color="none") +
  labs(x = paste0("</span>","<span style='font-size: 12pt'> ",
                  "qind"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 1 hour"),
       y = paste0("</span>","<span style='font-size: 12pt'> ",
                  "qind"," parameter",
                  "</span>",
                  "<span style='font-size: 18pt'> 24 hours")) +
  scale_fill_scico(name = "Fraction of rain",
                   palette = "lapaz",end=0.95,
                   labels=scaleFUN) +
  scale_shape_manual(name = "Duration consistency of station", 
                     values = c(21,24),
                     labels = c("consistent","inconsistent")) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")) ,
                        range=c(2,8),
                        breaks = c(50,1000,2000)) +
  theme_bw() +
  theme(text = element_text(family="serif",size = 18),
        aspect.ratio = 1,
        legend.position = c(0.2,0.71),
        legend.direction="vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.text = element_text(family="serif",size = 9),
        legend.title = element_text(family="serif",size = 9),
        legend.box.background = element_rect(colour = "black"),
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown()) 




figure <- ggarrange(g5,g6,
                    labels=c("(a)","(b)"),
                    widths=c(1,1),
                    nrow=1)
figure

ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "qind_param_est.pdf"),
       width=14,height=7,units="in")
ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "figure10.pdf"),
       width=14,height=7,units="in")
