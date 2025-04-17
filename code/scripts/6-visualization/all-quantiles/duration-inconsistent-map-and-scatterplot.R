##
##
##
##
##
## Make the map of duration inconsistencies (Fig 6) and the gridplot
## -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(scico)
library(ggpubr)
library(ggh4x)
library(patchwork)

load("~/floodGAM/results/output/all-quantiles/objects-duration-incon.rda")

load("~/floodGAM/results/output/all-quantiles/plotobj-duration-incon.rda")
# what's the difference between these two?

## load map data for Norway
load(paste0("~/floodGAM/data/raw-data/","map_files/",
            'station_selection_map_datafiles.rda'))
NorgeDF <- fortify(Norge)

## load in the station coordinates
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))


# Gridplot duration inconsistency -----------------------------------------

# add dummy variable for facet plotting
median.grid[,tvar:="T = 2 years"]
andre.grid[,tvar:="2 years < T <= 100 years"]
tredje.grid[,tvar:="100 years < T"]

scico.limit <- max(abs(rbind(median.grid,andre.grid,tredje.grid)$z),na.rm=T)

g1 <- ggplot(median.grid, 
             aes(as.factor(j),as.factor(i),fill=z)) +
  geom_tile() +
  scale_fill_scico(name = "Number of inconsistent stations  ",
                   palette = "vik",direction=1,
                   begin = 0.49,end=1,limit=c(0,scico.limit),
                   na.value="transparent") +
  labs(x = "duration (hours)", y = "duration (hours)") +
  theme_bw() +
  facet_nested(~ tvar + model, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_blank(),
        legend.key.width=unit(1,"cm"),
        legend.spacing = unit(4,"cm")) +
  guides(fill=guide_colourbar(byrow=T)) 


g2 <- ggplot(andre.grid, 
             aes(as.factor(j),as.factor(i),fill=z)) +
  geom_tile() +
  scale_fill_scico(name = "Number of inconsistent stations  ",
                   palette = "vik",direction=1,
                   begin = 0.49,end=1,limit=c(0,scico.limit),
                   na.value="transparent") +
  labs(x = "duration (hours)", y = "duration (hours)") +
  theme_bw() +
  facet_nested(~ tvar + model, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_blank(),
        legend.key.width=unit(1,"cm"),
        legend.spacing = unit(4,"cm")) +
  guides(fill=guide_colourbar(byrow=T)) 


g3 <- ggplot(tredje.grid, 
             aes(as.factor(j),as.factor(i),fill=z)) +
  geom_tile() +
  scale_fill_scico(name = "Number of inconsistent stations  ",
                   palette = "vik",direction=1,
                   begin = 0.49,end=1,limit=c(0,scico.limit),
                   na.value="transparent",
                   breaks = c(0,5,15,25)) +
  labs(x = "duration (hours)", y = "duration (hours)") +
  theme_bw() +
  facet_nested(~ tvar + model, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_blank(),
        legend.key.width=unit(1,"cm"),
        legend.spacing = unit(4,"cm")) +
  guides(fill=guide_colourbar(byrow=T)) 




figure <- ggarrange(g1, g2, g3,
                    labels = c("(a)","(b)","(c)"),
                    widths=c(0.6,1,1),
                    nrow=1,
                    common.legend = T,legend="bottom")

figure

# save manually, landscape 16 x 5in


# Make the map and example return level plots -----------------------------

Qdc <- alltt[type=="QRT",.N,by="ID"]
Qdc[,isQ:="only QRT"]

Pdc <- alltt[type=="PRT",.N,by="ID"]
Pdc[,isP:="pincon"]

dc <- merge(Qdc,Pdc,by="ID",all=T)

dc[,isB:=ifelse( !is.na(isP), "both PRT & QRT", "only QRT" )]

dc <- merge(dc[,c("ID","isB")],gfcov[,c("ID","X_utm","Y_utm","A")],by="ID")

## these stations:
theseID <- c("85-4","122-9","2-280") #"159-3","26-20",

dc[,isoutline:=ifelse(ID%in%theseID,"yes","no")]
dc[,sizeB:=ifelse(isB=="only QRT",3,2)]

dc[,navn:=NA]
dc[,navn:=ifelse(ID=="85-4","(a)",navn)]
dc[,navn:=ifelse(ID=="122-9","(b)",navn)]
dc[,navn:=ifelse(ID=="2-280","(c)",navn)]

ctab <- scico(palette = "lipari",2,end = 0.6)

g <- ggplot() +
  geom_polygon(data=NorgeDF,aes(long,lat,group=group),
               fill = "white",color="grey",alpha=0.3) +
  annotate("point", x = gfcov$X_utm,y = gfcov$Y_utm, color = "grey") +
  geom_point(data=dc,aes(X_utm,Y_utm,fill=isB,color=isB,size=sizeB),
             pch=24) +
  annotate("point", x = gfcov[ID%in%theseID]$X_utm,
           y = gfcov[ID%in%theseID]$Y_utm,shape=0,size=6,color="red") +
  annotate(geom = "text", x = gfcov[ID%in%theseID]$X_utm,
           y = gfcov[ID%in%theseID]$Y_utm, label = dc[ID%in%theseID]$navn,
           color="red",
           hjust = 1.5, fontface =2, family="serif", size = unit(6, "pt")) +
  coord_equal()+
  theme_void() +
  scale_fill_manual(breaks = c("both PRT & QRT", "only PRT","only QRT"),
                    values = c("both PRT & QRT" = ctab[1], 
                               "only PRT" = "maroon1",
                               "only QRT" = ctab[2]),
                    name = "Approach that produces inconsistency",
                    na.translate = F) +
  scale_color_manual(breaks = c("both PRT & QRT", "only PRT","only QRT"),
                    values = c("both PRT & QRT" = ctab[1], 
                               "only PRT" = "maroon1",
                               "only QRT" = ctab[2]),
                    name = "Approach that produces inconsistency",
                    na.translate = F) +
  scale_size_continuous(range=c(1.75,3),guide="none") +
  theme(text = element_text(family="serif",size = 16),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.margin = margin(5,5,5,5))


g



## now get return level plots
## get one plot with an inconsistency at the median, one with an 
## inconsistency between 2 and 100 years, and one above 100 years


grl1 <- grldat[ID%in%"85-4"&d%in%c(1,6,24)&tag!="local fit"]
grl1[,tvar:="T > 100 years"]
grl1[,tag:=ifelse(tag=="prt","PRT","QRT")]

ghundredplus <- ggplot(grl1) +
  # geom_ribbon(data=GEVresp[ID%in%these],
  #             aes(rp,ymin=`2.5%.rl`,ymax=`97.5%.rl`,group=d),alpha=0.1) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  #geom_point(data=gpt,aes(observed.y,V2,shape=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10,20,50,100,250,1000),
                labels= c("2","10","20","50","100","250","1000"),
                limits = c(2,1150)) +
  scale_shape_manual(values=c(1,4)) +
  scale_color_manual(values = ctab) +
  labs(x = "Return period (years)",
      y = expression(paste("Return level [l/s/", km^2, "]",
                           sep = ""))) +
  theme_bw() +
  facet_nested(~ tvar, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  guides(fill=guide_legend(title=" "),
         linetype=guide_legend(title="Duration (hours)"),
         color = guide_legend(title="Approach"),
         shape = guide_legend(title = " ")) +
  theme(text = element_text(family="serif",size = 16),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(family="serif",size = 12),
        legend.title = element_text(family="serif",size = 12),
        legend.spacing.y = unit(-.1, 'cm'),
        panel.spacing = unit(1.2, "lines"),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(family="serif",size = 16),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.tag.position  = c(.1, .96)) 


grl2 <- grldat[ID%in%"122-9"&d%in%c(1,6,24)&tag!="local fit"]
grl2[,tvar:="2 years < T <= 100 years"]

gtwohun <- ggplot(grl2) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10,20,50,100,250,1000),
                labels= c("2","10","20","50","100","250","1000"),
                limits = c(2,1150)) +
  scale_shape_manual(values=c(1,4)) +
  scale_color_manual(values = ctab) +
  labs(x = "Return period (years)",
       y = expression(paste("Return level [l/s/", km^2, "]",
                            sep = ""))) +
  theme_bw() +
  facet_nested(~ tvar, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  guides(fill=guide_legend(title=" "),
         linetype=guide_legend(title=" "),
         color = guide_legend(title=" "),
         shape = guide_legend(title = " ")) +
  theme(text = element_text(family="serif",size = 16),
        legend.position = "none",
        legend.background = element_blank(),
        #legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(family="serif",size = 12),
        legend.title = element_text(family="serif",size = 12),
        legend.spacing.y = unit(-.1, 'cm'),
        panel.spacing = unit(1.2, "lines"),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(family="serif",size = 16),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.tag.position  = c(.1, .96)) 

grl3 <- grldat[ID%in%"2-280"&d%in%c(1,12,24)&tag!="local fit"]
grl3[,tvar:="T = 2 years"]

gtwo <- ggplot(grl3) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10,20,50,100,250,1000),
                labels= c("2","10","20","50","100","250","1000"),
                limits = c(2,1150)) +
  scale_shape_manual(values=c(1,4)) +
  scale_color_manual(values = ctab) +
  labs(x = "Return period (years)",
       y = expression(paste("Return level [l/s/", km^2, "]",
                            sep = ""))) +
  theme_bw() +
  facet_nested(~ tvar, 
               nest_line = element_line(linetype = 1)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "lightgrey"))+
  guides(fill=guide_legend(title=" "),
         linetype=guide_legend(title=" "),
         color = guide_legend(title=" "),
         shape = guide_legend(title = " ")) +
  theme(text = element_text(family="serif",size = 16),
        legend.position = "none",
        legend.background = element_blank(),
        #legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(family="serif",size = 12),
        legend.title = element_text(family="serif",size = 12),
        legend.spacing.y = unit(-.1, 'cm'),
        panel.spacing = unit(1.2, "lines"),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(family="serif",size = 16),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.tag.position  = c(.1, .96)) 


gi <- g + inset_element(ghundredplus, left = -0.05, bottom = 0.58, right = 0.4, top = 1)

gii <- gi + inset_element(gtwohun, left = 0.58, bottom = 0.4, right = 1.01, top = 0.72)  

gii + inset_element(gtwo, left = 0.42, bottom = 0.05, right = 0.88, top = 0.37) +
  plot_annotation(tag_levels = list(c(' ','(a)','(b)','(c)')))


## saved manually as d_incon_map. portrait, 11 in x 9 in



























