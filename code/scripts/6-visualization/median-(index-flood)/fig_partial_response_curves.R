##
##
##
##
##
##
## Plot partial response curves for floodGAM
## and residuals
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)
library(mgcViz)
library(scico)
library(grid)

# Data preparation --------------------------------------------------------

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))


# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID") 
gfam[,specQ:=Qm3_s/A*1000]

# remove the id and lat/long columns
gfcov <- gfcov[,-c("RN","HN","Y_lat","X_long","Y_utm","X_utm",
                   "Y_G_Lat","X_G_Long","Y_G_UTM","X_G_UTM")]


## ----- the response variable here is the median of the annual max

gfam <- gfam[,.(qind = median(specQ)),by=c("ID","d")]

gamdat <- merge(gfcov,gfam,by="ID")

stack <- c("Q_N","A_LE","A_P","H_F","log_R_G_1085","W_Apr","P_Sep")
plotting.stack <- c("Q_N [l/s/km2]","A_LE [%]",
                    "A_P (Area / Circumference * 1000) [km]","H_F [m]",
                    "log_R_G_1085 [log(m/km)]","W_Apr [mm/month]",
                    "P_Sep [mm/month]")

# Fit the GAM and extract smooth effects + residuals -----------------------

smootheffects <- data.table(gx=numeric(),
                            gy=numeric(),
                            gse=numeric(),
                            d=numeric(),
                            type=character())

gamresiduals <- data.table(x=numeric(),
                           y=numeric(),
                           ID=character(),
                           d=numeric(),
                           type=character())


for(di in unique(gamdat[,get("d")])){
  
  gamdat.d <- gamdat[d==di]
  
  ## fit the model
  eta.floodGAM <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                        s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                      method = "REML",
                      select=T,
                      data = gamdat.d,
                      family = gaussian(link=log))
  
  ## use mgcViz to get smooth effects:
  gvobj <- mgcViz::getViz(eta.floodGAM)
  
  ## now calculate the residuals (on the link scale)
  fv <- predict(eta.floodGAM,type="terms")
  
  pred <- predict(eta.floodGAM)
  mu <- predict(eta.floodGAM, type="response")

  Sigma <- rep(eta.floodGAM$scale, length(mu))
  W <- mu*mu/Sigma
  ytilde <- pred + (gamdat.d[,get("qind")]-mu)/mu
  res <- ytilde - pred
  
  ## save smooth effects and residuals for each predictor:
  for(i in 1:length(stack)){
    
    ## smooth effects
    o <- plot( sm(gvobj, i) )
    n <- length(o$ggObj$data$y)
    smootheffects <- rbind(smootheffects,data.table(gx=o$ggObj$data$x,
                                gy=o$ggObj$data$y,
                                gse=o$ggObj$data$se,
                                d=rep(di,n),
                                type=rep(plotting.stack[i],n)))
    
    
    ## residuals
    prsd <- res + fv[,i]
    m <- length(mu)
    
    gamresiduals <- rbind(gamresiduals,data.table(x=gamdat.d[,get(stack[i])],
                                                  y=prsd,
                                                  ID=gamdat.d[,get("ID")],
                                                  d=rep(di,m),
                                                  type=rep(plotting.stack[i],m)))
  }
  
}



# plot the smooth effects and residuals -----------------------------------

## add an empty panel purely for plotting purposes (looks better
## if climatic characteristics P_Sep and W_Apr are on their own row)
empt <- smootheffects[1,]; empt[,type:=as.factor("imempty")]
smootheffects <- rbind(smootheffects, empt)
smootheffects[,
             type:=factor(type,levels=c(plotting.stack[1:5],
                                        "imempty",plotting.stack[6:7]))]

empt <- gamresiduals[1,]; empt[,type:=as.factor("imempty")]
gamresiduals <- rbind(gamresiduals, empt)
gamresiduals[,
              type:=factor(type,levels=c(plotting.stack[1:5],
                                         "imempty",plotting.stack[6:7]))]

## add the catchmetn descriptors to gamresiduals so they can be
## plotted with color/size
gamresiduals <- merge(gamresiduals,gamdat.d,by="ID")

## Each of the panels needs its own range, so makes this hack where we plot
## empty points to get around the automatic scales in facet_wrap:
rngDT <- data.table(type=c(plotting.stack[1:5],
                           "imempty",plotting.stack[6:7]))
rngDT[,ptlow:=c(-1.3,-1.5,-1.3,-1.3,-1.3,-1.3,-1.3,-1.3)]
rngDT[,ptup:=c(1.5,1,1.3,1.5,1.5,1.5,1.5,1.5)]
rngDT[,xcoord:=gamresiduals[,median(x),by="type"]$V1]
rngDT[,type:=factor(type,levels=c(plotting.stack[1:5],
                                  "imempty",plotting.stack[6:7]))]


## set the values for the confidence intervals:
level = 0.95
mul <- qnorm((level+1)/2)

scaleFUN <- function(x) sprintf("%.1f", x)


gclip <- ggplot(smootheffects[d==1|d==24]) +
  geom_point(data=rngDT,aes(xcoord,ptlow),color="white") +
  geom_point(data=rngDT,aes(xcoord,ptup),color="white") +
  geom_point(data=gamresiduals[d.x==24],
             aes(x,y,size=A,color=QD_fgp))+
  annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0,
           colour = "darkgrey",linetype=3) +
  geom_rug(data=gamresiduals[d.x==24],aes(x),sides="b") +
  geom_line(aes(gx,gy,group=as.factor(d),linetype=as.factor(d))) +
  geom_ribbon(aes(gx,gy,ymin=gy-mul*gse,ymax=gy+mul*gse,
                  group=as.factor(d),fill=as.factor(d)),
              alpha=0.1) +
  labs(y = "Effect on median flood (log scale)",
       x = "Predictor values") +
  scale_color_scico(name = "Fraction of rain",
                    palette = "lapaz",end=0.95,
                    labels=scaleFUN) +
  scale_fill_manual(name = "Flood duration (hours)", 
                    values=scico(2, palette = "turku",
                                 begin=0.1,end=0.55,direction=1)) +
  scale_linetype_manual(name = "Flood duration (hours)", values =c(1,2)) +
  scale_size_continuous(name = expression(paste("Catchment area [", km^2, "]",
                                                sep = "")),
                        range=c(1,11),
                        breaks = c(50,100,2000))+
  scale_y_continuous(
    breaks = ~ trunc(c(.x[1],0,.x[2]),4),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(type),scales="free") +
  theme_classic() +
  facet_wrap(vars(type),scales="free") +
  theme(strip.background =element_rect(fill = "white"),
        strip.text.x = element_text( margin = margin( b = 6, t = 4) ), 
        axis.line.x = element_line(colour = "black"),
        axis.text.x   = element_text(size=12,margin = unit(c(1, 0, 0, 0), "mm")),
        text = element_text(size=20,family="serif"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        legend.position=c(0.75,0.16)) #0.85,0.35


g <- ggplotGrob(gclip)
rm_grobs <- g$layout$name %in% c("panel-2-3","strip-t-3-2",
                                 "axis-l-2-3","axis-b-3-2")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel

grid.newpage()
grid.draw(g)


#ggsave(paste0(figPath,"floodGAM_partial_effects_residual.pdf"),
#width=23,height=18,units="in")









