##
##
##
##
##
##
## Duration inconsistencies in PRT and QRT
## -----------------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(scico)
library(ggpubr)

load("~/floodGAM/results/output/all-quantiles/qrt-prt-oos.rda")

## load in the local Stan fits:
gevp <- readRDS("~/floodGAM/results/output/all-quantiles/gamfeltstanresult.rds")

# small bit of data processing to get return levels ------------------------

## go from long to wide format
prt <- dcast(prt.predictions, ID + d ~ param, value.var = "val")

## convert to location-scale parameterization
prt[,sigma:=eta*exp(beta)]
prt[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

rp = unique(qrt.predictions[,get("rp")])

# compute the return periods from the GEV parameters
prt <- prt[,list(mu+sigma/xi * ((-log(1-1/rp))^(-xi)-1),rp),
           by=c("d","ID")]
setnames(prt,"V1","rl.prt")

## only the return periods we interested in
prt <- prt[rp>=2&rp<=750]
qrt <- qrt.predictions[rp>=2&rp<=750]

# go from long to wide wide
prtW <- dcast(prt, ID + rp ~ d, value.var = "rl.prt")
qrtW <- dcast(qrt, ID + rp ~ d, value.var = "adj.val")


# Data processing to get local return levels ------------------------------

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID + d ~ param, value.var = "mean")

# compute the return periods from the GEV parameters
gevp.rl <- gevp[,list(mu+sigma/xi * ((-log(1-1/rp))^(-xi)-1),rp),
                by=c("d","ID")]


# Tally duration inconsistencies ------------------------------------------

# this is messy because we need it in long format for ggplot....
# could do this in one for loop but easier to think about split...

dvec <- unique(prt$d) # unique durations
n <- length(dvec) # number of unique durations


# Median flood first ------------------------------------------------------

median.grid <- data.table(i=rep(dvec, each = n),
                     j=rep(dvec, n),
                     model=rep("Median flood",n*n))


zvec <- rep(NA,dim(median.grid)[1])
zi = 1

for(i in dvec){
  for(j in dvec){
    if(i<=j){
      zvec[zi] = NA
      zi=zi+1
    }else{
      tt = prtW[get(paste0(i)) > get(paste0(j)), .N, by = c("rp","ID")]
      tt = tt[,min(rp),by="ID"]

      zvec[zi] = dim(tt[V1==2])[1]
      
      zi=zi+1
    }
  }
}

median.grid[,z:=zvec]


# Now 2 < T >= 100 ------------------------------------------------------

andre.grid <- data.table(i=c(rep(dvec, each = n),rep(dvec, each = n)),
                          j=c(rep(dvec, n),rep( dvec, n)),
                          model=rep(c("QRT","PRT"),each=n*n))


zvec <- rep(NA,dim(andre.grid)[1])
zi = 1

for(navn in c("QRT","PRT")){
  for(i in dvec){
    for(j in dvec){
      if(i<=j){
        zvec[zi] = NA
        zi=zi+1
      }else{
        if(navn == "QRT"){
          
          tt = qrtW[get(paste0(i)) > get(paste0(j)), .N, by = c("rp","ID")]
          
          tt = tt[,min(rp),by="ID"]
          
          zvec[zi] = dim(tt[V1>2&V1<=100])[1]
          
          zi=zi+1
        }else{
          tt = prtW[get(paste0(i)) > get(paste0(j)), .N, by = c("rp","ID")]
          
          tt = tt[,min(rp),by="ID"]
          
          zvec[zi] = dim(tt[V1>2&V1<=100])[1]
          
          zi=zi+1
        }
        
      }
    }
  }
}

andre.grid[,z:=zvec]



# Now T > 100 years ------------------------------------------------------

tredje.grid <- data.table(i=c(rep(dvec, each = n),rep(dvec, each = n)),
                         j=c(rep(dvec, n),rep( dvec, n)),
                         model=rep(c("QRT","PRT"),each=n*n))


alltt <- data.table(ID=character(),V1=numeric(),i=numeric(),j=numeric(),
                    type=character())

zvec <- rep(NA,dim(tredje.grid)[1])
zi = 1

for(navn in c("QRT","PRT")){
  for(i in dvec){
    for(j in dvec){
      if(i<=j){
        zvec[zi] = NA
        zi=zi+1
      }else{
        if(navn == "QRT"){
          
          tt = qrtW[get(paste0(i)) > get(paste0(j)), .N, by = c("rp","ID")]
          
          tt = tt[,min(rp),by="ID"]
          
          tt[,i:=i]
          tt[,j:=j]
          tt[,type:=navn]
          
          alltt <- rbind(alltt,tt)
          
          zvec[zi] = dim(tt[V1>100])[1]
          
          zi=zi+1
        }else{
          tt = prtW[get(paste0(i)) > get(paste0(j)), .N, by = c("rp","ID")]
          
          tt = tt[,min(rp),by="ID"]
          
          tt[,i:=i]
          tt[,j:=j]
          tt[,type:=navn]
          
          alltt <- rbind(alltt,tt)
          
          zvec[zi] = dim(tt[V1>100])[1]
          
          zi=zi+1
        }
        
      }
    }
  }
}

tredje.grid[,z:=zvec]



# Save objects ------------------------------------------------------------

save(median.grid,andre.grid,tredje.grid,alltt,
     file="~/floodGAM/results/output/all-quantiles/plotobj-duration-incon.rda")



# ggplot test -------------------------------------------------------------

scico.limit <- max(abs(rbind(median.grid,andre.grid,tredje.grid)$z),na.rm=T)

g1 <- ggplot(median.grid, 
             aes(as.factor(j),as.factor(i),fill=z)) +
  geom_tile() +
  scale_fill_scico(name = "Number of inconsistent stations  ",
                   palette = "vik",direction=1,
                   begin = 0.49,end=1,limit=c(0,scico.limit),
                   na.value="transparent") +
  labs(x = "duration (hours)", y = "duration (hours)") +
  facet_wrap(vars(model)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_rect(fill = "white"),
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
  facet_wrap(vars(model)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_rect(fill = "white"),
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
  facet_wrap(vars(model)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family="serif",size = 20),
        strip.background =element_rect(fill = "white"),
        legend.key.width=unit(1,"cm"),
        legend.spacing = unit(4,"cm")) +
  guides(fill=guide_colourbar(byrow=T)) 




figure <- ggarrange(g1, g2, g3,
                    labels = c("(a)","(b)","(c)"),
                    widths=c(0.5,1,1),
                    nrow=1,
                    common.legend = T,legend="bottom")

figure




# Look at alltt -----------------------------------------------------------

qrt.type3 <- alltt[type=="QRT"&V1>100,.N,by="ID"]

qrt.type2 <- alltt[type=="QRT"&V1<=100&V1>2,.N,by="ID"]

type.med <- alltt[type=="QRT"&V1==2,.N,by="ID"]

qrtn <- merge(merge(type.med,qrt.type2,all=T),qrt.type3,all=T)

merge(qrt.type3,type.med,by="ID",all=T)


prt.type3 <- alltt[type=="PRT"&V1>100,.N,by="ID"]

prt.type2 <- alltt[type=="PRT"&V1<=100&V1>2,.N,by="ID"]

prtn <- merge(merge(type.med,prt.type2,all=T),prt.type3,all=T)



these <- merge(qrtn,prtn)[1:10,]$ID



qrt[,tag:="qrt"]
prt[,tag:="prt"]
gevp.rl[,tag:="local fit"]

grldat <- rbind(qrt[,c("d","ID","rp","adj.val","tag")],
                prt[,c("d","ID","rp","rl.prt","tag")],
                gevp.rl[,c("d","ID","rp","V1","tag")],use.names=F)



save(median.grid,andre.grid,tredje.grid,alltt,grldat,
     file="~/floodGAM/results/output/all-quantiles/plotobj-duration-incon.rda")



# Type 3 error ------------------------------------------------------------


merge(qrt.type3,type.med,by="ID",all=T)

ggplot(grldat[ID%in%"85-4"&d%in%c(1,6,12)]) +
  # geom_ribbon(data=GEVresp[ID%in%these],
  #             aes(rp,ymin=`2.5%.rl`,ymax=`97.5%.rl`,group=d),alpha=0.1) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  #geom_point(data=gpt,aes(observed.y,V2,shape=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10,20,50,100,250,1000),
                labels= c("2","10","20","50","100","250","1000"),
                limits = c(2,1150)) +
  scale_shape_manual(values=c(1,4)) +
  theme_bw() +
  facet_wrap(vars(ID),scales="free")




# type 2 error: -----------------------------------------------------------

tv <- merge(qrt.type2,type.med,by="ID",all=T)
these <- tv[is.na(N.y)]$ID

ggplot(grldat[ID%in%"122-9"&d%in%c(1,6,24)&tag!="local fit"]) +
  # geom_ribbon(data=GEVresp[ID%in%these],
  #             aes(rp,ymin=`2.5%.rl`,ymax=`97.5%.rl`,group=d),alpha=0.1) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  #geom_point(data=gpt,aes(observed.y,V2,shape=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10,20,50,100,250,1000),
                labels= c("2","10","20","50","100","250","1000"),
                limits = c(2,1150)) +
  scale_shape_manual(values=c(1,4)) +
  theme_bw() +
  facet_wrap(vars(ID),scales="free")


# Median error: -----------------------------------------------------------

these <- type.med$ID[26:50]

ggplot(grldat[ID%in%"2-280"&d%in%c(1,12,24)&tag!="local fit"&rp<10]) +
  # geom_ribbon(data=GEVresp[ID%in%these],
  #             aes(rp,ymin=`2.5%.rl`,ymax=`97.5%.rl`,group=d),alpha=0.1) +
  geom_line(aes(rp,adj.val,color=tag,group=interaction(d,tag),linetype=as.factor(d))) +
  #geom_point(data=gpt,aes(observed.y,V2,shape=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,10),
                labels= c("2","10"),
                limits = c(2,15)) +
  scale_shape_manual(values=c(1,4)) +
  theme_bw() +
  facet_wrap(vars(ID),scales="free")


