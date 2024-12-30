##
##
##
##
##
## Create figure for section 4.2: reliability
## probability integral transform (PIT) histograms
## -----------------------------------------------------------------------------

library(data.table)
library(scico)


# Custom functions --------------------------------------------------------

PITplot <- function(pit,navn,d1,d2){
  
  N <- dim(pit[d==1])[1]/length(unique(pit$model)) # number of stations
  NC <- 10
  
  ctab = scico(2, palette = "turku",
               begin=0.3,end=0.5,direction=1)
  
  p1 <- hist(pit[d==d1&model==navn,get("V1")], nclass=NC, plot=F)
  p2 <- hist(pit[d==d2&model==navn,get("V1")], nclass=NC, plot=F)
  plot(0,0,type="n",xlim=c(0,1),ylim=c(0,40),xlab="",ylab="")
  title(navn,line=0.5)
  plot(p1,col=ctab[1],add=TRUE)
  plot(p2,col=ctab[2],density=10,angle=135,add=TRUE)
  abline(a=N/NC, b=0)
  legend("topright", 
         legend = c(paste0(d1," hours"), paste0(d2," hours")), 
         col = ctab, 
         pch = c(15,15), 
         bty = "n", 
         pt.cex = 2, 
         cex = 1.2, 
         text.col = "black", 
         horiz = F , 
         inset = c(-0.05, 0.02))
}



# Load data ---------------------------------------------------------------

## ----- load in the predictions and error metrics:
oos.pred <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                           "gamfelt_hydagsupp_median_flood_predictive_accuracy.rds"))


# use the model mu and model sigma to PIT the eta.obs:

pit <- oos.pred[,pnorm(log(eta.obs),mu.gam,sigma.gam),by=c("model","d")]

par(mfrow=c(1,2),
    mar = c(3,3,3,1),
    family="serif")

PITplot(pit,"RFFA2018",1,24)

PITplot(pit,"floodGAM",1,24)


# saved manually as PIT_histograms.pdf, landscape, 11 x 4.5 in