##
##
##
##
##
## Fit floodGAM and RFFA_2018 on 9 folds and oos predict on the 10th
## Then check internal model consistency
## 
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)
library(caret)
library(ggplot2)

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "durations_gamfelt_annual_maxima.rds"))
# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# select only the floodGAM & RFFA_2018 covariates
gfcov <- gfcov[,c("ID","Q_N","A_LE","A_P","H_F", #floodGAM
                  "R_G_1085","log_R_G_1085","W_Apr","P_Sep", #floodGAM
                  "Q_N_cuberoot","R_L_sqrt","T_Feb_sqrd", #RFFA_2018 eta
                  "T_Mar_cubed","W_Mai_sqrt", #RFFA_2018 eta
                  "A_Glac","A_For","H_10","P_Jul","W_Jun", #RFFA_2018 beta
                  "R_TL_net")] #RFFA_2018 xi

# standardize cov values by dividing by 2 standard deviations
coltab = names(gfcov)[-1]
gfcov[, 
      (coltab) := lapply(.SD, function(Xw) (Xw - mean(Xw)) / (sd(Xw) * 2)), 
      .SDcols = coltab]

gamfelt <- merge(gfcov,gfam,by="ID")

## ----- load in the response variables (GEV parameters from Stan fits to each 
##       station in gfam)

gevp0 <- readRDS("~/floodGAM/results/output/gamfeltstanresult.rds")
gevp24 <- readRDS("~/floodGAM/results/output/gamfeltstanresult_24.rds")

# go from long to wide format, selecting only the posterior mean:
gevp0 <- dcast(gevp0, ID ~ param, value.var = "mean")
gevp24 <- dcast(gevp24, ID ~ param, value.var = "mean")

gevp <- rbind(gevp0[,d:=0],gevp24[,d:=24])

# merge the response variable with the predictors (gamfelt covariate matrix)
gamdat <- merge(gevp,gfcov,by="ID")


# Define the data folds ---------------------------------------------------
set.seed(42)
k = 10
# use the GEV estimated median to sort the stations into folds
fidx <- createFolds(gamdat[d==0,get("qind")],k) 


# Fit the GAMs on the folds and save just the predicted values ------------

oos.predictions <- data.table(eta=numeric(),beta=numeric(),xi=numeric(),
                              model=character(),fold=numeric(),d=numeric(),
                              ID=character())

for(di in c(0,24)){
  
  gamdat_d <- gamdat[d==di]
  
  for(i in 1:k){
    
    train.gamdat_d <- gamdat_d[-fidx[[i]]]
    test.gamdat_d <- gamdat_d[fidx[[i]]]
    
    ## ------- eta --------
    eta.floodGAM <- gam(qind ~ s(Q_N,k=6)+s(A_LE,k=6)+s(A_P,k=6)+s(H_F,k=6)+
                          s(log_R_G_1085,k=6)+s(W_Apr,k=3)+s(P_Sep,k=3),
                        method = "REML",
                        data = gamdat_d,
                        family = gaussian(link=log))
    
    eta.RFFA2018 <- gam(qind ~ I(Q_N_cuberoot) + I(R_L_sqrt) + A_LE + 
                          I(T_Feb_sqrd) + I(T_Mar_cubed) + I(W_Mai_sqrt),
                        method = "REML",
                        data = gamdat_d,
                        family = gaussian(link=log))
    
    ## ------- beta --------
    beta.floodGAM <- gam(beta ~ s(Q_N,k=6)+s(log_R_G_1085,k=6)+s(P_Sep,k=6),
                         method = "REML",
                         data = gamdat_d,
                         family = gaussian(link=identity))
    
    beta.RFFA2018 <- gam(beta ~ I(A_Glac) + I(A_For) + I(H_10) + 
                           I(P_Jul) + I(W_Jun),
                         method = "REML",
                         data = gamdat_d,
                         family = gaussian(link = identity))
    
    ## ------- xi --------
    xi.floodGAM <- gam(xi ~ s(A_LE,k=6)+s(log_R_G_1085,k=6)+s(W_Apr,k=6),
                       method = "REML",
                       data = gamdat_d,
                       family = gaussian(link=identity))
    
    xi.RFFA2018 <- gam(xi ~ I(A_LE) + I(R_TL_net),
                       method = "REML",
                       data = gamdat_d,
                       family = gaussian(link = identity))
    
    ## ------- generate and save the predictions --------
    n = dim(test.gamdat_d)[1]
    oos.predictions <- rbind(oos.predictions,
                             data.table(eta=predict.gam(eta.floodGAM, 
                                                        newdata = test.gamdat_d, 
                                                        type="response"),
                                        beta=predict.gam(beta.floodGAM, 
                                                         newdata = test.gamdat_d, 
                                                         type="response"),
                                        xi=predict.gam(xi.floodGAM, 
                                                       newdata = test.gamdat_d, 
                                                       type="response"),
                                        model=rep("floodGAM",n),
                                        fold=rep(i,n),
                                        d=rep(di,n),
                                        ID=test.gamdat_d[,get("ID")]))
    
    oos.predictions <- rbind(oos.predictions,
                             data.table(eta=predict.gam(eta.RFFA2018, 
                                                        newdata = test.gamdat_d, 
                                                        type="response"),
                                        beta=predict.gam(beta.RFFA2018, 
                                                         newdata = test.gamdat_d, 
                                                         type="response"),
                                        xi=predict.gam(xi.RFFA2018, 
                                                       newdata = test.gamdat_d, 
                                                       type="response"),
                                        model=rep("RFFA2018",n),
                                        fold=rep(i,n),
                                        d=rep(di,n),
                                        ID=test.gamdat_d[,get("ID")]))
    
  }
}



# Check the support of the parameter combinations -------------------------

# add in the range of observed data values for each station
oos.predictions <- merge(oos.predictions, 
                         gfam[,.(min.y = min(specQ), 
                                 max.y = max(specQ)),by="ID"],
                         by="ID")

# convert to location-scale parameterization:
oos.predictions[,sigma:=eta*exp(beta)]
oos.predictions[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

## calculate the lower (or upper) bound imposed by the parameter combos:
oos.predictions[,bound:=mu-sigma/xi]


which(oos.predictions[,(1 + xi*(max.y-mu)/sigma)]<=0)

which(oos.predictions[,(1 + xi*(min.y-mu)/sigma)]<=0)

oos.predictions[c(634,636)]

oos.predictions[ID=="23400018"]

oos.predictions[xi<0,c("ID","eta","xi","model","d","min.y","max.y","bound")]



# Compute return levels ---------------------------------------------------

oos.rp <- oos.predictions[ID=="23400018",
                mu+sigma/xi * ((-log(1-1/2:500))^(-xi)-1),
                by = c("model","d")]
oos.rp[,rp:=rep(2:500,4)]
setnames(oos.rp,"V1","rl")

ggplot(oos.rp) +
  geom_line(aes(rp,rl,color=model,linetype=as.factor(d))) +
  scale_x_log10(expand = c(0, 0), breaks = c(2,5,10,20,50,100,250,500),
                labels= c("2","5","10","20","50","100","250","500"),
                limits = c(2,510)) +
  labs(x = "Return period (years)",
       y = bquote("Return level"~"("~l/s/km^2~")")) +
  theme_bw()
gg''''vfdc bujhntfgbqasrh