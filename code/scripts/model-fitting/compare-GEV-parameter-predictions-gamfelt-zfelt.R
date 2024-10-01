##
##
##
##
##
## Use the gamfelt dataset to fit GAMs for each GEV parameter.
## 
## Compare the range of the in-sample predictions from the gamfelt
## to the out-of-sample predictions for zfelt
## -----------------------------------------------------------------------------

library(data.table)
library(mgcv)


## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                       "gamfelt_annual_maxima.rds"))
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

gamfelt <- merge(gfcov,gfam,by="ID")


## ----- load in the zfelt dataset
zfcov <- readRDS(paste0("~/floodGAM/data/processed-data/zfelt/",
                        "zfelt_catchment_covariates.rds"))


## ----- load in the response variables (GEV parameters from Stan fits to each 
##       station in gfam)

gevp <- readRDS("~/floodGAM/results/output/gamfeltstanresult.rds")

# go from long to wide format, selecting only the posterior mean:
gevp <- dcast(gevp, ID ~ param, value.var = "mean")

# merge the response variable with the predictors (gamfelt covariate matrix)
gamdat <- merge(gevp,gfcov,by="ID")



# Fit the GAMs on the gamfelt data ----------------------------------------

## ---- eta
eta <- gam(qind ~ s(Q_N,k=6)+
             s(A_LE,k=6)+
             s(A_P,k=6)+
             s(H_F,k=6)+
             s(log_R_G_1085,k=6)+
             s(W_Apr,k=3)+s(P_Sep,k=3),
           method = "REML",
           data = gamdat,
           family = gaussian(link=log))


## ---- beta
beta <- gam(beta ~ s(Q_N,k=6)+
              s(log_R_G_1085,k=6)+
              s(P_Sep,k=6),
            method = "REML",
            data = gamdat,
            family = gaussian(link=identity))

## ---- xi
xi <- gam(xi ~ s(A_LE,k=6)+
            s(log_R_G_1085,k=6) +
            s(W_Apr,k=6),
          method = "REML",
          data = gamdat,
          family = gaussian(link=identity))


# Fit RFFA_2018 models ----------------------------------------------------

eta.RFFA2018 <- gam(qind ~ I(Q_N_cuberoot) + I(R_L_sqrt) + A_LE + 
                      I(T_Feb_sqrd) + I(T_Mar_cubed) + I(W_Mai_sqrt),
                    method = "REML",
                    data = gamdat,
                    family = gaussian(link=log))

beta.RFFA2018 <- gam(beta ~ I(A_Glac) + I(A_For) + I(H_10) + 
                       I(P_Jul) + I(W_Jun),
                     method = "REML",
                     data = gamdat,
                     family = gaussian(link = identity))

xi.RFFA2018 <- gam(xi ~ I(A_LE) + I(R_TL_net),
                   method = "REML",
                   data = gamdat,
                   family = gaussian(link = identity))


# Check the support of the eta, beta, xi combos for gamfelt ------------------

## this predicts at the in-sample locations for gamfelt
g.eta <- predict.gam(eta, newdata = gamdat, type="response")
g.beta <- predict.gam(beta, newdata = gamdat, type="response")
g.xi <- predict.gam(xi, newdata = gamdat, type="response")

## add predictions and observed data to a data table:
gDT <- gamdat[,"ID"]
gDT[,c("eta","beta","xi"):=list(g.eta,g.beta,g.xi)]
gDT <- merge(gDT,
             gfam[,.(min.y = min(specQ), max.y = max(specQ)),by="ID"],
             by="ID")
# convert to location-scale parameterization:
gDT[,sigma:=eta*exp(beta)]
gDT[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# none of the parameter combinations for gamfelt data are outside the support:
which(gDT[,(1 + xi*(max.y-mu)/sigma)]<=0)

which(gDT[,(1 + xi*(min.y-mu)/sigma)]<=0)

## calculate the lower (or upper) bound imposed by the parameter combos:
gDT[,bound:=mu-sigma/xi]

## there are no estimated xi parameters < 0:
gDT[xi<0]


# Check the support of the eta, beta, xi combos for RFFA_2018 ------------------

## this predicts at the in-sample locations 
r.eta <- predict.gam(eta.RFFA2018, newdata = gamdat, type="response")
r.beta <- predict.gam(beta.RFFA2018, newdata = gamdat, type="response")
r.xi <- predict.gam(xi.RFFA2018, newdata = gamdat, type="response")

## add predictions and observed data to a data table:
rDT <- gamdat[,"ID"]
rDT[,c("eta","beta","xi"):=list(r.eta,r.beta,r.xi)]
rDT <- merge(rDT,
             gfam[,.(min.y = min(specQ), max.y = max(specQ)),by="ID"],
             by="ID")
# convert to location-scale parameterization:
rDT[,sigma:=eta*exp(beta)]
rDT[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

# one of the parameter combinations for gamfelt data is outside the support:
which(rDT[,(1 + xi*(max.y-mu)/sigma)]<=0)

which(rDT[,(1 + xi*(min.y-mu)/sigma)]<=0)

## calculate the lower (or upper) bound imposed by the parameter combos:
rDT[,bound:=mu-sigma/xi]

## there are no estimated xi parameters < 0:
rDT[xi<0]






# Use the fitted floodGAM models to predict at zfelt locations -----------------

## make the predictions for the zfelt
zfelt.eta <- predict.gam(eta, newdata = zfcov, type="response")
zfelt.beta <- predict.gam(beta, newdata = zfcov, type="response")
zfelt.xi <- predict.gam(xi, newdata = zfcov, type="response")

## add zfelt predictions to a data.table:
zDT <- zfcov[,"ID"]
zDT[,c("eta","beta","xi"):=list(zfelt.eta,zfelt.beta,zfelt.xi)]
# convert to location-scale parameterization:
zDT[,sigma:=eta*exp(beta)]
zDT[,mu:=eta - sigma*(log(2)^(-xi)-1)/xi]

## calculate the lower (or upper) bound imposed by the parameter combos:
zDT[,bound:=mu-sigma/xi]

## in most cases the bound is negative (interpretation: zero, since it's streamflow)
## but in 9 cases the bound is positive:
zDT[bound>0]


## ---- make the histograms

par(mfrow=c(1,3))

# ETA
p1 <- hist(g.eta, nclass=20, plot=F)
p2 <- hist(zfelt.eta, nclass=30, plot=F)
plot(0,0,type="n",xlim=c(0,5000),ylim=c(0,1*10^-2.6),xlab="",ylab="")
title("eta (median flood)",line=0.5)
plot(p1,col="darkgrey",add=TRUE, freq=F)
plot(p2,col="brown",density=25,angle=135,add=TRUE, freq=F)
legend("topright", 
       legend = c("gamfelt", "zfelt"), 
       col = c("darkgrey","brown"), 
       pch = c(15,15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.02))


# BETA
p1 <- hist(g.beta, nclass=20, plot=F)
p2 <- hist(zfelt.beta, nclass=30, plot=F)
plot(0,0,type="n",xlim=c(-1.75,-0.75),ylim=c(0,4),xlab="",ylab="")
title("beta",line=0.5)
plot(p1,col="darkgrey",add=TRUE, freq=F)
plot(p2,col="brown",density=25,angle=135,add=TRUE, freq=F)



# XI
p1 <- hist(g.xi, nclass=20, plot=F)
p2 <- hist(zfelt.xi, nclass=30, plot=F)
plot(0,0,type="n",xlim=c(-0.05,0.12),ylim=c(0,35),xlab="",ylab="")
title("xi",line=0.5)
plot(p1,col="darkgrey",add=TRUE, freq=F)
plot(p2,col="brown",density=25,angle=135,add=TRUE, freq=F)



# Scatterplots for predicted values ---------------------------------------

par(mfrow=c(1,2))

plot(zfelt.eta,zfelt.xi)
points(g.eta,g.xi,col="magenta",pch=18)

plot(zfelt.eta,zfelt.beta)
points(g.eta,g.beta,col="magenta",pch=18)





neg.xi <- which(zfelt.xi<0)


zfcov[neg.xi]

rawzfcov <- fread("~/floodGAM/data/raw-data/zfelt_catchment_covariates.csv")

rawzfcov[OBJECTID%in%neg.xi]















