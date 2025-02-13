##
##
##
##
##
## Response variables for the GAM models are GEV parameters obtained from 
## at-site frequency analysis.
##
## This script fits a GEV distribution at each gamfelt station.
##
## -----------------------------------------------------------------------------

library(data.table)
library(rstan)

# set stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# set wd to where stan model lives
setwd("~/floodGAM/code/functions/Stan")

## ----- load in the gamfelt dataset
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
# ^ gfcov loaded only to convert to specific discharge

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))

# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# gfam = gamfelt annual maxima



# Loop over the stations and durations -----------------------------------------

stationvec <- gfam[,unique(ID)]

result <- list(); i = 1

for(station in stationvec){
  
  for(di in unique(gfam[,get("d")])){
    
    # data the Stan model takes is a list where
    # N = years of data
    # y = observed data points
    sdata <- list(N = gfam[ID==station&d==di][,.N],
                  y = gfam[ID==station&d==di,specQ])
    
    if(median(sdata[["y"]]) > 1000){ #different priors on qind for large v small catchments
      fit <- stan(
        file = 'ifgev_prior2.stan',
        data = sdata,
        chains = 3,
        warmup = 5000,
        iter = 25000,
        cores = 1,
        control = list(adapt_delta = 0.9999),
        seed = 42)
    }else{
      fit <- stan(
        file = 'ifgev_prior1.stan',
        data = sdata,
        chains = 3,
        warmup = 5000,
        iter = 25000,
        cores = 1,
        control = list(adapt_delta = 0.9999),
        seed = 42)
    }
    
    ## very often, issues with the fit are minor computational issues 
    ## due to the complicated geometry of the posterior (Lambert-W fn)  
    ## and can be fixed by re-running with a different seed:
    
    # check for any divergences
    # if n > 0, the model has not converged and more investigation is needed
    # (may need extra tuning, such as altering adapt_delta)
    sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
    divergent <- do.call(rbind, sampler_params)[,'divergent__']
    n = sum(divergent)
    
    # if any divergences, try up to six more times with different seeds...
    j = 1; seedvec <- c(43,44,45,85,86,87)
    while(n > 0 & j < 7){
      print("got a divergence")
      if(median(sdata[["y"]]) > 1000){
        fit <- stan(file = 'ifgev_prior2.stan',data = sdata,
                    chains = 3,warmup = 5000,iter = 25000,cores = 1,
                    control = list(adapt_delta = 0.9999),
                    seed = seedvec[j])
      }else{
        fit <- stan(file = 'ifgev_prior1.stan',data = sdata,
                    chains = 3,warmup = 5000,iter = 25000,cores = 1,
                    control = list(adapt_delta = 0.9999),
                    seed = seedvec[j])
      }
      sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
      divergent <- do.call(rbind, sampler_params)[,'divergent__']
      n = sum(divergent)
      j = j + 1
    }
    
    
    # check Rhat convergence diagnostic values.
    # Rhat compares the between- and within-chain estimates for model parameters.
    # If any Rhat < 1.005, chains have not mixed, so try up to six more times
    # with different seeds
    s <- as.data.table(summary(fit)$summary)
    rh = any(s$Rhat > 1.005)
    j = 1
    while(rh == T & j < 7){
      print("Rhat > 1.005")
      if(median(sdata[["y"]]) > 1000){
        fit <- stan(file = 'ifgev_prior2.stan',data = sdata,
                    chains = 3,warmup = 5000,iter = 25000,cores = 1,
                    control = list(adapt_delta = 0.9999),
                    seed = seedvec[j])
      }else{
        fit <- stan(file = 'ifgev_prior1.stan',data = sdata,
                    chains = 3,warmup = 5000,iter = 25000,cores = 1,
                    control = list(adapt_delta = 0.9999),
                    seed = seedvec[j])
      }
      s <- as.data.table(summary(fit)$summary)
      rh = any(s$Rhat > 1.005)
      j = j + 1
    }
    
    # this just saves the summary of the stan model.
    # if the actual model is needed (f. ek. we want to look at traceplots 
    # or plot the posteriors) we can save that too, i.e. result[[i]] <- fit
    if(n > 0){
      result[[i]] <- n
      i = i+1
    } else{
      s <- as.data.table(summary(fit)$summary)
      s[,param:=c("qind","beta","xi","sigma","mu","lp__")]
      s[,ID:=station]
      result[[i]] <- s
      i = i+1
    }
  }
}

namesvec <- gfam[,.N,by=c("ID","d")][,paste0(ID,"_",d)]

names(result) <- namesvec

saveRDS(result,file="~/floodGAM/results/output/gamfeltstanresult_intermediate.rds")

result <- rbind(rbindlist(result)) # if all converged, this will work

saveRDS(result,file="~/floodGAM/results/output/gamfeltstanresult.rds")

