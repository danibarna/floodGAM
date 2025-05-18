##
##
##
##
##
## for all stations that are triangular in Figure 6,, compute
## the ratio of tehir 1 and 24 hour floods
## -----------------------------------------------------------------------------

library(data.table)


# Load in observed data ---------------------------------------------------
## load in the data to get the median 1 hour and median 24 hour floods:
gfcov <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt/",
                        "gamfelt_catchment_covariates.rds"))
# ^ gfcov loaded only to convert to specific discharge

gfam <- readRDS(paste0("~/floodGAM/data/processed-data/gamfelt-durations/",
                       "gamfelt_hydagsupplement_durations_annual_maxima.rds"))

# convert to specific discharge
gfam <- merge(gfam,gfcov[,c("ID","A")],by="ID")
gfam[,specQ:=Qm3_s/A*1000]

# gfam = gamfelt annual maxima



# Load in the stations that have inconsistent oos predictions: ------------

## load in these to get the staiton IDs:

load("~/floodGAM/results/output/all-quantiles/objects-duration-incon.rda")

load("~/floodGAM/results/output/all-quantiles/plotobj-duration-incon.rda")
# what's the difference between these two?



# A little data processing ------------------------------------------------

## compute the median 1 hour and median 24 hour flood per station:

obsmed <- merge(gfam[d==1,median(specQ),by="ID"],
                gfam[d==24,median(specQ),by="ID"],by="ID")

setnames(obsmed,c("V1.x","V1.y"),c("one","twentyfour"))


## find the IDs of the inconsistent stations
Qdc <- alltt[type=="QRT",.N,by="ID"]
Qdc[,isQ:="only QRT"]

Pdc <- alltt[type=="PRT",.N,by="ID"]
Pdc[,isP:="pincon"]
dc <- merge(Qdc,Pdc,by="ID",all=T)

dc[,isB:=ifelse( !is.na(isP), "both PRT & QRT", "only QRT" )]


## add which are inconsistent to obsmed:
obsmed[,isi:=ifelse(ID%in%dc$ID,"duration inconsistent","duration consistent")]


## compute the culmination factor:
obsmed[,cf:=one/twentyfour]


# Make the plot -----------------------------------------------------------

ggplot(obsmed) + 
  geom_histogram(aes(cf,fill=isi),
                 color="black", binwidth = .04) +
  scale_fill_manual(values = c("white", "grey"), name = " ") +
  labs(x = "Culmination factor", y = "Number of stations") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = c(0.75,0.85),
        legend.background = element_blank())



ggsave(paste0("~/floodGAM/",
              "results/figures/","all-quantiles/",
              "culmination_factor_histogram.pdf"),
       width=6,height=4,units="in")






