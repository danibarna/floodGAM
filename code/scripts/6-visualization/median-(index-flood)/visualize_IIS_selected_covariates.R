

library(data.table)
library(ggh4x)
library(gtable)
library(grid)
library(scico)



iisDE <- readRDS(paste0("~/floodGAM/results/output/median-(index-flood)/",
                             "gamfelt_hydagsupp_featuresFromIIS_gam.rds"))


iisDE <- iisDE[edf>0.001]

# step 3: visualize ML-selected covariates --------------------------------

# Prepare variable names
iisDE[, navn := sub("_", "[", Feature)]
iisDE[, navn := gsub("_", "", navn)]
iisDE[, navn := ifelse(grepl("[", navn, fixed = TRUE), paste0(navn, "]"), navn)]
iisDE[, navn := ifelse(Feature == "M_200_h", "M[h200]", navn)]
iisDE[, navn := ifelse(Feature == "M_200_d", "M[d200]", navn)]
iisDE[, navn := ifelse(Feature == "M_200_d_SN", "M[dsn200]", navn)]

# Count the number of folds and total counts
iisDE[, numFolds := .N, by = .(navn, d)]
iisDE[, dum.y := 1]
iisDE[, fc := sum(dum.y), by = .(navn)]
tc <- iisDE[, sum(fc), by = .(navn)]
faclev <- unique(tc[rev(order(V1))][, navn])

# Fix variable levels
faclev[1] <- "Q[N]"
faclev[3] <- "A[P]"
iisDE[, navn := factor(navn, levels = faclev)]


## 8 in x 15 in
gdat <- iisDE[fc>10]

ctab <- scico(max(gdat$ordFeat), palette = "grayC", end = 0.9)

ggplot(gdat, aes(fill = as.factor(ordFeat), x = as.factor(d), y = dum.y)) + 
  geom_col(position = "stack", color = "white") +
  labs(y = "Number of times predictor included", x = "Duration (hours)") +
  theme_bw() +
  scale_fill_scico_d(palette = "grayC", end = 0.9,
                     name = "Order of predictor selection") +
  scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", 
                             title.theme = element_text(angle = 0, 
                                                        family = "serif", 
                                                        size = 16))) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "serif"),
        legend.title = element_text(size = 25),
        legend.direction = 'horizontal',
        legend.position = 'top',
        axis.title = element_text(size = 20, family = "serif"),
        axis.title.y = element_text(hjust = 0.3),
        axis.title.x = element_text(margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(angle = 0,size=20)) +
  facet_wrap(~navn, labeller = label_parsed, nrow = 1, strip.position = "top")

#16x8.5

#17.5x5.5



# Check the full grid with fold + feature order ---------------------------


ctab <- scico(max(iisDE$ordFeat), palette = "grayC", end = 0.9)

gdat <- iisDE

gdat[,fold1:=fold]

ggplot(gdat) + 
  geom_rect(aes(fill = as.factor(ordFeat)),alpha=0.7,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +
  #labs(y = "Number of times predictor included", x = "Duration (hours)") +
  theme_bw() +
  scale_fill_scico_d(palette = "grayC", end = 0.9,
                     name = "Order of predictor selection") +
  #scale_y_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", 
                             title.theme = element_text(angle = 0, 
                                                        family = "serif", 
                                                        size = 16))) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "serif"),
        legend.title = element_text(size = 25),
        legend.direction = 'horizontal',
        legend.position = 'top',
        axis.title = element_text(size = 20, family = "serif"),
        axis.title.y = element_text(hjust = 0.3),
        axis.title.x = element_text(margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(angle = 0,size=20)) +
  facet_nested(d + fold ~ navn , scales = "free",
               space = "free_y",
               labeller = label_wrap_gen(40),
               strip = strip_nested(size = "variable"),
               nest_line = TRUE)


