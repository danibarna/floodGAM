


library(ggplot2)
library(scico)







# step 3: visualize ML-selected covariates --------------------------------

iisDE <- readRDS(paste0(dataPath,"featuresFromIIS.rds"))

# Prepare variable names
iisDE[, navn := sub("_", "[", Feature)]
iisDE[, navn := gsub("_", "", navn)]
iisDE[, navn := ifelse(grepl("[", navn, fixed = TRUE), paste0(navn, "]"), navn)]
iisDE[, navn := ifelse(Feature == "M_200_h", "M[h200]", navn)]
iisDE[, navn := ifelse(Feature == "M_200_d", "M[d200]", navn)]

# Count the number of folds and total counts
iisDE[, numFolds := .N, by = .(navn, d, model)]
iisDE[, dum.y := 1]
iisDE[, fc := sum(dum.y), by = .(navn, model)]
tc <- iisDE[, sum(fc), by = .(navn, model)]
faclev <- tc[rev(order(V1))][, navn]

# Fix variable levels
faclev[1] <- "Q[N]"
faclev[2] <- "A[LE]"
iisDE[, navn := factor(navn, levels = faclev)]

# Filter data
gdat <- iisDE[fc >= 15]

# Define color palette
ctab <- scico(17, palette = "grayC", end = 0.9)[c(1, 4, 7, 10, 13, 14:17)]

# Plot
ggplot(gdat, aes(fill = as.factor(ordFeat), y = as.factor(d), x = dum.y)) + 
  geom_col(position = "stack", color = "white") +
  labs(x = "Number of times predictor included", y = "Duration (hours)") +
  theme_bw() +
  scale_fill_scico_d(palette = "grayC", end = 0.9,
                     name = "Order of predictor selection") +
  scale_x_continuous(breaks = c(0, 5, 10), limits = c(0, 10)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", 
                             title.theme = element_text(angle = 0, family = "serif", size = 16))) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white"),
        text = element_text(size = 16, family = "serif"),
        legend.title = element_text(size = 20),
        legend.direction = 'horizontal',
        legend.position = 'top',
        axis.title = element_text(size = 20, family = "serif"),
        strip.text.y.right = element_text(angle = 0)) +
  facet_wrap(~navn, labeller = label_parsed, ncol = 1, strip.position = "right")


## 8 in x 15 in
gdat <- iisDE

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


