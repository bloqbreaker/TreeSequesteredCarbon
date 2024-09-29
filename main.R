# script information ####
# title: Approximation of carbon storage in trees
# fieldTrip: Urban Ecology - University Potsdam
# author: Max Jacobs - MatrikelNo.: 821113
# packages: tidyverse, gridExtra, cowplot, ggpubr

# Skript initiation ####

# load dependencies #####

# unload all non-base packages
# source: https://stackoverflow.com/a/57981202 , answer by user:7322615, 17-Sep-'19 at 19:53, accessed 10-Dec-'21
# for (i in 1:2) {
#   base::lapply(names(sessionInfo()$otherPkgs), function(pkgs) {
#     detach(
#       paste0("package:", pkgs),
#       character.only = T,
#       unload = T,
#       force = F
#     )
#   })
# }

# check if required, passive packages are installed, install if needed
# adapted from https://stackoverflow.com/a/4090208 , answer by user:163053, 03-Nov-'10 at 18:13, accessed 09-Dec-'21
requiredPackages <- c('tidyverse', 'cowplot', 'gridExtra', 'ggpubr')
newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
if (length(newPackages) > 0) {
  cat("installing package:", packages[i])
  install.packages(newPackages)
}

rm(requiredPackages, newPackages, i)

# import packages
library(tidyverse)

# run scripts for co2 approximation by tables and co2 calculation by eqn (equation)

source('scripts/readDataMetadata.R')
source('scripts/carbonStorage-approxTables.R')
source('scripts/carbonStorage-eqn.R')

# save all plots individually
png('results/plots/hist_wgtGroup.png')
hist
dev.off()
png('results/plots/bar_totWgt_wgtGroup.png')
bar_totwgt_wgtGroup
dev.off()
png('results/plots/bar_totWgt_abundSpecies.png')
bar_totWgt_abundSpecies
dev.off()
png('results/plots/bar_totWgt_massSpecies.png')
bar_totWgt_massSpecies
dev.off()
png('results/plots/eqn_bar_totWgt_abundSpecies.png')
eqn_bar_totWgt_abundSpecies
dev.off()
png('results/plots/eqn_bar_totWgt_massSpecies.png')
eqn_bar_totWgt_massSpecies
dev.off()


# set new theme with axis title removed
rmAxisTitle <- theme(
  axis.title = element_blank()
)

# create text grobs for axis titles and plot enumeration
xLab <- ggpubr::text_grob('Baumarten',
                          face = 'bold')
yLab <- ggpubr::text_grob('Gesamtes gespeichertes CO2 [kg]',
                          face = 'bold',
                          rot = -270,
                          just = 'top')
# PLOT ENUMERATION
a1 <- ggpubr::text_grob('A.1',
                        face = 'bold')
a2 <- ggpubr::text_grob('A.2',
                        face = 'bold')
b1 <- ggpubr::text_grob('B.1',
                        face = 'bold')
b2 <- ggpubr::text_grob('B.2',
                        face = 'bold')

# create empty grob for spacing   ##### do i need this if i can justify the text in the right direction??????
blankPlot <- ggplot() +
  geom_blank(aes(1, 1)) +
  cowplot::theme_nothing()

# arrange plots with joined axis titles and plot enumeration
combinedPlots <- ggpubr::as_ggplot(
  gridExtra::arrangeGrob(
    bar_totWgt_abundSpecies + rmAxisTitle,     # 1
    eqn_bar_totWgt_abundSpecies + rmAxisTitle, # 2
    bar_totWgt_massSpecies + rmAxisTitle,      # 3
    eqn_bar_totWgt_massSpecies + rmAxisTitle,  # 4
    xLab,                                      # 5
    yLab,                                      # 6
    blankPlot,                                 # 7
    a1, a2, b1, b2,                            # 8 - 11
    layout_matrix = matrix(data = c(6,  8,  1,  9,  2,
                                    6,  7,  1,  7,  2,
                                    6,  10, 3, 11,  4,
                                    6,  7,  3,  7,  4,
                                    7,  5,  5,  5,  5),
                           nrow = 5,
                           ncol = 5
    ) %>% t(),
    widths = c(0.05 , 0.05, 0.425, 0.05, 0.425),
    heights = c(0.03 , 0.455, 0.03, 0.455, 0.03)
  ) 
)

png('results/plots/combinedPlot.png',
    width = 1200,
    height = 800,
    units = "px",
    #pointsize = 1/72,
    res = 72)
combinedPlots
dev.off()

# clean up workspace :)
rm(a1, a2, b1, b2, xLab, yLab, blankPlot, rmAxisTitle, i)
