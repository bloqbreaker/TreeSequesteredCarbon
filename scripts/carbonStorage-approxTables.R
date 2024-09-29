# script information ####
# title: Approximation of carbon storage in trees
# fieldTrip: Urban Ecology - University Potsdam
# author: Max Jacobs - MatrikelNo.: 821113
# packages: tidyverse

# allocate weight groups from approximation tables
data$gwGruppe <- NA

# create function to retrieve row and column number from approximation tables
# this returns the row/col-number for the maximum value or replaces, it in case 
# BHD or HOEHE is greater than the approx tables highest value, with the
# respective max value
my_fun <- function(ref, dat) {
  a <- ref >= round(dat)
  if(any(a)) {
    return(which.max(a))
  } else {
    return(nrow(ref))
  }
}

for (i in 1:nrow(data)) {
  data$gwGruppe[i] = approxList[[data[i,]$class]] [
    my_fun(approxList$HOEHE_ref, data$HOEHE[i] ),
    my_fun(approxList$BHD_ref, data$BHD[i] ) ]
  # print values for debugging where necessary
  if(is.na(data$gwGruppe[i])) {
    cat('i = ', i, '\n')
    cat('Hoehe = ', round(data$HOEHE[i] ), '\n',
        'which(HOEHE) = ',
        my_fun(approxList$HOEHE_ref, data$HOEHE[i] ), '\n',
        'BHD = ', round(100 * data$BHD[i] ), '\n',
        'which(BHD) = ',
        my_fun(approxList$BHD_ref, data$BHD[i] ), '\n',
        'class = ', data[i,]$class, '\n',
        'wgtclass =', approxList[[data[i,]$class]] [
          my_fun(approxList$HOEHE_ref, data$HOEHE[i] ),
          my_fun(approxList$BHD_ref, data$BHD[i] ) ], '\n\n')
  }
}

cat('was every element for which data was available computed?\n',
      !any(is.na(data$gwGruppe)), '\n')

# ??? write actual weight into separate col to export and visualize in QGIS??? ####
write.csv(data,
          paste(getwd(), '/results/tables/approxTab_data_gwGruppe.csv', sep = ''),
          row.names = FALSE)

## visualization of the results ####
### distribution plot (prevalence?/abundance?) ####
hist <- data %>% 
  ggplot(
    aes(gwGruppe)
  ) +
  geom_histogram(
    binwidth = .5,
    colour = 'darkslateblue',
    fill = 'darkslateblue',
    alpha = 0.8
  ) + 
  theme_bw() +
  labs(x = 'Gewichtsgruppe [kg]', y = 'Anzahl') + 
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0.5, 11.5),
    breaks = seq(1, 11, 1),
    minor_breaks = NULL,
    labels = approxList$Gewichtsgruppen$V2
  ) + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0,80),
    breaks = seq(0, 80, 10),
    minor_breaks = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,
                               hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

### bar chart to represent total amount of CO2 stored by weight group ####

bar_totwgt_wgtGroup <- data %>% 
  group_by(gwGruppe) %>%
  summarize(n = n()) %>%
  mutate(
    lowerTotal = n * approxList$Gewichtsgruppen$lower,
    upperTotal = n * approxList$Gewichtsgruppen$upper
  ) 

write.csv(bar_totwgt_wgtGroup,
          paste(getwd(), 'results/tables/approxTab_bar_totwgt_wgtGroup.csv', sep = '/'),
          row.names = FALSE)

bar_totwgt_wgtGroup <- bar_totwgt_wgtGroup %>%
ggplot(
  aes(x = gwGruppe)
  ) + 
  geom_bar(
    aes(y = lowerTotal),
    stat = "identity",
    #alpha = 0.6,
    color = 'deepskyblue3',
    fill = 'deepskyblue3',
    width = 0.25,
    just = 0
  ) + 
  geom_bar(
    aes(y = upperTotal),
    stat = "identity",
    #alpha = 0.6,
    color = 'darkslateblue',
    fill = 'darkslateblue',
    width = 0.25,
    just = 1
  ) + 
  theme_bw() +
  labs(x = 'Gewichtsgruppe [kg]', y = 'Gesamt gespeichertes CO2 [kg]') + 
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0.5, 11.5),
    breaks = seq(1, 11, 1),
    minor_breaks = NULL,
    labels = approxList$Gewichtsgruppen$V2
  ) + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 75000),
    breaks = seq(0, 75000, 10000),
    minor_breaks = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,
                               hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

### bar chart to represent total amount of CO2 stored by most abundant tree species ####
bar_totWgt_abundSpecies <- data %>% 
  group_by(NAME_DT) %>%
  summarize(n = n(),
  lowerTotal = sum(approxList$Gewichtsgruppen$lower[gwGruppe]),
  upperTotal = sum(approxList$Gewichtsgruppen$upper[gwGruppe])
  ) %>%
  # arrange df in descending order of counts by tree group, keep top 10 most 
  # abundant species and summarise others ('Sonstige')
  arrange(desc(n)) %>%
  mutate(NAME_DT = if_else(row_number() <= 10, NAME_DT, 'Sonstige')
  ) %>%
  summarise(n = sum(n),
            lowerTotal = sum(lowerTotal),
            upperTotal = sum(upperTotal),
            .by = NAME_DT) %>% 
  mutate(NAME_DT = if_else(row_number() == grep(' ', NAME_DT),
                          paste(gsub(' ', '\n', NAME_DT), '\nn = ', n, sep = ''),
                          paste(NAME_DT, '\nn = ', n, sep = '')),
         NAME_DT = factor(NAME_DT, levels = NAME_DT)
         ) 

write.csv(bar_totWgt_abundSpecies,
          paste(getwd(), 'results/tables/approxTab_bar_totWgt_abundSpecies.csv', sep = '/'),
          row.names = FALSE)

bar_totWgt_abundSpecies <- bar_totWgt_abundSpecies %>%
  ggplot(
    aes(x = NAME_DT)
    ) + 
  geom_bar(
    aes(y = lowerTotal),
    stat = 'identity',
    #alpha = 0.6,
    color = 'deepskyblue3',
    fill = 'deepskyblue3',
    width = 0.25,
    just = 0
  ) + 
  geom_bar(
    aes(y = upperTotal),
    stat = 'identity',
    #alpha = 0.6,
    color = 'darkslateblue',
    fill = 'darkslateblue',
    width = 0.25,
    just = 1
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gesamt gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 75000),
    breaks = seq(0, 75000, 10000),
    minor_breaks = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,
                               hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

### bar chart to represent total amount of CO2 stored by tree species of highest total weight ####
bar_totWgt_massSpecies <- data %>% 
  group_by(NAME_DT) %>%
  summarize(n = n(),
            lowerTotal = sum(approxList$Gewichtsgruppen$lower[gwGruppe]),
            upperTotal = sum(approxList$Gewichtsgruppen$upper[gwGruppe])
  ) %>%
  # arrange df in descending order of counts by tree group, keep top 10 most 
  # abundant species and summarise others ('Sonstige')
  arrange(desc(upperTotal)) %>%
  mutate(NAME_DT = if_else(row_number() <= 10, NAME_DT, 'Sonstige')
  ) %>%
  summarise(n = sum(n),
            lowerTotal = sum(lowerTotal),
            upperTotal = sum(upperTotal),
            .by = NAME_DT) %>% 
  mutate(NAME_DT = if_else(row_number() %in% grep(' ', NAME_DT),
                           paste(gsub(' ', '\n', NAME_DT), '\nn = ', n, sep = ''),
                           paste(NAME_DT, '\nn = ', n, sep = '')),
         NAME_DT = factor(NAME_DT, levels = NAME_DT)
  ) 

write.csv(bar_totWgt_massSpecies,
          paste(getwd(), 'results/tables/approxTab_bar_totWgt_massSpecies.csv', sep = '/'),
          row.names = FALSE)

bar_totWgt_massSpecies <- bar_totWgt_massSpecies %>%
  ggplot(
    aes(x = NAME_DT)
  ) + 
  geom_bar(
    aes(y = lowerTotal),
    stat = 'identity',
    #alpha = 0.6,
    color = 'deepskyblue3',
    fill = 'deepskyblue3',
    width = 0.25,
    just = 0
  ) + 
  geom_bar(
    aes(y = upperTotal),
    stat = 'identity',
    #alpha = 0.6,
    color = 'darkslateblue',
    fill = 'darkslateblue',
    width = 0.25,
    just = 1
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gesamt gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 75000),
    breaks = seq(0, 75000, 10000),
    minor_breaks = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5,
                               hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )
