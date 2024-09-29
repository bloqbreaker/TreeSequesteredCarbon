# script information ####
# title: Approximation of carbon storage in trees
# fieldTrip: Urban Ecology - University Potsdam
# author: Max Jacobs - MatrikelNo.: 821113
# packages: tidyverse

# allocate parameters from Parameterzuordung
data <- data %>% left_join(approxList$Parameterzuordnung, by = 'class')

# calculate stored carbon
data <- data %>% mutate(
  biomass = a * BHD ^ b * HOEHE ^ c, # calculate above ground biomass
  biomass = (1 + RSfac) * biomass, # add sub ground biomass
  carbonmass = 0.5 * biomass, # convert biomass to carbon mass
  co2mass = 3.67 * carbonmass # convert carbon mass to co2 mass
)

# ??? write actual weight into separate col to export and visualize in QGIS??? ####
write.csv(data, paste(getwd(), 'results/tables/eqn_data_gwGruppe.csv', sep = '/'), row.names = FALSE)

## visualization of the results ####
### bar chart to represent total amount of CO2 stored by most abundant tree species ####
eqn_bar_totWgt_abundSpecies <- data %>% 
  group_by(NAME_DT) %>%
  summarise(n = n(),
  co2Total = sum(co2mass)
  ) %>%
  # arrange df in descending order of counts by tree group, keep top 10 most 
  # abundant species and summarise others ('Sonstige')
  arrange(desc(n)) %>%
  mutate(NAME_DT = if_else(row_number() <= 10, NAME_DT, 'Sonstige')
  ) %>%
  summarise(n = sum(n),
            co2Total = sum(co2Total),
            .by = NAME_DT) %>% 
  mutate(NAME_DT = if_else(row_number() == grep(' ', NAME_DT),
                          paste(gsub(' ', '\n', NAME_DT), '\nn = ', n, sep = ''),
                          paste(NAME_DT, '\nn = ', n, sep = '')),
         NAME_DT = factor(NAME_DT, levels = NAME_DT)
         ) 

write.csv(eqn_bar_totWgt_abundSpecies,
          paste(getwd(), 'results/tables/eqn_bar_totWgt_abundSpecies.csv', sep = '/'),
          row.names = FALSE)

eqn_bar_totWgt_abundSpecies <- eqn_bar_totWgt_abundSpecies %>%
  ggplot(
    aes(x = NAME_DT)
    ) + 
  geom_bar(
    aes(y = co2Total),
    stat = 'identity',
    #alpha = 0.6,
    color = 'deepskyblue3',
    fill = 'deepskyblue3',
    width = 0.35,
    just = 0.5
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gesamt gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 75000),
    breaks = seq(0, 70000, 10000),
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
eqn_bar_totWgt_massSpecies <- data %>% 
  group_by(NAME_DT) %>%
  summarize(n = n(),
            co2Total = sum(co2mass)
  ) %>%
  # arrange df in descending order of counts by tree group, keep top 10 most 
  # abundant species and summarise others ('Sonstige')
  arrange(desc(co2Total)) %>%
  mutate(NAME_DT = if_else(row_number() <= 10, NAME_DT, 'Sonstige')
  ) %>%
  summarise(n = sum(n),
            co2Total = sum(co2Total),
            .by = NAME_DT) %>% 
  mutate(NAME_DT = if_else(row_number() %in% grep(' ', NAME_DT),
                           paste(gsub(' ', '\n', NAME_DT), '\nn = ', n, sep = ''),
                           paste(NAME_DT, '\nn = ', n, sep = '')),
         NAME_DT = factor(NAME_DT, levels = NAME_DT)
  ) 

write.csv(eqn_bar_totWgt_massSpecies,
          paste(getwd(), 'results/tables/eqn_bar_totWgt_massSpecies.csv', sep = '/'),
          row.names = FALSE)

eqn_bar_totWgt_massSpecies <- eqn_bar_totWgt_massSpecies %>%
  ggplot(
    aes(x = NAME_DT)
  ) + 
  geom_bar(
    aes(y = co2Total),
    stat = 'identity',
    #alpha = 0.6,
    color = 'deepskyblue3',
    fill = 'deepskyblue3',
    width = 0.35,
    just = 0.5
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

### not used for the essay ####
#### box plot of weight by species ####
data %>% ggplot() +
  geom_boxplot(
    aes(x = NAME_DT, y = co2mass)
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 8500),
    breaks = seq(0, 8500, 500),
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

# make 2nd one zoomed in
data %>% ggplot() +
  geom_boxplot(
    aes(x = NAME_DT, y = co2mass)
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 3500),
    breaks = seq(0, 3500, 500),
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

# and a 3rd one
data %>% ggplot() +
  geom_boxplot(
    aes(x = NAME_DT, y = co2mass)
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1000),
    breaks = seq(0, 1000, 100),
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

# and a 4th one
data %>% ggplot() +
  geom_boxplot(
    aes(x = NAME_DT, y = co2mass)
  ) + 
  theme_bw() +
  labs(x = 'Baumart', y = 'Gespeichertes CO2 [kg]') + 
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 500),
    breaks = seq(0, 500, 50),
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
