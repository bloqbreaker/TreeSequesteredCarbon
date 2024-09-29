# script information ####
# title: Approximation of carbon storage in trees
# fieldTrip: Urban Ecology - University Potsdam
# author: Max Jacobs - MatrikelNo.: 821113
# packages: tidyverse

# import data ####

## import approximation tables and metadata #####
# create path variables
metadataPath <- 'data/metadata'
dataPath <- 'data/co2ApproxTables'

# list and save names of .csv-files in respective data path
# list metadata files
metadataFiles <- list.files(path = metadataPath, pattern = '.csv')
# for the data, only list the array for urban carbon approximation
dataFiles <- list.files(path = dataPath, pattern = '_urban.csv')

# create empty list to store arrays in
approxList <- list()

# read in the metadata and data arrays and store in a table
for (i in metadataFiles) {
  approxList[[gsub('.csv', '', i) ]] <- read.csv(
    paste(metadataPath, i, sep = '/'),
    header = FALSE)
}

# spread approxList$Artenzuordnung rows into seperate variables of char vectors
# for grep() and logic access
for (i in 1:nrow(approxList$Artenzuordnung) ) {
  approxList$ArtenzuordnungList[[approxList$Artenzuordnung[i,1] ]] <- strsplit(
    approxList$Artenzuordnung[i,2], ', ')
}

# replace Artenzuordnung with ArtenzuordnungList and remove the latter
approxList$Artenzuordnung <- approxList$ArtenzuordnungList
approxList$ArtenzuordnungList <- NULL

# spread Gewichtsgruppen into lower and upper bound
approxList$Gewichtsgruppen <- approxList$Gewichtsgruppen %>% mutate(
  lower = gsub('-.*', '', V2) %>% as.integer(),
  upper = gsub('.*-', '', V2) %>% as.integer()
)

# concatenate approximation tables to approxList
for (i in dataFiles) {
  approxList[[gsub('.csv|_.*.csv', '', i) ]] <- read.csv(
    paste(dataPath, i, sep = '/'),
    header = FALSE)
}

# set first row to names for Parameterzuordnung
colnames(approxList$Parameterzuordnung) <- approxList$Parameterzuordnung[1,]
approxList$Parameterzuordnung <- approxList$Parameterzuordnung[-1,]

# set type of parameters to numeric
approxList$Parameterzuordnung <- approxList$Parameterzuordnung %>% mutate(
  a = as.numeric(a),
  b = as.numeric(b),
  c = as.numeric(c),
  RSfac  = as.numeric(RSfac)
)

rm(dataPath, metadataPath, dataFiles, metadataFiles, i)

## import QGIS data ####
# i exported the QGIS layer as .csv to circumvent having to load in and
# clean the .gpkg file in R

rawData <- read.csv(
  'data/data/baumkatasterDaten.csv',
  header = TRUE
)

# filter rows for which critical data is available
data <- rawData %>% 
  select(-c('NAME_LAT', 'KRONE_D') ) %>%
  filter(!(is.na(BHD) | is.na(HOEHE) ) )

# carbon storage approximation ####
## classify tree types ####
# rough classification for all trees by their taxonomic group, column: 'ART'
# 'Laubbaum' = deciduous
# 'Nadelbaum' = coniferous
data <- data %>% mutate(
  class = ART %>% 
    gsub('Laubbaum', 'Eiche', .) %>% 
    gsub('Nadelbaum', 'Kiefer', .)
) 

# more detailed classification by the genus (Gattung), column: 'DT_NAME'
for (i in 1:length(approxList$Artenzuordnung) ) {
  data$class[grep(stringi::stri_join_list(approxList$Artenzuordnung[[i]], sep = '|'),
                  data$NAME_DT, ignore.case = TRUE) ] <- 
    names(approxList$Artenzuordnung[i] )
}

# recalculate BHD for trees/shrubs with multiple tree trunks
# set default for trunk number to 1
data$n_staemme <- 1
# overwrite n_trunks for trees with multiple ones 
for (i in grep('^\\d.*stämme', data$KOMMENTAR, ignore.case = TRUE)) {
  data$n_staemme[i] <- as.numeric(str_extract(data$KOMMENTAR[i], '^\\d*') )
}

data <- data %>% mutate(
  BHD = as.numeric(BHD) * sqrt(n_staemme) * 100 # BHD unit now in [cm]
) %>%
  select(-'KOMMENTAR')
