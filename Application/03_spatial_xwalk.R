#######################################################################################
### This file is designed to
### 1) create a crosswalk that allows an allocation across different spatial levels ###
#######################################################################################

### Load required packages
library(data.table)
library(geosphere)

### Load required functions
source('00_directories.R')
source('helpers/functions_census.R')

### Read data files
setwd(midsave)
crca_info 	<- read.csv('crca_info.csv'); setnames(crca_info,'OBJECTID_1','com') # Generated in 'ind_allocation_and_dep_vars.R'
tow2com     <- fread('com_allocation_exact.csv') # Generated in 'tow_to_com.R'
setwd(main)

### Combine the tower-to-commune allocation and the revised region code
xwalk <- as.data.table(merge(crca_info, tow2com, by = 'com', all.y = T))
setnames(xwalk, 'site_id', 'tower')

### Create dakar specific centroid and calculate distance on tower level
centroid <- xwalk[xwalk$DEPT == "DAKAR",]
centroid <- data.table(lon = mean(centroid$lon), lat = mean(centroid$lat))

### Calculate the distance of the towers to the centroid of Dakar to get average distance of calls and sms
xwalk$dist2d <- distHaversine(xwalk[,list(lon,lat)],centroid)/1000
rm(centroid)

### Align format of crosswalk
# Change variable type
xwalk$REG <- as.character(xwalk$REG)
xwalk$DEPT <- as.character(xwalk$DEPT)
xwalk$ARR <- as.character(xwalk$ARR)
# Correct region names
xwalk$REG <- gsub('-', '_', xwalk$REG)
# Correct department names
xwalk$DEPT <- gsub('-', '_', xwalk$DEPT)
xwalk$DEPT <- gsub('?''', '', xwalk$DEPT) # The ' has to be adjusted every time! unique(xwalk$DEPT)
# Correct arrondissement names
xwalk$ARR <- gsub('-', '_', xwalk$ARR)
xwalk$ARR[xwalk$ARR == "DAROU MINAME II"] <- "DAROU MINAM II"
xwalk$ARR[xwalk$ARR == "MEUR MOMAR SARR"] <- "KEUR MOMAR SARR"
xwalk$ARR[xwalk$ARR == "NDIAYE BERESS"] <- "NDIAYE MBERESS"
xwalk$ARR[xwalk$ARR == "SARA BIDJI"] <- "SARE BIDJI"
# Change spaces for underlines
xwalk$REG <- gsub(' ', '_', xwalk$REG)
xwalk$DEPT <- gsub(' ', '_', xwalk$DEPT)
xwalk$ARR <- gsub(' ', '_', xwalk$ARR)

# Write file
setwd(midsave)
write.csv(xwalk, 'spatial_xwalk_exact.csv', row.names = F)
setwd(main)

############################################################################################################
