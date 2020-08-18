###############################################################################
### This file is designed to extract following features from the shape file
### 1) contiguity matrices
### 2) centroids
### 3) NUTS layers 1-4
###############################################################################

### Load required packages
library(dplyr)
library(spdep)
library(rgdal)
library(maptools)

### Load required functions
source('00_directories.R')

# Preprocessing -----------------------------------------------------------
### Load shape file
setwd(input)
com.ogr <- readOGR(dsn = ".", layer = 'LIMITE_CA_CR_2014') # package rgdal		
com     <- spTransform(com.ogr, CRS("+proj=longlat +ellps=GRS80")) # package sp	
rm(com.ogr)

### Align variable type
com$REG <- as.character(com$REG)
com$DEPT <- as.character(com$DEPT)
com$ARR <- as.character(com$ARR)

### Correct region names
com$REG <- gsub('-', '_', com$REG)

### Correct department names
com$DEPT <- gsub('-', '_', com$DEPT)
com$DEPT <- gsub('\u0092', '', com$DEPT) # The ' has to be adjusted every time! unique(com$DEPT)

### Correct arrondissement names
com$ARR[com$ARR == 'CAS-CAS'] <- 'CAS CAS'
com$ARR[com$ARR == "DAROU MINAME II"] <- "DAROU MINAM II"
com$ARR[com$ARR == "MEUR MOMAR SARR"] <- "KEUR MOMAR SARR"
com$ARR[com$ARR == "NDIAYE BERESS"] <- "NDIAYE MBERESS"
com$ARR[com$ARR == "SARA BIDJI"] <- "SARE BIDJI"
com$ARR <- gsub('-', '_', com$ARR)

### Change spaces for underlines
com$REG <- gsub(' ', '_', com$REG)
com$DEPT <- gsub(' ', '_', com$DEPT)
com$ARR <- gsub(' ', '_', com$ARR)

# Creata contiguity matrices and centroids per spatial level -------------------

### Communes
# nb matrix
com_nb <- 
  com %>%
  poly2nb(queen = T, row.names = com$OBJECTID_1, snap = 0.03) %>% 
  nb2mat()
colnames(com_nb) <- row.names(com_nb)
# centroid
com_centroid <- coordinates(com)
row.names(com_centroid) <- as.numeric(row.names(com_centroid)) + 1 
colnames(com_centroid) <- c('lon', 'lat')

### Arrondissement
arr   <- unionSpatialPolygons(com , as.data.frame(com)$ARR) # maptools
# nb matrix  
arr_nb <- 
  arr %>%
  poly2nb(queen = T, snap = 0.03) %>% 
  nb2mat()
colnames(arr_nb) <- row.names(arr_nb)
# Centroid
arr_centroid <- coordinates(arr)
colnames(arr_centroid) <- c('lon', 'lat')
# Spatial polygons data frame
id <- data.frame(id = getSpPPolygonsIDSlots(arr))
row.names(id) <- id$id
shape <- SpatialPolygonsDataFrame(arr, data = id)
arr <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80")); rm(shape, id)

### Department
dept   <- unionSpatialPolygons(com , as.data.frame(com)$DEPT)
# nb matrix  
dept_nb <- 
  dept %>%
  poly2nb(queen = T, snap = 0.03) %>% 
  nb2mat()
colnames(dept_nb) <- row.names(dept_nb)
# Centroid
dept_centroid <- coordinates(dept)
colnames(dept_centroid) <- c('lon', 'lat')
# Spatial polygons data frame
id <- data.frame(id = getSpPPolygonsIDSlots(dept))
row.names(id) <- id$id
shape <- SpatialPolygonsDataFrame(dept, data = id)
dept <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80")); rm(shape, id)

### Region
reg   <- unionSpatialPolygons(com , as.data.frame(com)$REG)
# nb matrix  
reg_nb <- 
  reg %>%
  poly2nb(queen = T, snap = 0.03) %>% 
  nb2mat()
colnames(reg_nb) <- row.names(reg_nb)
# centroid
reg_centroid <- coordinates(reg)
colnames(reg_centroid) <- c('lon', 'lat')
# spatial polygons data frame
id <- data.frame(id = getSpPPolygonsIDSlots(reg))
row.names(id) <- id$id
shape <- SpatialPolygonsDataFrame(reg, data = id)
reg <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80")); rm(shape, id)

# Save results ------------------------------------------------------------

setwd(midsave)
  
writePolyShape(com, "shape_com")
write.csv(com_nb, 'CM_com.csv')
write.csv(com_centroid, 'centroid_com.csv')
  
writePolyShape(arr, "shape_arr")
write.csv(arr_nb, 'CM_arr.csv')
write.csv(arr_centroid, 'centroid_arr.csv')
  
writePolyShape(dept, "shape_dept")
write.csv(dept_nb, 'CM_dept.csv')
write.csv(dept_centroid, 'centroid_dept.csv')

writePolyShape(reg, "shape_reg")
write.csv(reg_nb, 'CM_reg.csv')
write.csv(reg_centroid, 'centroid_reg.csv')

setwd(main)
