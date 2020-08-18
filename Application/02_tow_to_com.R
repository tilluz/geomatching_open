##############################################
### This file is designed to
### 1) allocate the towers to the communes of Senegal
##############################################

### Load relevant packages
library(data.table)
library(rgdal)
library(sp)
library(ggplot2)

### Load directories
source('00_directories.R')
setwd(input)

### Load Commune Level Shapes and Transform UTM to GPS
sen.ogr <- readOGR(".",'LIMITE_CA_CR_2014')
sen <- spTransform(sen.ogr, CRS("+proj=longlat +ellps=GRS80"))
shape.df <- as.data.frame(sen)
map.df    <- fortify(sen) # turn spatial object into a dataframe
id <- unique(shape.df$CCOD_CRCA)
map.df$id <- as.numeric(map.df$id)+1 # correct commune id count

### Load Tower Coordinates
setwd(input)
GPS <- fread('SITE_ARR_LONLAT_EXACT.csv') # exact tower locations as provided by MNO (located on server)
coord <- subset(GPS, select = c(site_id, lon, lat))

### Allocate towers to Communes
# The function 'point.in.polygon' allocates the GPS coordinate of the tower to the communes it is located in
temp <- data.frame(site_id=coord$site_id,com=NA)
for (i in unique(shape.df$OBJECTID_1)){
  com   <- subset(map.df,map.df$id == i)
  alloc <- ifelse((point.in.polygon(as.numeric(coord$lon),as.numeric(coord$lat),
                          as.numeric(com$long),
                          as.numeric(com$lat),
                          mode.checked=FALSE))==1,i,NA)
  
  temp <- cbind(temp,alloc)
  temp$com[!is.na(temp$alloc)] <- i
  temp$alloc <- NULL
  rm(com, alloc)
  gc()
}

### Add coordinates
temp <- merge(temp, coord, by = 'site_id', all.x = T)

### Correction for Exact
# The towers 487 and 254 are located very close to the border and have therefore not been correctly
# allocated by the above algorithm 
temp$com[temp$site_id == 487] <- 1
temp$com[temp$site_id == 254] <- 387

### Save results
setwd(midsave)
write.csv(temp, 'com_allocation_exact.csv', row.names = F)
setwd(main)



