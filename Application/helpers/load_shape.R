###########################################################
### This file is designed to
### 1) load the shape file of Senegal
### 2) load the chef de lieu shape file of Senegal
### 3) unify naming of spatial entities
###########################################################
		
### Load required packages
library(rgdal)		
library(sp)		
library(ggplot2)		
library(data.table)

### Load required functions
source('helpers/directories.R')

### Load commune level shapes
setwd(input)
sen.ogr <- readOGR(".",'LIMITE_CA_CR_2014')
sen     <- spTransform(sen.ogr, CRS("+proj=longlat +ellps=GRS80"))	
rm(sen.ogr);gc()		

## Get shape index into data frame		
shape.df  <- as.data.frame(sen)		
cacr_id   <- unique(as.character(sen$CCOD_CRCA))		
    		
### Load Point Coordinates		
chef.ogr  <- readOGR(".",'Chef_lieu_entite_administrative_Senegal')		
chef      <- spTransform(chef.ogr, CRS("+proj=longlat +ellps=GRS80"))		
chef.df   <- as.data.frame(chef)		
setnames(chef.df,'coords.x1','lon')		
setnames(chef.df,'coords.x2','lat')	

### Unify naming
shape.df$ARR[shape.df$ARR == "CAS CAS"] <- "CAS-CAS"
shape.df$ARR[shape.df$ARR == "DAROU MINAME II"] <- "DAROU MINAM II"
shape.df$ARR[shape.df$ARR == "MEUR MOMAR SARR"] <- "KEUR MOMAR SARR"
shape.df$ARR[shape.df$ARR == "NDIAYE BERESS"] <- "NDIAYE MBERESS"
shape.df$ARR[shape.df$ARR == "SARA BIDJI"] <- "SARE BIDJI"
shape.df$ARR[shape.df$COD_CAV == "201" & shape.df$ARR == "RUFISQUE"] <- "BAMBILOR"
shape.df$ARR[shape.df$COD_CAV == "201" & shape.df$ARR == "NIODIOR"] <- "DJILOR"
shape.df$ARR[shape.df$COD_CAV == "201" & shape.df$ARR == "TAIF"] <- "KAEL"
shape.df$ARR[shape.df$COD_CAV == "202" & shape.df$ARR == "NDIEDIENG"] <- "KOUMBAL"
shape.df$ARR[shape.df$COD_CAV == "202" & shape.df$ARR == "WACK NGOUNA"] <- "KOUMBAL"
shape.df$ARR[shape.df$CAV == "AR. TENDOUCK" & shape.df$ARR == "LOUDIA OUOLOF"] <- "TENDOUCK"
shape.df$ARR[shape.df$CAV == "AR. LOUDIA OUOLOF" & shape.df$ARR == "TENDOUCK"] <- "LOUDIA OUOLOF"
shape.df$ARR[shape.df$COD_CAV == "202" & shape.df$ARR == "SAGATTA GUETH"] <- "NDANDE"
shape.df$ARR[shape.df$COD_CAV == "203" & shape.df$ARR == "SALDE"] <- "THILE BOUBACAR"

### Remove objects no longer needed		
rm(chef.ogr, chef);gc()		
    		
# change back to code directory		
setwd(main)
