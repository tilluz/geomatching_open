################################################
### This file is designed to allocate the individuals that are not distributed by the Commune Key
### CCOD_CRCA to the respective commune
### Problem to cope with: Some individuals are located to a non-commune entity. This entity
### is listed only in the Point Coordinates File.
### 1) this file will connect the point coordinates to the shapes (communes).
### 2) this link is used to give the so far unallocated individuals a clear commune identifier (CCOD_CRCA)
### 3) Generate dependent variables
### 4) Reduce census to different administrative levels
### 5) Prepare a crosswalk for all spatial entities
################################################

### Load required packages
library(data.table)
library(foreign)
library(rgdal)
library(sp)
library(dplyr)

## Load required functions
source('directories.R')
source('helpers/functions_census.R')

######################################
#### First Part: Points to Shapes ####
######################################

### Load Commune Level Shapes as well as chef de lieu and Transform UTM to GPS
source('helpers/load_shape.R')

## Extract only important information for 
chef.df <- subset(chef.df, select = c(LOCALITE,lon,lat))

### Reconstruct regional code, find miscoded areas in shape file and recode
shape.df$crca_m <- paste0(shape.df$COD_REG,shape.df$COD_DEPT,shape.df$COD_CAV,shape.df$COD_CRCA)
check           <- shape.df[,c('CCOD_CRCA','crca_m')]
diff_id         <- check[check[,1] != check[,2],]

## Perform check
if(nrow(diff_id) > 0){
  print(paste0('There are ',nrow(diff_id),' ids coded differently! Recoding ..'))
  
  ## Copy new entries
  shape.df$CCOD_CRCA <- shape.df$crca_m
}

## Clean up
shape.df$crca_m <- NULL
rm(check,diff_id);gc()




### Allocate Points of chef de milieu to Communes

## Turn main map into data frame
map.df    <- fortify(sen)
map.df$id <- as.numeric(map.df$id)+1 # correct for area id

## Create crosswalk between area id from shape file (OBJECTID_1) and region code from census (CCOD_CRCA)
xwalk <- shape.df[,c('CCOD_CRCA','OBJECTID_1')]
names(xwalk) <- c('crca','id')

## Add crosswalk to dataframe of main map
map.df <- merge(map.df, xwalk, by = 'id', all.x = T)
  
## Prepare allocation of chef de lieu points to communes
com.allocation <- data.frame(crca_n=chef.df$LOCALITE,id=NA)

## Run allocation
for(i in shape.df$OBJECTID_1){
  # Extract polygon info
  com <- subset(map.df,map.df$id == i)
  # Create indicator for polygon allocation
  temp <- ifelse((point.in.polygon(as.numeric(chef.df$lon),as.numeric(chef.df$lat), 
                                   as.numeric(com$long), as.numeric(com$lat), 
                                   mode.checked=FALSE))==1,i,NA)
  # Copy info to unified first column
  com.allocation[!is.na(temp),'id'] <- i
  # Remove temporary objects
  rm(temp,com)
  gc()
}

## Merge region code from census to it
com.allocation <- as.data.table(merge(com.allocation, xwalk, by = 'id', all.x = T))
setkey(com.allocation, 'crca_n')
  



##########################################################################
#### Second Part: Allocate so far unallocated individuals to communes ####
##########################################################################
setwd(input)

### Load census
census <- as.data.table(read.spss('spss_car_individus_10eme_dr.sav', to.data.frame = T))
setwd(main)
  
### Extract first 8 digits of unique identifier to get commune level identification
census$CCOD_CRCA <- substr(census$IDDR, start = 1, stop = 8)

### Get individuals who do not match the crca shape identifiers
no_shape <- census[!census$CCOD_CRCA %in% shape.df$CCOD_CRCA]
ifelse(any(is.na(no_shape$cacr)) == F,'No NAs found!','Error: NAs in dataset!') # check for NAs -> none

### Create subset of individuals without shape and their crca name
ind <- subset(no_shape, select = c('IDDR', 'cacr'))
setnames(ind, 'cacr', 'crca_n') # name conventions
ind <- unique(ind) # get only unique combos for xwalk

### Unifying crca names
ind$crca_n <- gsub("COM .","",ind$crca_n, fixed = T)
ind$crca_n <- gsub("COM. ","",ind$crca_n, fixed = T)
ind$crca_n <- gsub("'","",ind$crca_n, fixed = T)  ## here, "'" has be be recopied every time from unique(ind$crca_n)
ind$crca_n[ind$crca_n == "CAYAR"] <- "NDAME"
ind$crca_n[ind$crca_n == "DIAOUBE- KABENDOU"] <- "DIAOBE-KABENDOU"
ind$crca_n[ind$crca_n == "DIANAH MALARY"] <- "DIANNAH MALARY"
ind$crca_n[ind$crca_n == "DIOFIOR"] <- "DIOFFIOR"
ind$crca_n[ind$crca_n == "GUEDE CHANTIER"] <- "GUEDE CHANTIERS"
ind$crca_n[ind$crca_n == "GUEKOKH"] <- "NGUEKHOKH"
ind$crca_n[ind$crca_n == "GUINGUINEO"] <- "GUINGUENEO"
ind$crca_n[ind$crca_n == "JAXAAY PARCELLE NIAKOUL RAP"] <- "DIAXAY PARCELLE NIAKOURAP"
ind$crca_n[ind$crca_n == "JOAL- FADIOUTH"] <- "JOAL-FADIOUTH"
ind$crca_n[ind$crca_n == "MADINA WANDIFA"] <- "MEDINA WANDIFA"
ind$crca_n[ind$crca_n == "MALEM HODDAR"] <- "MALEME HODDAR"
ind$crca_n[ind$crca_n == "MALEM NIANI"] <- "MALEME NIANI"
ind$crca_n[ind$crca_n == "NDIANDANE"] <- "THILLE BOUBACAR"
ind$crca_n[ind$crca_n == "NDOMBO SANDJIRY"] <- "NDOMBO"
ind$crca_n[ind$crca_n == "NGUIDILOGNE"] <- "NGUIDJILONE"
ind$crca_n[ind$crca_n == "NIORO"] <- "NIORO ALASSANE TALL" ## or NIORO DU RIP
ind$crca_n[ind$crca_n == "POPOGUINE"] <- "POPENGUINE"
ind$crca_n[ind$crca_n == "ROSSO-SENEGAL"] <- "ROSSO SENEGAL"
ind$crca_n[ind$crca_n == "SAINT LOUIS"] <- "SAINT-LOUIS"
ind$crca_n[ind$crca_n == "SAMINE"] <- "SAMINE ESCALE"
ind$crca_n[ind$crca_n == "SINTHIOU BAMANBE-BANADJI"] <- "SINTHIOU BAMAMBE"
ind$crca_n[ind$crca_n == "THIONCK-ESSYL"] <- "THIONK ESSYL"
ind$crca_n[ind$crca_n == "K. MANDONGO"] <- "KEUR MANDONGO (KANTEYENE)"

### Merge unified names to communes
ind <- as.data.frame(ind)
com.allocation <- as.data.frame(com.allocation)
ind <- merge(x = ind, y = com.allocation, by = 'crca_n', all.x = T)
  
### Remove duplicates
temp <- ind[,c('crca_n','id')]; temp <- unique(temp)
kill <- names(table(temp$crca_n)[table(temp$crca_n)>1])
ind <- ind[-as.numeric(row.names(ind[substr(ind$IDDR,1,3) != substr(ind$crca,1,3) & ind$crca_n %in% kill,])),]
rm(temp,kill)
  
### testing for right merge (Note: Some households simply cannot be matched (around 34 households))
ifelse(any(is.na(ind$IDDR)) == T, 'Merge gone wrong, redo with right apostrophe!',
       'Merge gone right, no NAs in IDDR!')

### Merge new region codes to census
setnames(ind, 'crca', 'CRCA_NEW')
census <- as.data.frame(census)
census <- merge(census, ind[,c('IDDR','CRCA_NEW')], by = 'IDDR', all.x = T)

## if NA, use old region code
census[is.na(census$CRCA_NEW),'CRCA_NEW'] <- census[is.na(census$CRCA_NEW),'CCOD_CRCA'] 

## Find the remaining households that cannot be allocated and delete them
check <- unique(census[!census$CRCA_NEW %in% shape.df$CCOD_CRCA,'IDDR'])
census <- census[!census$IDDR %in% check,]
rm(check)




##################################################
#### Third Part: Generate dependent variables ####
##################################################
  
### Create population count
census$popcount <- census$coef2*10

### Reduce census to variables of interest
census_sub <- subset(census, select = c('CRCA_NEW','cacr','popcount', 'A05', 
                                          'A04A','A04B','A06','A07','A07HAM','A08_conces','A09_Menage',
                                          'B06','B08','B16','B21','B22','B23','B24','B25','B26','B34',
                                          'B36','B38','B41'))
  
### Washington Group questions to identify persons with disabilities
## variables of interest
vars <- c('B21','B22','B23','B24','B25','B26')

## apply binarizing
census_sub <- cbind(census_sub[,c('CRCA_NEW','cacr','popcount', 'A05', 
                                  'A04A','A04B','A06','A07','A07HAM','A08_conces','A09_Menage',
                                  'B06','B08','B16','B34','B36','B38','B41')],
                    as.data.frame(lapply(census_sub[,vars], FUN = function(x) {sapply(x,FUN=binarize)})))
  
### Binarize Gender to 0 (Male) and 1 (Female)
census_sub$sexfem <- as.numeric(census_sub$B06)-1
census_sub$B06 <- NULL
  
### Literacy
census_sub <- 
  census_sub %>% 
  mutate(B34 = as.integer(B34) - 1,
         literatedhs = ifelse(B08 < 50 & B08 > 14, B34, NA))

### Binarize Nationality into Senegalese (1) and Foreigner (0)
census_sub$natsen <- ifelse(census_sub$B16 == "S?n?gal",1,0)
census_sub$B16 <- NULL
  
### Binarize Working Status
census_sub$work <- ifelse(census_sub$B36 == "Occup?",1,0)
census_sub$workhome <- ifelse(census_sub$B36 == "Occup? au foyer",1,0)
census_sub$workno <- ifelse(census_sub$B36 == "Chômeur ayant travaillé" | census_sub$B36 == "A la recherche d'un premier emploi"
                            & census_sub$B08 > 9,1,0)
census_sub$workno[census_sub$B08 < 10] <- NA
census_sub$worknoold <- ifelse(census_sub$B36 != "Occup?" & census_sub$B36 != "Occup? au foyer", 1, 0)
census_sub$B36 <- NULL

### Binarize Working Situation
census_sub$empself <- ifelse(census_sub$B38 == "Travailleur ind?pendant",1,0)
census_sub$emp <- ifelse(census_sub$B38 == "Salari?/Employ? permanent" 
                         | census_sub$B38 == "Salari?/Employ? temporaire",1,0)
census_sub$empoth <- ifelse(census_sub$B38 != "Travailleur indépendant" 
                            & census_sub$B38 != "Salari?/Employ? permanent"
                            & census_sub$B38 != "Salari?/Employ? temporaire",1,0)
census_sub$B38 <- NULL

### Binarize Matrimonial Status
census_sub$marmono <- ifelse(census_sub$B41 == 'Monogame',1,0)
census_sub$marpoly <- ifelse(census_sub$B41 == 'Poly/?re épouse' | census_sub$B41 == 'Poly/2ié  épouse/2 épouses' | 
                               census_sub$B41 == 'Poly/3iè épouse/3 épouses' | census_sub$B41 == 'Poly/4iè épouse /4 épouses'
                             | census_sub$B41 == 'Poly/5iè épouse /5 épouses',1,0)
census_sub$maroth <- ifelse(census_sub$B41 == 'Célibataire' | census_sub$B41 == 'Veuf/Veuve' | 
                              census_sub$B41 == 'Divorcé(e)' | census_sub$B41 == 'Union libre(concubinage)' |
                              census_sub$B41 == 'Séparé(e)',1,0)
census_sub$B41 <- NULL

### Set easily identifiable names
census_sub <- as.data.table(census_sub)
setnames(census_sub,vars,c('vis_impaired','acc_impaired','mov_impaired','con_impaired','oth_impaired','spe_impaired'))
setnames(census_sub,'B34','literate')
setnames(census_sub,'B08','age')



  
#####################################################
#### Fourth Part: Reduce census to commune level ####
#####################################################

### Create new crosswalk
key <- unique(shape.df[,c('OBJECTID_1','DEPT','ARR','CCOD_CRCA','REG')])
setnames(key,'CCOD_CRCA','CRCA_NEW')

### Rename Misnumbered: Two different communes (OBJECTID_1) have the same commune identifier ("092203201").
### "09220302", because "09220303" existing, but no "09220302", so assumption of wrongful numbering realistic
### further, 06120203 is for 165 and 177, so 177 gets new CRCA. These changes also have to be applied to the
### census file
key$CRCA_NEW[key$OBJECTID_1 == 95] <- "09220302"
key$CRCA_NEW[key$OBJECTID_1 == 177] <- "06120204"
census_sub$CRCA_NEW[census_sub$cacr == 'COM. NDOFFANE'] <- "06120204"
census_sub$CRCA_NEW[census_sub$cacr == 'COM. KARANG POSTE'] <- "09220302"

### Prepare aggregation
# Add crosswalk to census
key <- 
  key %>%
  group_by(CRCA_NEW) %>%  
  unique
census_sub <- merge(census_sub, key, by = 'CRCA_NEW', all.x = T)
  
# Rename NUTS 4
setnames(census_sub, 'OBJECTID_1', 'com')

# Align variable name style
names(census_sub)[-1] <- tolower(names(census_sub))[-1]

# Align variable type
census_sub$reg <- as.character(census_sub$reg)
census_sub$dept <- as.character(census_sub$dept)
census_sub$arr <- as.character(census_sub$arr)

# Correct region names
census_sub$reg <- gsub('-', '_', census_sub$reg)

# Correct department names
census_sub$dept <- gsub('-', '_', census_sub$dept)
census_sub$dept <- gsub('\u0092', '', census_sub$dept) # The ' has to be adjusted every time! unique(census_sub$dept)

# Correct arrondissement names
census_sub$arr <- gsub('-', '_', census_sub$arr)
census_sub$arr[census_sub$arr == "DAROU MINAME II"] <- "DAROU MINAM II"
census_sub$arr[census_sub$arr == "MEUR MOMAR SARR"] <- "KEUR MOMAR SARR"
census_sub$arr[census_sub$arr == "NDIAYE BERESS"] <- "NDIAYE MBERESS"
census_sub$arr[census_sub$arr == "SARA BIDJI"] <- "SARE BIDJI"

# Change spaces for underlines
census_sub$reg <- gsub(' ', '_', census_sub$reg)
census_sub$dept <- gsub(' ', '_', census_sub$dept)
census_sub$arr <- gsub(' ', '_', census_sub$arr)

##################################################################
#### Fifth Part: Prepare a crosswalk for all spatial entities ####
##################################################################
setwd(midsave)
write.csv(key, 'crca_info.csv',row.names = F)
setwd(main)
###################################################
#### Sixth Part: Prepare rdata file for saving ####
###################################################
  
census <- census_sub
setwd(midsave)

rm(census_sub,
   chef.df, com.allocation, ind, key, map.df, no_shape, shape.df,
   xwalk, cacr_id, censusdir, gitdir, i, midsave, sen, shapedir, vars, binarize, Mode, towerdir)
save.image('census.RData')
