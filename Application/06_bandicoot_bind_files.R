# Preparations ------------------------------------------------------------

### Load required packages
library(dplyr, warn = F)
library(data.table, warn = F)

### Load required functions
source('00_directories.R')

### Set folder with bandicoot files
setwd(midsave)

#   variable names
bc_names <- c('id',
              'active_days__allweek__allday__callandtext',
              'number_of_contacts__allweek__allday__text',
              'number_of_contacts__allweek__allday__call',
              'call_duration__allweek__allday__call__std',
              'call_duration__allweek__allday__call__mean',
              'percent_nocturnal__allweek__allday__text',
              'percent_nocturnal__allweek__allday__call',
              'percent_initiated_conversations__allweek__allday__callandtext',
              'percent_initiated_interactions__allweek__allday__call',
              'response_delay_text__allweek__allday__callandtext__std',
              'response_delay_text__allweek__allday__callandtext__mean',
              'response_rate_text__allweek__allday__callandtext',
              'entropy_of_contacts__allweek__allday__text',
              'entropy_of_contacts__allweek__allday__call',
              'balance_of_contacts__allweek__allday__text__std',
              'balance_of_contacts__allweek__allday__text__mean',
              'balance_of_contacts__allweek__allday__call__std',
              'balance_of_contacts__allweek__allday__call__mean',
              'interactions_per_contact__allweek__allday__text__std',
              'interactions_per_contact__allweek__allday__text__mean',
              'interactions_per_contact__allweek__allday__call__std',
              'interactions_per_contact__allweek__allday__call__mean',
              'interevent_time__allweek__allday__text__std',
              'interevent_time__allweek__allday__text__mean',
              'interevent_time__allweek__allday__call__std',
              'interevent_time__allweek__allday__call__mean',
              'percent_pareto_interactions__allweek__allday__text',
              'percent_pareto_interactions__allweek__allday__call',
              'percent_pareto_durations__allweek__allday__call',
              'number_of_interactions__allweek__allday__text',
              'number_of_interactions__allweek__allday__call',
              'number_of_interaction_in__allweek__allday__text',
              'number_of_interaction_in__allweek__allday__call',
              'number_of_interaction_out__allweek__allday__text',
              'number_of_interaction_out__allweek__allday__call',
              'number_of_antennas__allweek__allday',
              'entropy_of_antennas__allweek__allday',
              'percent_at_home__allweek__allday',
              'radius_of_gyration__allweek__allday',
              'frequent_antennas__allweek__allday',
              'churn_rate__std',
              'churn_rate__mean')

#   function for binding files from HDFS cluster into one
load_n_bind <- function(d){
  
  # get into directory
  setwd(paste0(getwd(),'/', d, '/'))
  
  # get list of files
  l <- 
    data.frame(file = list.files()) %>% 
    filter(file = grepl('part', file))
  
  # initial dataframe
  df <- data.frame()
  
  # read in all existing files in subfolder
  for(i in 1:nrow(l)){
    
    message(l[i,])
    
    temp <- read.csv2(as.character(l[i,]), header = F)
    
    df <- bind_rows(df, temp)
    
    rm(temp);gc()
    
  }
  
  # back into parent directory
  setwd(paste0(getwd(), '/..'))
  
  # variable names
  colnames(df) <- bc_names
  
  # return final dataframe
  return(df)
}

#   function to merge variables to tower level
bc_to_tower <- function(months){
  
  for(m in months){
    
    #   which month?
    message(paste0('Staring on month ', m))
    
    #   get metrics for month
    if(!file.exists(paste0('bc_metrics', m, '.rds'))){
      bc <- load_n_bind(d = m)
      saveRDS(bc, file = paste0('bc_metrics', m, '.rds'))
    } else {
      bc <- readRDS(paste0('bc_metrics', m, '.rds'))
    }
    gc()
    
    #   get interactions
    id <- fread(paste0('id_tower_', m, '.csv'));gc()
    
    #   split in four smaller parts by tower id
    idp1 <- filter(id, tower <= 200);gc()
    idp2 <- filter(id, tower > 200 & tower <= 400);gc()
    idp3 <- filter(id, tower > 400 & tower <= 800);gc()
    idp4 <- filter(id, tower > 800);gc()
    
    #   clean RAM again
    rm(id);gc()
    
    #   create the 4 merged parts
    p1 <- 
      bc %>% 
      group_by(id) %>% 
      left_join(idp1, by = 'id') %>% 
      group_by(tower) %>% 
      select(-id) %>% 
      mutate_each(funs(as.numeric(.))) %>% 
      summarise_each(funs((sum(.*interactions, na.rm = T)/ sum(interactions))))
    gc()
    message(paste0('Done with p1 out of 4 in month ', m))
    
    p2 <- 
      bc %>% 
      group_by(id) %>% 
      left_join(idp2, by = 'id') %>% 
      group_by(tower) %>% 
      select(-id) %>% 
      mutate_each(funs(as.numeric(.))) %>% 
      summarise_each(funs((sum(.*interactions, na.rm = T)/ sum(interactions))))
    gc()
    message(paste0('Done with p2 out of 4 in month ', m))
    
    p3 <- 
      bc %>% 
      group_by(id) %>% 
      left_join(idp3, by = 'id') %>% 
      group_by(tower) %>% 
      select(-id) %>% 
      mutate_each(funs(as.numeric(.))) %>% 
      summarise_each(funs((sum(.*interactions, na.rm = T)/ sum(interactions))))
    gc()
    message(paste0('Done with p3 out of 4 in month ', m))
    
    p4 <- 
      bc %>% 
      group_by(id) %>% 
      left_join(idp4, by = 'id') %>% 
      group_by(tower) %>% 
      select(-id) %>% 
      mutate_each(funs(as.numeric(.))) %>% 
      summarise_each(funs((sum(.*interactions, na.rm = T)/ sum(interactions))))
    gc()
    message(paste0('Done with p4 out of 4 in month ', m))
    
    #   bind parts together
    bc_tower <- 
      bind_rows(p1,p2,p3,p4) %>% 
      select(-interactions) %>% 
      na.omit()
    
    #   save results
    saveRDS(bc_tower, file = paste0(dropboxdir, paste0('/bandicoot/bc_metrics_tower', m, '.rds')))
    
    #   clean up
    rm(bc, idp1, idp2, idp3, idp4, p1, p2, p3, p4, bc_tower);gc()
    
  }
}


# Apply -------------------------------------------------------------------
months = c(paste0(0, 1:9), 10:12) # all months

bc_to_tower(months = months)


# Bind them all together --------------------------------------------------
setwd(midsave)

t01 <- readRDS('bc_metrics_tower01.rds')
t02 <- readRDS('bc_metrics_tower02.rds')
t03 <- readRDS('bc_metrics_tower03.rds')
t04 <- readRDS('bc_metrics_tower04.rds')
t05 <- readRDS('bc_metrics_tower05.rds')
t06 <- readRDS('bc_metrics_tower06.rds')
t07 <- readRDS('bc_metrics_tower07.rds')
t08 <- readRDS('bc_metrics_tower08.rds')
t09 <- readRDS('bc_metrics_tower09.rds')
t10 <- readRDS('bc_metrics_tower10.rds')
t11 <- readRDS('bc_metrics_tower11.rds')
t12 <- readRDS('bc_metrics_tower12.rds')

t_all <- 
  bind_rows(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12) %>% 
  group_by(tower) %>% 
  summarise_each(funs(mean))

saveRDS(t_all, 'bc_metrics_tower.rds')
  


