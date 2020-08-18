###############################################################################
### This file is designed to
### 1) create various covariates from the the preprocessed CDR data
###############################################################################

# packages
# library(ggvis)
library(dplyr)
library(tidyr)
library(lubridate)
library(geosphere)

### Load Directories
source('00_directories.R')

setwd(midsave)

# data frames on hourly and daily level
df <- 
  read.csv('daily.csv') %>% 
  dplyr::select(time = day, everything()) %>% 
  filter(time != 'NULL')

## Plot (visual analytics)
#   # metrics
#   metric <- c('og_sms', 'ic_sms', 'og_calls', 'ic_calls', 'og_vol', 'ic_vol')
#   
#   # ggvis app
#   df %>% 
#     group_by(time) %>% 
#     select(-tower) %>% 
#     mutate_each(funs(as.numeric)) %>% 
#     summarise_each(funs(sum(., na.rm = T))) %>%
#     as.data.frame() %>% 
#     gather(variable, value, -time) %>% 
#     ggvis( ~time) %>%
#     filter(variable == eval(input_select(
#       choices = metric,
#       selected = 'og_sms',
#       label = "Metric"))) %>% 
#     layer_lines(y = ~value, strokeWidth := 2)
  

### Variable creation
## by aggregated variables

# weekend over whole week ratio
week <- 
  df %>% 
  group_by(tower) %>% 
  mutate(weekday = wday(time)) %>% 
  mutate_each(funs(as.numeric)) %>% 
  summarise(og_sms_week = sum(og_sms, na.rm = T),
            og_sms_weekend = sum(og_sms[!weekday %in% 2:6], na.rm = T),
            ic_sms_week = sum(ic_sms, na.rm = T),
            ic_sms_weekend = sum(ic_sms[!weekday %in% 2:6], na.rm = T),
            og_calls_week = sum(og_calls, na.rm = T),
            og_calls_weekend = sum(og_calls[!weekday %in% 2:6], na.rm = T),
            ic_calls_week = sum(ic_calls, na.rm = T),
            ic_calls_weekend = sum(ic_calls[!weekday %in% 2:6], na.rm = T),
            og_vol_week = sum(og_vol, na.rm = T),
            og_vol_weekend = sum(og_vol[!weekday %in% 2:6], na.rm = T),
            ic_vol_week = sum(ic_vol, na.rm = T),
            ic_vol_weekend = sum(ic_vol[!weekday %in% 2:6], na.rm = T)) %>% 
  mutate(og_sms_week_ratio = og_sms_weekend/og_sms_week,
         ic_sms_week_ratio = ic_sms_weekend/ic_sms_week,
         og_calls_week_ratio = og_calls_weekend/og_calls_week,
         ic_calls_week_ratio = ic_calls_weekend/ic_calls_week,
         og_vol_week_ratio = og_vol_weekend/og_vol_week,
         ic_vol_week_ratio = ic_vol_weekend/ic_vol_week) %>% 
  dplyr::select(tower, 
         og_sms_week_ratio, ic_sms_week_ratio, 
         og_calls_week_ratio, ic_calls_week_ratio,
         og_vol_week_ratio, ic_vol_week_ratio)
  
# Create monthly aggregation
monthly <- 
  df %>% 
  mutate(time = month(as.Date(format(time)))) %>% # extract month
  group_by(time, tower) %>% 
  mutate_each(funs(as.numeric)) %>% 
  summarise_each(funs(sum(., na.rm = T))) %>%  # aggregate
  mutate(calls_ratio = og_calls/ic_calls, sms_ratio = og_sms/ic_sms, vol_ratio = og_vol/ic_vol) %>%  # create ratios
  dplyr::select(month = time, everything())
  
# df on tower level
tow_df <- 
  monthly %>% 
  group_by(tower) %>% 
  summarise(og_sms = sum(og_sms), ic_sms = sum(ic_sms),
            og_calls = sum(og_calls), ic_calls = sum(ic_calls),
            og_vol = sum(og_vol), ic_vol = sum(ic_vol),
            calls_ratio_var = var(calls_ratio, na.rm = T), 
            sms_ratio_var = var(sms_ratio, na.rm = T), 
            vol_ratio_var = var(vol_ratio, na.rm = T)) %>% 
  mutate(calls_ratio = og_calls/ic_calls, sms_ratio = og_sms/ic_sms, 
         vol_ratio = og_vol/ic_vol, sms2calls_ratio = og_sms/og_calls)

# hourly variables, i.e. worktime & farmer vs. office worker ratio (peaks)
hourly <- read.csv('hourly.csv')

work <- 
hourly %>% 
  filter(hour %in% 9:17) %>% # work time as in from 9 to 5
  group_by(tower) %>%
  mutate_each(funs(as.numeric)) %>%
  summarise_each(funs(sum(., na.rm = T))) %>% 
  dplyr::select(tower,
         og_sms_work = og_sms, ic_sms_work = ic_sms,
         og_calls_work = og_calls, ic_calls_work = ic_calls,
         og_vol_work = og_vol, ic_vol_work = ic_vol)

work_peak <- 
  hourly %>% 
  mutate(early_peak = as.numeric(hour %in% 3:5), 
         late_peak = as.numeric(hour %in% 10:12)) %>% # farmer vs office
  group_by(tower) %>% 
  mutate_each(funs(as.numeric)) %>%
  summarise(og_sms_early = sum(og_sms * early_peak, na.rm = T), 
            og_sms_late = sum(og_sms * late_peak, na.rm = T), 
            ic_sms_early = sum(ic_sms * early_peak, na.rm = T), 
            ic_sms_late = sum(ic_sms * late_peak, na.rm = T), 
            og_calls_early = sum(og_calls * early_peak, na.rm = T), 
            og_calls_late = sum(og_calls * late_peak, na.rm = T), 
            ic_calls_early = sum(ic_calls * early_peak, na.rm = T), 
            ic_calls_late = sum(ic_calls * late_peak, na.rm = T), 
            og_vol_early = sum(og_vol * early_peak, na.rm = T), 
            og_vol_late = sum(og_vol * late_peak, na.rm = T), 
            ic_vol_early = sum(ic_vol * early_peak, na.rm = T), 
            ic_vol_late = sum(ic_vol * late_peak, na.rm = T)) %>%
  mutate(og_sms_peak_ratio = og_sms_early/og_sms_late,
         ic_sms_peak_ratio = ic_sms_early/ic_sms_late,
         og_calls_peak_ratio = og_calls_early/og_calls_late,
         ic_calls_peak_ratio = ic_calls_early/ic_calls_late,
         og_vol_peak_ratio = og_vol_early/og_vol_late,
         ic_vol_peak_ratio = ic_vol_early/ic_vol_late) %>% 
  dplyr::select(tower, 
         og_sms_peak_ratio, ic_sms_peak_ratio, 
         og_calls_peak_ratio, ic_calls_peak_ratio,
         og_vol_peak_ratio, ic_vol_peak_ratio)

rm(hourly);gc()


## yearly interactions
yic <- read.csv('yearly_interactions_calls.csv')
yis <- read.csv('yearly_interactions_sms.csv')

# rename columns and reshape it into long format
colnames(yic) <- gsub('X_c', '', colnames(yic), fixed = T)
colnames(yis) <- gsub('X_c', '', colnames(yis), fixed = T)

yi <- 
  yic %>% 
  group_by(og_tower) %>% 
  arrange(og_tower) %>% 
  gather(ic_tower, calls_interactions, -og_tower) %>% 
  mutate_each(funs(as.numeric))

yi <- 
  yis %>% 
  group_by(og_tower) %>% 
  arrange(og_tower) %>% 
  gather(ic_tower, sms_interactions, -og_tower) %>% 
  mutate_each(funs(as.numeric)) %>% 
  full_join(yi, by = c('og_tower', 'ic_tower'))

rm(yic, yis); gc()


# entropy & isolation
entropy <- 
yi %>% 
  group_by(og_tower) %>% 
  mutate(sum_calls = sum(calls_interactions, na.rm = T), sum_sms = sum(sms_interactions, na.rm = T)) %>% 
  summarise(calls_entropy = -sum((calls_interactions/sum_calls)*log2(calls_interactions/sum_calls), na.rm = T),
         sms_entropy = -sum((sms_interactions/sum_sms)*log2(sms_interactions/sum_sms), na.rm = T)) %>% 
  dplyr::select(tower = og_tower, everything())

calls_isolation <- 
  yi %>% 
  dplyr::select(og_tower, ic_tower, calls_interactions) %>% 
  filter(calls_interactions > 0) %>% 
  group_by(og_tower) %>% 
  summarise(calls_isolation = length(ic_tower)) %>% 
  dplyr::select(tower = og_tower, everything())

sms_isolation <- 
  yi %>% 
  dplyr::select(og_tower, ic_tower, sms_interactions) %>% 
  filter(sms_interactions > 0) %>% 
  group_by(og_tower) %>% 
  summarise(sms_isolation = length(ic_tower)) %>% 
  dplyr::select(tower = og_tower, everything())


## distances
tow_coord <- 
  read.csv('towers.csv') %>% 
  dplyr::select(tower = site_id, everything())
dmat <- read.csv('dmat.csv')

# transform dmat
colnames(dmat)[1] <- 'og_tower'
colnames(dmat) <- gsub('X', '', colnames(dmat), fixed = T)


dmat <- 
  dmat %>% 
  group_by(og_tower) %>% 
  arrange(og_tower) %>% 
  gather(ic_tower, dist, -og_tower) %>% 
  mutate_each(funs(as.numeric))

centroid <- 
  tow_coord %>% 
  filter(region_id %in% 1:4) %>% 
  dplyr::select(lon, lat) %>% 
  summarise_each(funs(mean))

tow_coord$dist2d <- apply(dplyr::select(tow_coord, lon, lat), 1, 
                          FUN = function(X) distHaversine(X, centroid)/1000) # to get km

# distanced by interactions
dist_mean <- 
yi %>% 
  left_join(dmat, by = c('og_tower', 'ic_tower'))

rm(dmat); gc()

dist_mean <- 
  dist_mean %>% 
  mutate(calls_dist = calls_interactions*dist,
         sms_dist = sms_interactions*dist) %>% 
  dplyr::select(og_tower, calls_interactions, sms_interactions, calls_dist, sms_dist) %>% 
  group_by(og_tower) %>% 
  summarise_each(funs(sum)) %>% 
  mutate(calls_dist_mean = calls_dist/calls_interactions,
         sms_dist_mean = sms_dist/sms_interactions) %>% 
  dplyr::select(tower = og_tower, everything())

# dakar related metrics
dak_tow <- 
  tow_coord %>% 
  filter(region_id %in% 1:4) %>% # arrondissement 1 to 4 make up the dakar region
  dplyr::select(tower) %>% unique()

dak_vars <- 
yi %>% 
  mutate(dak = as.numeric(ic_tower %in% dak_tow$tower)) %>% 
  group_by(og_tower) %>% 
  summarise(sms2d = sum(sms_interactions*dak, na.rm = T),
            sms = sum(sms_interactions, na.rm = T), 
            calls2d = sum(calls_interactions*dak, na.rm = T),
            calls = sum(calls_interactions, na.rm = T)) %>% 
  mutate(calls2d_ratio = calls2d / calls, sms2d_ratio = sms2d / sms) %>% 
  dplyr::select(tower = og_tower, calls2d_ratio, sms2d_ratio)


# merge everything together
tow <- 
  Reduce(function(x, y) full_join(x, y, by = 'tower', all = T), list(tow_df, entropy, work, work_peak, week,
                                                                     calls_isolation, sms_isolation,
                                                                     dplyr::select(tow_coord, tower, dist2d) %>%
                                                                       as.data.frame(), dak_vars, dist_mean))

# additional vars (ratios)
tow <- 
  tow %>% 
  mutate(og_sms_work_ratio = og_sms_work / og_sms, ic_sms_work_ratio = ic_sms_work / ic_sms,
         og_calls_work_ratio = og_calls_work / og_calls, ic_calls_work_ratio = ic_calls_work / ic_calls,
         og_vol_work_ratio = og_vol_work / og_vol, ic_vol_work_ratio = ic_vol_work / ic_vol)

# save it
write.csv(tow, 'tow_df.csv', row.names = F)

## aggregate variables up to hogher spatial levels
# NUTS5
nuts5 <- 
  tow %>% 
  dplyr::select(tower, 
         calls_ratio, sms_ratio, vol_ratio,
         calls_ratio_var, sms_ratio_var, vol_ratio_var,
         calls_entropy, sms_entropy,
         sms2calls_ratio,
         calls_isolation, sms_isolation,
         og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
         ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
         og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
         ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
         og_calls_week_ratio, og_sms_week_ratio, og_vol_week_ratio,
         ic_calls_week_ratio, ic_sms_week_ratio, ic_vol_week_ratio,
         dist2d,
         calls_dist_mean, sms_dist_mean,
         calls2d_ratio, sms2d_ratio)

write.csv(nuts5, 'NUTS5_tower.csv', row.names = F)
  
# spatial xwalk
xwalk <- read.csv('spatial_xwalk_exact.csv')

# aggregation function
#   AGGREGATE_LEVEL <- function(level, xwalk){
# 
#     xwalk <- xwalk[, c('tower', level)]
#       
#     temp <- 
#       tow %>% 
#       left_join(xwalk, by = 'tower') %>% 
#       group_by(level) %>% 
#       summarise(calls_ratio = sum(og_calls, na.rm = T) / sum(ic_calls, na.rm = T),
#                 sms_ratio = sum(og_sms, na.rm = T) / sum(ic_sms, na.rm = T),
#                 vol_ratio = sum(og_vol, na.rm = T) / sum(ic_vol, na.rm = T))
#     
#     temp <- 
#       tow %>% 
#       left_join(select(xwalk, tower, level), by = 'tower') %>% 
#       group_by(level) %>% 
#       select(calls_ratio_var, sms_ratio_var, vol_ratio_var,
#              calls_entropy, sms_entropy,
#              sms2calls_ratio,
#              calls_isolation, sms_isolation,
#              og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
#              ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
#              og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
#              ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
#              dist2d,
#              calls_dist_mean, sms_dist_mean,
#              calls2d_ratio, sms2d_ratio) %>% 
#       summarise_each(funs(median(., na.rm = T))) %>% 
#       full_join(temp, by = level)
#     
#     return(temp)
#   }

# NUTS4
# nuts4 <- AGGREGATE_LEVEL(level = 'com', xwalk = xwalk)

tow <- subset(tow, !tower %in% c('1106','1111')) ### filter tower with short life and abnormal behaviour out (makes benchmarking impossible)

nuts4 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, com), by = 'tower') %>% 
  group_by(com) %>% 
  summarise(calls_ratio = sum(og_calls, na.rm = T) / sum(ic_calls, na.rm = T),
            sms_ratio = sum(og_sms, na.rm = T) / sum(ic_sms, na.rm = T),
            vol_ratio = sum(og_vol, na.rm = T) / sum(ic_vol, na.rm = T))
  
nuts4 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, com), by = 'tower') %>% 
  group_by(com) %>% 
  dplyr::select(calls_ratio_var, sms_ratio_var, vol_ratio_var,
         calls_entropy, sms_entropy,
         sms2calls_ratio,
         calls_isolation, sms_isolation,
         og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
         ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
         og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
         ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
         og_calls_week_ratio, og_sms_week_ratio, og_vol_week_ratio,
         ic_calls_week_ratio, ic_sms_week_ratio, ic_vol_week_ratio,
         dist2d,
         calls_dist_mean, sms_dist_mean,
         calls2d_ratio, sms2d_ratio) %>% 
  summarise_each(funs(median(., na.rm = T))) %>% 
  full_join(nuts4, by = 'com')
  
write.csv(nuts4, 'NUTS4_com.csv', row.names = F)

# NUTS3
nuts3 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, ARR), by = 'tower') %>% 
  group_by(ARR) %>% 
  summarise(calls_ratio = sum(og_calls, na.rm = T) / sum(ic_calls, na.rm = T),
            sms_ratio = sum(og_sms, na.rm = T) / sum(ic_sms, na.rm = T),
            vol_ratio = sum(og_vol, na.rm = T) / sum(ic_vol, na.rm = T))

nuts3 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, ARR), by = 'tower') %>% 
  group_by(ARR) %>% 
  dplyr::select(calls_ratio_var, sms_ratio_var, vol_ratio_var,
         calls_entropy, sms_entropy,
         sms2calls_ratio,
         calls_isolation, sms_isolation,
         og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
         ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
         og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
         ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
         og_calls_week_ratio, og_sms_week_ratio, og_vol_week_ratio,
         ic_calls_week_ratio, ic_sms_week_ratio, ic_vol_week_ratio,
         dist2d,
         calls_dist_mean, sms_dist_mean,
         calls2d_ratio, sms2d_ratio) %>% 
  summarise_each(funs(median(., na.rm = T))) %>% 
  full_join(nuts3, by = 'ARR')

write.csv(nuts3, 'NUTS3_arr.csv', row.names = F)

# NUTS2
nuts2 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, DEPT), by = 'tower') %>% 
  group_by(DEPT) %>% 
  summarise(calls_ratio = sum(og_calls, na.rm = T) / sum(ic_calls, na.rm = T),
            sms_ratio = sum(og_sms, na.rm = T) / sum(ic_sms, na.rm = T),
            vol_ratio = sum(og_vol, na.rm = T) / sum(ic_vol, na.rm = T))

nuts2 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, DEPT), by = 'tower') %>% 
  group_by(DEPT) %>% 
  dplyr::select(calls_ratio_var, sms_ratio_var, vol_ratio_var,
         calls_entropy, sms_entropy,
         sms2calls_ratio,
         calls_isolation, sms_isolation,
         og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
         ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
         og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
         ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
         og_calls_week_ratio, og_sms_week_ratio, og_vol_week_ratio,
         ic_calls_week_ratio, ic_sms_week_ratio, ic_vol_week_ratio,
         dist2d,
         calls_dist_mean, sms_dist_mean,
         calls2d_ratio, sms2d_ratio) %>% 
  summarise_each(funs(median(., na.rm = T))) %>% 
  full_join(nuts2, by = 'DEPT')

write.csv(nuts2, 'NUTS2_dept.csv', row.names = F)

# NUTS1
nuts1 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, REG), by = 'tower') %>% 
  group_by(REG) %>% 
  summarise(calls_ratio = sum(og_calls, na.rm = T) / sum(ic_calls, na.rm = T),
            sms_ratio = sum(og_sms, na.rm = T) / sum(ic_sms, na.rm = T),
            vol_ratio = sum(og_vol, na.rm = T) / sum(ic_vol, na.rm = T))

nuts1 <- 
  tow %>% 
  left_join(dplyr::select(xwalk, tower, REG), by = 'tower') %>% 
  group_by(REG) %>% 
  dplyr::select(calls_ratio_var, sms_ratio_var, vol_ratio_var,
         calls_entropy, sms_entropy,
         sms2calls_ratio,
         calls_isolation, sms_isolation,
         og_calls_work_ratio, og_sms_work_ratio, og_vol_work_ratio,
         ic_calls_work_ratio, ic_sms_work_ratio, ic_vol_work_ratio,
         og_calls_peak_ratio, og_sms_peak_ratio, og_vol_peak_ratio,
         ic_calls_peak_ratio, ic_sms_peak_ratio, ic_vol_peak_ratio,
         og_calls_week_ratio, og_sms_week_ratio, og_vol_week_ratio,
         ic_calls_week_ratio, ic_sms_week_ratio, ic_vol_week_ratio,
         dist2d,
         calls_dist_mean, sms_dist_mean,
         calls2d_ratio, sms2d_ratio) %>% 
  summarise_each(funs(median(., na.rm = T))) %>% 
  full_join(nuts1, by = 'REG')

write.csv(nuts1, 'NUTS1_reg.csv', row.names = F)
