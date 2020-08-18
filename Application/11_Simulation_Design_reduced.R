# Preparations --------------------------------------------------------------------------------

### Load Packages
library(foreign)
library(plyr)
library(data.table)
library(rgdal)
library(caret)
library(randomForest)
library(pROC)
library(sae)
library(sampling)
library(MASS)
library(nlme)
library(spgwr)
library(formula.tools)
library(tcltk2)
library(fields)
library(lattice)
library(BayesSAE)
library(cluster)
library(SemiPar)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(maptools)
library(spdep)
library(alabama)
library(numDeriv)
library(glmnet)

library(gstat)
library(dplyr, warn = F)


### Load Directories
source('00_directories.R')

### Load Model Functions
source('helpers/FH_freq_transnew.R')

### Load Benchmarking functions
source('helpers/Help_function.R')

### Load design effects function
source('helpers/design_effect.R')

### Get Crosswalk
setwd(midsave)
xwalk <- read.csv('spatial_xwalk.csv')

### Get Census Data
load('census.RData')

### Get Shape File
# Commune
shape_com <- readOGR(".",'shape_com')
sh.com <- as.data.frame(shape_com)
# Region
shape_reg <- readOGR(".",'shape_reg')
sh.reg <- as.data.frame(shape_reg)

### Get Contiguity Matrix
# Commune
w.com <- read.csv('CM_com.csv', row.names = 'X')
# Region
w.reg <- read.csv('CM_reg.csv', row.names = 'X')

### Get Centroids
# Commune
c.com <- read.csv('centroid_com.csv')
# Region
c.reg <- read.csv('centroid_reg.csv')

### Get Mobile Phone Datasets
# Tower
x.tow <- read.csv('NUTS5_tower.csv')
# Commune
x.com_old <- read.csv('NUTS4_com.csv')
# Region
x.reg <- read.csv('NUTS1_reg.csv')
setnames(x.reg, 'REG', 'reg')

### Adding bandicoot variables
bandi <- readRDS('bc_metrics_tower.rds')
bandi <- merge(bandi, xwalk[,c('tower','com')], by = 'tower')
bandicoot <- bandi %>% 
        group_by(com) %>% 
        dplyr::summarise_each(funs(mean(., na.rm = T)), -tower)
# x.com <- merge(as.data.frame(bandicoot)[,c(1:23,28:43)], x.com, by = 'com', all.y = T)
x.com_old <- merge(
        as.data.frame(bandicoot)[,c(1:10,12:23,28:43)], as.data.frame(x.com_old)[,c(1:17,19:35)], by = 'com', all.y = T)

### Prepare Census
y <- 'worknoold' #literate #sexfem #literatedhs #work #workno #marpoly #worknoold

# Create Cluster ID
census$clu <- paste0(as.numeric(census$a05),as.character(census$a06),census$com)

# Reduce to simulation variable of interest
census_raw <- as.data.frame(census)[,c('com','reg', y)]

# Reduce to Complete Cases
census <- as.data.table(na.omit(census_raw))

# Prepare first stage of sampling
census_raw$ycount[!is.na(census_raw[,y])] <- 1

census_pps <- 
        census_raw %>% 
        mutate(count = 1) %>% 
        group_by(com, reg) %>% 
        dplyr::summarise(clupop = sum(count, na.rm = T), yclupop = sum(ycount, na.rm = T))

census_pps <- 
        census_raw %>% 
        mutate(count = 1) %>% 
        group_by(reg) %>%
        dplyr::summarise(strpop = sum(count, na.rm = T)) %>% 
        left_join(census_pps, by = 'reg') %>% 
        mutate(w = clupop/strpop) %>% 
        as.data.table()

# Define Simulation Parameters ----------------------------------------------------------------

# Level of Survey
level <- 'reg'
# Model Selection Mechanism
mechanism <- 'lasso'
# Sampling Design
sampling <- 'dhs'
# Transformation
trans <- TRUE
# MSE Estimation        
boot <- FALSE
bootnumber <- 2
# Bench MSE Estimation
bench <- FALSE
benchnumber <- 1
# Simulation Runs
m <- 500 #500
# Sample Size
n <- 20000
# Number of Clusters selected per Strata (only applies to dhs sampling)
k <- 14
# Balanced dataset (same number of rural and urban observations)
balanced <- FALSE

### Set Output Frames
sim.pred <- data.frame()
sim.selection <- data.frame()

for (j in c(
        'point_nocount',
        'voronoi_nocount', 'augvoronoi_nocount', 'wpgvoronoi_nocount',
        'overlap_best_nocount', 'overlap_knn_nocount', 'wpg_overlap_best_nocount', 'wpg_overlap_knn_nocount'
)){
        
        
        x.com <-
                read.csv(paste0('sim_',j,'_adm_old.csv'))
        setnames(x.com, 'MAP_ID', 'com')
        x.com$com <- x.com$com + 1
        
        x.com <- x.com[names(x.com) %in% names(x.com_old)]
        
        setwd(gitdir)
        
        ### Define Variables of Interest
        # Covariates
        x <- names(x.com)[-1]
        
        ### Random Number Generation
        set.seed(1234)
        
        # Prepare First Stage of dhs: Stratified PPS Sampling of clusters within each stratum
        stage1 <- 
                census_pps %>% 
                as.data.frame() %>% 
                filter(w == 1) %>%
                subset(select = c(level,'com'))
        stage1 <- 
                rbindlist(l = list(stage1,census_pps[w != 1][, list('com' = sample(com, k, replace = F, prob = w)), by = level]))
        stage1 <- merge(stage1, as.data.frame(census_pps)[,c('com','clupop','yclupop')], by = 'com', all.x = T)
        
        ### Create Weights for Kriging
        krig <- xwalk %>%
                group_by(com)  %>%
                mutate(towcount = 1) %>%
                dplyr::summarise(towcount = sum(towcount)) %>%
                mutate(weight = mean(towcount, na.rm = T)) %>% 
                mutate(weight = towcount/weight)
        # Add Coordinates to Predictions
        krig <- merge(krig, c.com, by.x = 'com', by.y = 'X', all.y = T)
        
        # Aggregates and list of variables ------------------------------------------------------------
        
        ### Aggregate on NUTS and get full data set (y and x)
        # Commune
        com <- 
                census %>% 
                group_by(com, reg) %>% 
                dplyr::summarise_each_(funs(mean(., na.rm = T)), var = y) %>% 
                full_join(x.com, by = 'com')
        
        # Model Selection ---------------------------------------------------------------------
        
        ### Share of Households Selected within a Cluster (Second Stage)
        l <- n/sum(stage1$yclupop)
        
        # Run Simulation ------------------------------------------------------------------------------
        # Alternatively: Inverse Distance Weighting
        temp <- merge(krig, x.com, by = 'com', all.x = T)
        # kriged <- temp$com[!temp$com %in% na.omit(x.com[,c('com', predictors(results))])$com]
        for(r in x){
                temp1 <- na.omit(temp[,c(r,'lon','lat')])
                coordinates(temp1)=~ lon+lat
                
                temp2 <- temp[,c(r,'lon','lat')]
                coordinates(temp2)=~ lon+lat
                
                eval(parse(text = paste0("test <- as.data.frame(idw(",r," ~ 1, temp1, temp2, idp = 2))[,c(1:3)]")))
                setnames(test,'var1.pred','idw')
                test$com <- rownames(test)
                temp <- merge(temp, test[,c('com','idw')], by = 'com')
                temp$idw[!is.na(temp[,r])] <- temp[,r][!is.na(temp[,r])]
                temp[,r] <- NULL
                setnames(temp, 'idw', r)
        }
        pop <- temp[,c('com',x)]
        
        ### Initiate Loop
        # for (i in 1:m){
        i <- 1
        while (i <= m){
                
                ### Counter
                cat("\r Run", (i-1), "of", m, "Completed")
                flush.console()
                
                ### Survey Sampling Design
                # Two-Stage Stratified Cluster Sampling (Stratum = region)
                # Second Stage: Systematic Sampling within each cluster
                smpl <- 
                        census %>% 
                        filter(com %in% stage1$com) %>% 
                        group_by(com, reg) %>% 
                        sample_frac(size = l, replace = F)
                
                ### Get In-Sample Dataset
                target <- smpl %>%
                        mutate(count = 1) %>% 
                        group_by(com, reg) %>%
                        dplyr::summarise_each_(funs(mean(., na.rm = T)), var = y)
                target <-  smpl %>% 
                        mutate(count = 1) %>% 
                        group_by(com) %>%
                        dplyr::summarise(count = sum(count, na.rm = T)) %>% 
                        left_join(target, by = 'com')
                
                # target <- as.data.frame(merge(target, design, by = 'reg', all.x = T))
                target <- as.data.frame(merge(target, pop[,c('com',x)], by = 'com', all.x = T))
                target <- na.omit(target[,c('com','reg',y,x,'count')])
                target$var <- (target[,y]*(1-target[,y]))/target$count
                #   target$countdes <- target$count/target$quotient
                #   target$vardes <- (target[,y]*(1-target[,y]))/target$countdes
                
                ### Model Construction on Commune-Level
                # Reduce to Available Data
                temp.com.fitting <- as.data.frame(na.omit(target[,c('com','reg',x,y, 'count')])) %>% 
                        filter(count > 30) %>%
                        mutate(y = asin(sqrt(worknoold))) %>%
                        # mutate(y = worknoold) %>%
                        select(-count)
                # Get Best Model
                if(mechanism == 'aic'){
                        # Get Model
                        eval(parse(text = paste0("fit <- lm(y ~ ",paste(unlist(x), collapse =" + "),",data = temp.com.fitting)"))) # ",y,"
                        # Run Selection
                        results <- step(fit, scope = . ~ ., direction = 'backward', k = log(dim(temp.com.fitting)[1]), trace = F)
                        x_select <- predictors(results)
                }
                
                if(mechanism == 'lasso'){
                        hC <- findCorrelation(cor(temp.com.fitting[,x]), names = T, cutoff=0.99, exact = T)
                        temp.com.fitting <- temp.com.fitting %>% select(-all_of(hC))
                        xm <- x[!x %in% hC]
                        # Get Model
                        fit <- cv.glmnet(as.matrix(temp.com.fitting[,xm]), 
                                         as.matrix(temp.com.fitting[,y]),
                                         alpha = 1,
                                         type.measure = "mse",
                                         nfolds = 10)
                        # Run Selection
                        test <- coef(fit, s = "lambda.1se")
                        x_select <- test@Dimnames[[1]][test@i + 1][-1]
                }
                
                if(length(x_select) != 0){
                        # Number of OOC
                        kriged <- pop$com[!pop$com %in% na.omit(x.com[,c('com', x_select)])$com]
                        
                        # Get Predictions
                        eval(parse(text = paste0("fit <- lm(y~",paste(unlist(x_select), collapse =" + "),",data = temp.com.fitting)")))
                        temp.com.fitting$pred <- predict(fit, newdata = temp.com.fitting)
                        # Save Performance Measures
                        selection <- data.table( 'dependent' = y,
                                                 'Rsquared' = summary(fit)$r.squared,
                                                 'adj.Rsquared' = summary(fit)$adj.r.squared,
                                                 'cor' = cor(temp.com.fitting$pred, temp.com.fitting[,'y']),
                                                 'bias' = mean((temp.com.fitting$pred - temp.com.fitting[,'y']), na.rm = T),
                                                 'mae' = mean(abs(temp.com.fitting$pred - temp.com.fitting[,'y']), na.rm = T),
                                                 'mape' = mean(abs((temp.com.fitting[,'y'] - temp.com.fitting$pred)/mean(temp.com.fitting$pred, na.rm = T))), #ifelse(temp.com.fitting[,'y'] == 0,1,temp.com.fitting[,'y'])), na.rm = T),
                                                 'rmse' = sqrt(mean((temp.com.fitting$pred - temp.com.fitting[,'y'])^2, na.rm = T)),
                                                 'OOC' = length(kriged),
                                                 'run' = i,
                                                 'model' = j,
                                                 'n_pred' = dim(as.data.frame(x_select))[1],
                                                 'n' = dim(temp.com.fitting)[1]
                        )
                        
                        sim.selection <- rbindlist(l = list(sim.selection, selection), fill = T, use.names = T)
                        
                        ### Use True Census Values as Output Frame
                        pred <- data.frame(com = as.numeric(sh.com$OBJECTID_1))
                        pred <- merge(pred,com[,c('com',y)], by = 'com', all.x = T)
                        setnames(pred, y, 'true')
                        
                        ### Add Sample Average
                        pred <- merge(pred, na.omit(target)[,c('com',y)], by = 'com', all.x = T)
                        setnames(pred, y, 'smpl')
                        
                        fitfhtrans <- FH_freq_transnew(formula(paste0(y,' ~ ',paste(unlist(x_select), collapse =" + "))),
                                                       vardir = target$var, dataframe_sample = target, saind = target$com,
                                                       dataframe_pop_aux = pop,x.total_saind = pop$com,
                                                       area_count = target$count,trans=trans,Boot=boot,B=bootnumber)
                        
                        
                        
                        # Get predictions
                        temp <- fitfhtrans$est_mean_final
                        # MSE estimates
                        if(boot == TRUE){
                                temp <- left_join(temp, fitfhtrans$mse, by = 'level')
                                temp <- left_join(temp, fitfhtrans$ci %>% select(-pred_in), by = 'level')
                                # # Benchmarked predictions
                                # temp <- merge(temp, fitfhtrans$est_mean_bench, by = 'level')
                        }
                        # MSE estimates of Benchmarked predictions
                        if(bench == TRUE){
                                temp <- merge(temp, fitfhtrans$mse_bench, by = 'level')}
                        
                        pred <- merge(pred, temp, by.x = 'com', by.y = 'level', all.x = T)
                        
                        pred$type[!is.na(pred$smpl) & pred$com %in% kriged == F] <- 'in'
                        pred$type[is.na(pred$smpl) & pred$com %in% kriged == F] <- 'out'
                        pred$type[pred$com %in% kriged == T] <- 'kriged'
                        
                        ### Add additional information
                        pred$run <- i
                        pred$model <- j
                        
                        ### Save Outputs
                        sim.pred <- rbindlist(l = list(sim.pred, pred), fill = T, use.names = T)
                        ### Counter
                        i = i+1
                        print(paste("Run",i,"of",m,"Completed"))
                }
        }
}  

# Save Output ---------------------------------------------------------------------------------
write.csv(sim.pred, 'simulation_prediction_worknoold.csv', row.names = F)
write.csv(sim.selection, 'simulation_selection_worknoold.csv', row.names = F)
setwd(main)
                