design.eff <- function(y){
  design.help <- 
    census_raw %>% 
    mutate(count =  1) %>% 
    group_by(reg) %>% 
    summarise(regpop = sum(count, na.rm = T), comcount = length(unique(com))) %>% 
    as.data.frame()
  
  direct <- 
    smpl %>% 
    mutate(count =  1) %>% 
    group_by(reg) %>% 
    summarise_each_(funs(regmean = mean(., na.rm = T), regcount = sum(count, na.rm = T)), vars = y) %>% 
    full_join(design.help, by = 'reg') %>% 
    mutate(directvar = regmean*(1-regmean)/regcount*(1-(regcount/regpop)))
  
  test <- 
    as.data.frame(smpl) %>% 
    mutate(count =  1) %>% 
    group_by(com,reg) %>% 
    summarise_each_(funs(mean(., na.rm = T)), vars = y) %>%
    setnames(y,'commean') %>% 
    left_join(design.help, by = 'reg') %>% 
    group_by(reg) %>% 
    summarise(designvar = var(commean, na.rm = T)/length(unique(com))*(1-length(unique(com))/mean(comcount, na.rm = T))) %>%
    full_join(direct, by = 'reg') %>% 
    mutate(quotient = designvar/directvar) %>% 
    dplyr::select(reg, quotient)
  
  test$quotient[test$quotient == 0] <- 1
  
  
  return(test)
}
