##########
# CREATE MAGNITUDE FILES FOR EPI MODEL CALCULATIONS
##########


source('code/analysis/load.R')
library(zoo)



create_magnitudes = function(approach, ending=end_date, version) {
  df_coefs = make_coef_df('deaths',approach,misinfo=FALSE, version=version, data) %>%
    mutate(type='deaths') %>%
    bind_rows(make_coef_df('cases',approach,misinfo=FALSE, version=version, data) %>%
                mutate(type='cases'))
  data2 = data %>% group_by(elapdate) %>%
    summarise(mean_cases = mean(cases, na.rm=TRUE),
              mean_deaths = mean(deaths, na.rm=TRUE)) %>%
    gather(type, value, -elapdate) %>%
    mutate(type = recode(type, 'mean_cases'='cases', 'mean_deaths'='deaths'),
           mean = log(value+1)) %>%
    inner_join(df_coefs, by=c('elapdate', 'type')) %>%
    mutate(mean_more =  mean+coef, 
           error_more_lower = mean+error_low, 
           error_more_higher = mean+error_high)
  
  df3 = data2 %>% dplyr::select(mean, mean_more, error_more_lower, error_more_higher, elapdate, type) %>%
    gather(key, value, -c('elapdate','type')) %>% 
    filter(str_detect(key, 'mean')) %>%
    inner_join(data2 %>% dplyr::select(error_more_higher, error_more_lower, elapdate, type), by=c('elapdate', 'type')) %>%
    mutate(error_more_higher = ifelse(key=='mean', NA, error_more_higher),
           error_more_lower = ifelse(key=='mean', NA, error_more_lower))  %>%
    mutate(key = recode(key, 'mean' = 'Mean viewership difference', 
                        'mean_more'='1 SD higher viewership difference'),
           key = factor(key, levels = c('Mean viewership difference', 
                                        '1 SD higher viewership difference'))) %>%
    mutate(alpha=ifelse(key == '1 SD higher viewership difference', '0', '1')) %>%
    rename(date=elapdate)
  
  write_dta(df3, str_interp('data/working/epi/magnitudes-${approach}.dta'))
}


create_magnitudes('OLS', version='V1', ending=="2020-11-10")
create_magnitudes('2SLS', version='V1', ending=="2020-11-10")

