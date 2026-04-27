##########
# CREATE TIMING FIGURE
##########

source('code/analysis/load.R')
library(zoo)



estimate_behavior_model = function(x) {
  survey$outcome = (I(survey$change_date<=as.numeric(x-ymd('2020-02-01'))) & survey$change_by_survey_date==1)
  model = lm(as.formula(str_interp('outcome ~ watch_hannity+watch_carlson+watch_other+age+male+retired+fulltime+hhi+whitenohisp+educ+cnnviewer+msnbcviewer')), data=survey) %>% tidy
  hannity_coef = model[model$term=='watch_hannity', 'estimate'] %>% as.numeric()
  carlson_coef = model[model$term=='watch_carlson', 'estimate'] %>% as.numeric()
  return(c(hannity_coef, carlson_coef))
}


survey = read_dta('data/working/behavior-survey.dta') %>% 
  filter(foxviewer==1) %>%
  mutate(watch_other = watch_ingraham | watch_thefive | watch_story | watch_otherfoxshow)
  

bts_data = read_dta('data/working/BTS.dta')


create_timing_figure = function(typearg='2SLS', version='V1') {
  
  agg_deaths = make_coef_df('deaths', typearg, misinfo=FALSE, data, version=version) %>% mutate(outcome='deaths')
  
  agg_cases = make_coef_df('cases', typearg, misinfo=FALSE, data, version=version) %>% mutate(outcome='cases')
  
  agg_ratings = read_dta('data/working/show-seriousness.dta') %>%
    mutate(show = recode(show, 
                         'Sean Hannity' = 'rating_hannity', 
                         'The Five' = 'rating_thefive',
                         'The Ingraham Angle' = 'rating_ingraham',
                         'The Story with Martha MacCallum' = 'rating_thestory',
                         'Tucker Carlson' = 'rating_tucker',
                         'Special Report with Bret Baier' = 'rating_specialrep',
                         'Fox News at Night' = 'rating_newsatnight')) %>% 
    select(-discussed_coronavirus) %>%
    pivot_wider(names_from=show,
                values_from=coronavirus_serious) %>%
    complete(elapdate = seq.Date(min(elapdate), max(elapdate), by="day")) %>%
    mutate_at(vars(rating_hannity, rating_tucker),
              function(x) rollapply(x, width=7, FUN=function(x) mean(x, na.rm=T), fill=NA, align='right')) %>%
    mutate(coef = rating_tucker-rating_hannity,
           outcome = 'misinfo_difference') %>%
    filter(elapdate<'2020-03-25', elapdate>'2020-02-01') %>%
    dplyr::select(coef, elapdate, outcome)  %>%  mutate(coef = coef/max(coef)) 


  agg_behavior = tibble(elapdate=seq(ymd('2020-02-01'), ymd('2020-04-03'), by = '1 day')) %>%
    mutate(model = map(elapdate, estimate_behavior_model),
           hannity_coef = map_dbl(model, 1),
           carlson_coef = map_dbl(model, 2)) %>%
    mutate(survey_difference = carlson_coef - hannity_coef) %>%
    dplyr::select(elapdate, survey_difference) %>%
    rename(coef=survey_difference) %>% mutate(outcome='survey_difference')
  
  bts_merged = bts_data %>% 
    mutate(loutcome = share_stayhome,
           outcome2019 = share_stayhome_2019,
           source='BTS') %>% 
    inner_join(data %>% select(fipscode, county:state_fips, starts_with("pr_hutput_V1"),
                               fox_shr_Jan2020, msnbc_shr_of_cable, fox_shr_of_cable, IV_V1_hannity,
                               popweighted_lat, popweighted_lon, repshare2016, ltotvotes2016,
                               starts_with("pop_"), starts_with("lpop_"), starts_with("edushare_"),
                               poor_physical_days_raw, uninsured_raw, urate_bls, perc_poor2018,
                               beds, nurses, personnel, zdiff_viewersHvT, lmed_hh_inc2018
    ) %>% distinct() , by=c('state_fips','county')) %>% 
    arrange(state_fips, county, elapdate) %>% 
    group_by(state_fips, county) %>% 
    mutate(loutcome = zoo::rollmean(loutcome, 7, na.pad=T),
           outcome2019 = zoo::rollmean(outcome2019, 7, na.pad=T)) %>% 
    mutate(weights=1)
  
  agg_bts = make_coef_df('outcome', '2SLS', misinfo=F, version='V1', bts_merged, cluster='geography', add_controls='+outcome2019') %>% 
    mutate(coef = -coef*40, outcome='Share leaving home') %>% 
    filter(elapdate<='2020-03-24')
  
  agg_all = agg_deaths %>%
    bind_rows(agg_cases) %>%
    bind_rows(agg_ratings) %>%
    bind_rows(agg_behavior) %>%
    bind_rows(agg_bts) %>%
    filter(elapdate>'2020-02-01', elapdate<='2020-04-15') %>%
    group_by(outcome) %>%
    arrange(outcome, elapdate) %>%
    mutate(coef = coef/max(coef, na.rm=T),
           coef_rolling = rollmean(coef, k=7, fill=NA, na.pad=TRUE, align='center')) %>%
    mutate(outcome=recode(outcome,
                          'deaths'='Deaths',
                          'cases'='Cases',
                          'misinfo_difference'='Pandemic coverage gap',
                          'survey_difference'='Behavioral change gap')) %>%
    mutate(outcome = factor(outcome, levels = c('Pandemic coverage gap','Behavioral change gap','Share leaving home', 'Cases','Deaths'))) %>% 
    mutate(gap = str_detect(outcome,'gap'))
  
  
  plot = ggplot(data=agg_all, aes(x = elapdate, col = outcome, shape=outcome, fill=outcome)) + 
    geom_point(size=2, aes(alpha=gap,  y = coef)) + 
    geom_line(aes(y = coef_rolling), size=1.2, alpha=0.65) +
    ylab('Value (as share of maximum)') +
    labs(fill='Outcome', col='Outcome', shape='Outcome') +
    guides(alpha=FALSE) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.text=element_text(size=12),
          legend.title=element_text(size=13),
          axis.text = element_text(size=12),
          axis.title.y = element_text(size=14),
          axis.title.x = element_blank(),
          text = element_text(family='LM Roman 10')) +
    scale_shape_manual(values=c(23, 22, 4, 16, 17)) +
    scale_color_manual(values=c('#D2691E', '#006400', '#00bcd4',  '#808080', '#DC143C')) +
    scale_fill_manual(values=c('#D2691E', '#006400', '#00bcd4',  '#808080', '#DC143C')) +
    scale_alpha_manual(values=c(1, 0.6)) +
    coord_cartesian(xlim = c(ymd('2020-02-01'),ymd('2020-04-15')))
  
  return(plot)
  
}


# Timing 
create_timing_figure() %>% ggsave(str_interp('output/figures/V1-timing.png'), ., width=9, height=4.5, units='in')
