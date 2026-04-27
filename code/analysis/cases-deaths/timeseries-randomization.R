##########
# CREATE TIMESERIES RANDOMIZATION FIGURES
##########

source('code/analysis/load.R')
library(doParallel)

nreps=1000
set.seed(42)

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

create_randomization_figure = function(agg) {
  agg = agg %>%
    group_by(elapdate, outcome) %>%
    summarise(error_95 = quantile(coef[bootstrap==1], 0.95, na.rm=T),
              error_05 = quantile(coef[bootstrap==1], 0.05, na.rm=T),
              error_975 = quantile(coef[bootstrap==1], 0.975, na.rm=T),
              error_025 = quantile(coef[bootstrap==1], 0.025, na.rm=T),
              coef = coef[bootstrap==0])
  
  plot = ggplot(agg, aes(x = elapdate, y = coef)) + 
    geom_point(aes(col = outcome, shape=outcome), fill='black', size=2) + 
    geom_ribbon(aes(ymin=error_025, ymax=error_975, fill=outcome), alpha=0.2, col=NA) +
    ylab(str_interp('Coefficient estimate')) +
    labs(fill='Outcome', col='Outcome', shape='Outcome') +
    guides(alpha=F) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          axis.text = element_text(size=10),
          axis.title.y = element_text(size=12),
          axis.title.x = element_blank(),
          text = element_text(family='LM Roman 10')) +
    scale_color_manual(values=c('#808080', '#DC143C')) +
    scale_fill_manual(values=c('#808080', '#DC143C')) +
    coord_cartesian(xlim=c(ymd('2020-02-24'),ymd("2020-04-15")))
  return (plot)
}


make_coef_df = function(outcome, type, misinfo=FALSE, dataarg, version, begin=NA, cluster='geography', iv=NA, add_controls='', ci=1.96) {
  print('Estimating DF...')
  iv = str_interp('IV_${version}_hannity')
  ivs = str_interp('(${end}~${iv})')
  base_iv = 'pr_hutput_V1_hannity + pr_hutput_V1_tucker + pr_hutput_V1_ingraham + fox_shr_of_cable + fox_shr_Jan2020 + lpop_2019 + pop_density_2019 + msnbc_shr_of_cable + popweighted_lat + popweighted_lon'
  
  spec = str_interp('${base_iv}+${iv}+${fullcontr}${add_controls}|state_fips|0|${cluster}')
  term = iv
  
  if (str_detect(outcome, 'cases') & is.na(begin)) {
    begin = '2020-02-24'
  } else if (str_detect(outcome, 'deaths') & is.na(begin)) {
    begin = '2020-03-01'
  }
  estimate_model = function(x) {
    model = tryCatch({
      x = x %>% drop_na((str_interp('l${outcome}')), zdiff_viewersHvT, poor_physical_days_raw)
      model1 = felm(as.formula(str_interp('l${outcome}~${spec}')), data=x, weights=x$weights)
      tidy(model1) %>% 
        filter(str_detect(term, str_interp('IV|${end}|${end_misinfo}'))) %>% 
        rename(coef=estimate, se=std.error) %>% 
        dplyr::select(term, coef, se)
      
    }, error = function(err) {
      data.frame('term'=NA, 'coef'=NA, 'se'=NA)
    })
    
  }
  
  df = dataarg %>%
    filter(elapdate>=begin) %>% 
    group_by(elapdate) %>% 
    nest() %>%
    mutate(model = map(data, estimate_model)) %>%
    select(-data) %>%
    ungroup() %>%
    unnest(cols = c(model, elapdate)) %>%
    mutate(error_high = coef+ci*se,
           error_low = coef-ci*se)
  
  return(df)
}

create_bootstrap_df = function(typearg, version=version, misinfoarg=F, reps=3) {

  n_groups = length(unique(data$geography))
  data_sample = data
  aggs = foreach(r = 1:reps, .export=c('make_coef_df', 'sample_n_groups'), .packages=c('tidyverse', 'dplyr','lfe', 'lubridate', 'broom', 'cowplot')) %dopar% {
    if (r != 1) {
      data_sample = data %>% group_by(geography) %>% sample_n_groups(n_groups, replace=T)
    }
    make_coef_df('deaths', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>% 
      mutate(outcome='Deaths') %>%
      bind_rows(make_coef_df('cases', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>% 
                  mutate(outcome='Cases')) %>%
      mutate(rep = r,
             bootstrap = r>1)
  }
  return(aggs %>% bind_rows())
}

create_permutation_df = function(typearg, version=version, misinfoarg=F, reps=3) {

  n_groups = length(unique(data$geography))
  data_sample = data
  aggs = foreach(r = 1:reps, .export=c('make_coef_df', 'sample_n_groups'), .packages=c('tidyverse', 'dplyr','lfe', 'lubridate', 'broom', 'cowplot')) %dopar% {
    if (r != 1) {
      data_sample = data %>% group_by(elapdate) %>%
        mutate(rn = row_number(),
               rn = sample(rn),
               lcases = lcases[rn],
               ldeaths = ldeaths[rn])
    }
    make_coef_df('deaths', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>%
      mutate(outcome='Deaths') %>%
      bind_rows(make_coef_df('cases', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>%
                  mutate(outcome='Cases')) %>%
      mutate(rep = r,
             bootstrap = r>1)
  }

  return(aggs %>% bind_rows())
}

create_randomization_inference_df = function(typearg, version=version, misinfoarg=F, reps=3) {
 
  instrument_df = data %>%
    group_by(geography) %>%
    dplyr::select(contains('pr_hutput'), fox_shr_leaveout, contains('IV_V'), geography) %>%
    summarise_all(function(x) mean(x, na.rm=T))
  data_sample = data

  aggs = foreach(r = 1:reps, .export=c('make_coef_df', 'sample_n_groups'), .packages=c('tidyverse', 'dplyr','lfe', 'lubridate', 'broom', 'cowplot')) %dopar% {
    if (r>1) {
      instrument_df = instrument_df %>%
        mutate_at(vars(contains('pr_hutput')), sample) %>%
        mutate(IV_V1_hannity = scale(pr_hutput_V1_hannity*fox_shr_leaveout),
               IV_V2_hannity = scale(pr_hutput_V2_hannity*fox_shr_leaveout),
               IV_V3_hannity = scale(pr_hutput_V3_hannity*fox_shr_leaveout))

      data_sample = data %>%
        dplyr::select(-c(contains('pr_hutput'), fox_shr_leaveout, contains('IV_V'))) %>%
        inner_join(instrument_df, by='geography')
      }

    make_coef_df('deaths', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>%
      mutate(outcome='Deaths') %>%
      bind_rows(make_coef_df('cases', typearg, version=version, misinfo=misinfoarg, data=data_sample) %>%
                  mutate(outcome='Cases')) %>%
      mutate(rep = r,
             bootstrap = r>1)
  }

  return(aggs %>% bind_rows())
}



create_bootstrap_df(type='RF', version='V1',reps=nreps) %>% create_randomization_figure() %>% ggsave(str_interp('output/figures/V1-bootstrap.png'), ., width=6, height=4, units='in') 
create_permutation_df(type='RF', version='V1',reps=nreps) %>% create_randomization_figure() %>% ggsave(str_interp('output/figures/V1-permutation.png'), ., width=6, height=4, units='in') 
create_randomization_inference_df(type='RF', version='V1', reps=nreps) %>% create_randomization_figure() %>% ggsave(str_interp('output/figures/V1-ri.png'), ., width=6, height=4, units='in') 
