##########
# CREATE TIMESERIES FIGURES
##########

source('code/analysis/load.R')
library(zoo)

timeseries_figure = function(typearg, misinfoarg=FALSE, version, cluster='geography', add_controls = '') {
  
  agg_deaths = make_coef_df('deaths', typearg, misinfo=misinfoarg, version=version, data, cluster=cluster, add_controls=add_controls) %>% 
    mutate(outcome='Deaths')
  
  agg_cases = make_coef_df('cases', typearg, misinfo=misinfoarg, version=version, data, cluster=cluster, add_controls=add_controls) %>% 
    mutate(outcome='Cases')
  
  agg = agg_deaths %>%
    bind_rows(agg_cases)
  
  plot = ggplot(agg, aes(x = elapdate, y = coef)) + 
    geom_point(aes(col = outcome, shape=outcome), fill='black', size=2) + 
    geom_ribbon(aes(ymin=error_low, ymax=error_high, fill=outcome), alpha=0.07, col=NA) +
    ylab(str_interp('${typearg} estimate')) +
    labs(fill='Outcome', col='Outcome', shape='Outcome') +
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

# Timeseries
timeseries_figure('OLS', misinfo=FALSE, version='V1') %>% ggsave(str_interp('output/figures/OLS-timeseries.png'), ., width=6, height=4, units='in') 
timeseries_figure('2SLS', misinfo=FALSE, version='V1') %>% ggsave(str_interp('output/figures/V1-timeseries.png'), ., width=6, height=4, units='in')

# Tiemseries with state clustering
timeseries_figure('2SLS', misinfo=FALSE, version='V1', cluster='state_fips') %>% ggsave(str_interp('output/figures/V1-timeseries-state-clustering.png'), ., width=6, height=4, units='in') 
timeseries_figure('OLS', misinfo=FALSE, version='V1', cluster='state_fips') %>% ggsave(str_interp('output/figures/OLS-timeseries-state-clustering.png'), ., width=6, height=4, units='in') 



# Leaveout figure
create_leaveout_figure = function(typearg, misinfoarg=FALSE, version) {
  
  aggs = list()
  
  for (lo in list(c(6, 25, 34, 36, 53), c(6), c(25), c(34), c(36), c(53), '1p', 'highest')) {
    if (lo == '1') {
      to_keep = data %>% 
        filter(elapdate == '2020-04-15') %>% 
        filter(cases <= quantile(cases, 0.99, na.rm=T))
      filtered = data %>% filter(fipscode %in% to_keep$fipscode)
    } else if (lo == 'highest') {
      to_keep = data %>% 
        filter(elapdate == '2020-04-15') %>% 
        group_by(state_fips) %>%
        arrange(desc(cases)) %>% 
        filter(row_number()>1)
      filtered = data %>% filter(fipscode %in% to_keep$fipscode)
    } else {
      filtered = data %>% filter(!(state_fips %in% lo))
    }
    
    agg = make_coef_df('deaths', typearg, misinfo=misinfoarg, version=version, filtered, ci=1.96) %>% 
      mutate(outcome='Deaths') %>%
      bind_rows(make_coef_df('cases', typearg, misinfo=misinfoarg, version=version, filtered, ci=1.96) %>% 
                  mutate(outcome='Cases')) %>%
      mutate(lo = as.character(ifelse(length(lo)>1, paste(lo, collapse=' '), lo)))
    aggs[[length(aggs)+1]] = agg
  }

  agg = bind_rows(aggs) %>%
    mutate(
      leftout = case_when(
        lo == '6 25 34 36 53' ~ 'Leave out CA, MA, NJ, NY, WA',
        lo == '36' ~ 'Leave out NY',
        lo == '53' ~ 'Leave out WA',
        lo == '6'  ~ 'Leave out CA',
        lo == '34' ~ 'Leave out NJ',
        lo == '1p' ~ 'Leave out top 1%',
        lo == 'highest' ~ 'Leave out highest-case county in each state',
        lo == '25'  ~ 'Leave out MA'),
      leftout = factor(leftout, levels = c('Leave out CA', 
                                           'Leave out MA',
                                           'Leave out NJ', 
                                           'Leave out NY', 
                                           'Leave out WA',
                                           'Leave out CA, MA, NJ, NY, WA',
                                           'Leave out top 1%',
                                           'Leave out highest-case county in each state')))
  
  plot = ggplot(agg, aes(x = elapdate, y = coef)) + 
    geom_point(aes(col = outcome, shape = outcome), fill='black', size=2) +
    geom_ribbon(aes(ymin=error_low, ymax=error_high, fill=outcome), alpha=0.15, col=NA) +
    ylab(str_interp('${typearg} estimate')) +
    labs(fill='Outcome', col='Outcome', shape='Outcome') +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          axis.text = element_text(size=12),
          axis.title.y = element_text(size=14),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size=14),
          text = element_text(family='LM Roman 10')) +
    scale_color_manual(values=c('#808080', '#DC143C')) +
    scale_fill_manual(values=c('#808080', '#DC143C')) +
    coord_cartesian(xlim=c(ymd('2020-02-24'),ymd("2020-04-15")), ylim=c(-0.5, 1.1))
  return (plot)
}

(create_leaveout_figure('2SLS', misinfo=FALSE, version='V1') + facet_wrap(~leftout, ncol = 2)) %>%  
 ggsave(str_interp('output/figures/V1-timeseries-leaveout.png'), ., width=7, height=8, units='in') 

(create_leaveout_figure('2SLS', misinfo=FALSE, version='V1') + facet_wrap(~leftout, ncol = 2)) %>%
  ggsave(str_interp('output/figures/V1-timeseries-leaveout.png'), ., width=7, height=8.2, units='in') 


# Timeseries with unbalanced panel
unbalanced_panel_figure = function(typearg, version, threshold) {
  
  agg = make_coef_df('deaths', typearg, version=version, dataarg=data %>% filter(cases>=threshold), begin='2020-03-13') %>% 
    mutate(outcome='Deaths') %>%
    bind_rows(make_coef_df('cases', typearg, version=version, dataarg=data %>% filter(cases>=threshold), begin='2020-03-13') %>% 
                mutate(outcome='Cases'))
  plot = ggplot(agg, aes(x = elapdate, y = coef)) + 
    geom_point(aes(col = outcome, shape=outcome), fill='black', size=2) + 
    geom_ribbon(aes(ymin=error_low, ymax=error_high, fill=outcome), alpha=0.07, col=NA) +
    ylab(str_interp('${typearg} estimate')) +
    labs(fill='Outcome', col='Outcome', shape='Outcome') +
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
    coord_cartesian(xlim=c(ymd('2020-03-13'),ymd("2020-04-15")), ylim=c(-0.5, 1.1))
  
  return (plot)
}

unbalanced_panel_figure('2SLS', version='V1', threshold=1) %>% ggsave(str_interp('output/figures/V1-timeseries-unbalanced.png'), ., width=6, height=4, units='in') 
