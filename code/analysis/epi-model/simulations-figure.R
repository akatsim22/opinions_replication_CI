##########
# CREATE EPI MODEL SIMULATIONS FIGURES
##########

library(tidyverse)
library(haven)
library(lubridate)
library(cowplot)
library(gridExtra)
library(extrafont)

ifr_y = 0.00724
ifr_o = 0.075
ifr_yw = ifr_y/(ifr_y+ifr_o)
ifr_ow = ifr_o/(ifr_y+ifr_o)

nmeanyu = 0.6784
nmeanou = 0.3216
nhigheryt = 0.0097
nhigherot = 0.0112
nhigheryu = nmeanyu-nhigheryt
nhigherou = nmeanou-nhigherot
nhigher = nhigheryt+nhigherot
nmean = 1-nhigher

create_figures = function(approach) {
  folder = ifelse(approach=='2SLS','V1','OLS')
  
  magnitudes = read_dta(str_interp('data/working/epi/magnitudes-${approach}.dta')) %>%
    rename(elapdate=date) %>%
    filter(type=='deaths', !is.na(error_more_higher)) %>% 
    dplyr::select(elapdate, error_more_higher, error_more_lower) %>%
    mutate(variable = '1 SD higher viewership difference', type='2SLS')
  
  magnitudes = magnitudes %>%
    bind_rows(magnitudes %>% mutate(variable='Treatment effect'))

  timeseries = read_dta(str_interp('data/working/epi/epi-deaths-${approach}.dta')) %>%
    mutate(elapdate = ymd(date),
           variable = recode(variable, 'deaths' = 'Mean viewership difference',
                             'deaths_higher' = '1 SD higher viewership difference',
                             'treatment' = 'Treatment effect')) %>%
    select(-date) %>%
    left_join(magnitudes, by=c('elapdate', 'variable', 'type')) %>%
    mutate(variable = factor(variable, levels=c('Mean viewership difference','1 SD higher viewership difference', 'Treatment effect'))) %>%
    group_by(elapdate) %>%
    mutate(error_more_higher = case_when(
             variable == 'Mean viewership difference' ~ NA_real_, 
             variable == '1 SD higher viewership difference' ~ error_more_higher,
             variable == 'Treatment effect' ~ error_more_higher - value[variable=='Mean viewership difference']
           ),
           error_more_lower = case_when(
             variable == 'Mean viewership difference' ~ NA_real_, 
             variable == '1 SD higher viewership difference' ~ error_more_lower,
             variable == 'Treatment effect' ~ error_more_lower - value[variable=='Mean viewership difference']
           ))

  checkpoints = read_dta(str_interp('data/working/epi/epi-betas-${approach}.dta')) %>%
    mutate(elapdate = ymd(date)) %>% select(-date) %>%
    gather(variable, value, -period, -elapdate) %>%
    mutate(variable = recode(variable, 'betamean' = 'Untreated',
                             'betahigher' = 'Treated'),
           value = sqrt(value)) %>%
    filter(elapdate>='2020-02-01')
  
  unt = checkpoints[checkpoints$elapdate=='2020-03-01' & checkpoints$variable=='Untreated', 'value'] %>% unlist
  tre = checkpoints[checkpoints$elapdate=='2020-03-01' & checkpoints$variable=='Treated', 'value'] %>% unlist
  print((tre-unt)/mean(c(tre, unt)))
  
  checkpoints2 = checkpoints %>%
    mutate(weight = ifelse(variable=='Untreated', nmeanyu*ifr_yw+nmeanou*ifr_ow, nhigheryt*ifr_yw+nhigherot*ifr_ow)) %>%
    group_by(elapdate) %>%
    summarise(mean = value[variable=='Untreated'],
              higher = weighted.mean(value, w=weight, na.rm=T)) %>%
    gather(variable, value, -elapdate) %>%
    mutate(variable = recode(variable, 'mean' = 'Mean viewership difference (average)',
                             'higher' = '1 SD higher viewership difference (average)'))
  
  checkpoints3 = checkpoints %>%
    mutate(age = 'Young (treated)', 
           weight = ifelse(variable=='Untreated', nhigheryu/(nhigheryu+nhigheryt), 1-nhigheryu/(nhigheryu+nhigheryt))) %>%
    bind_rows(checkpoints %>% 
                mutate(age='Old (treated)',
                       weight = ifelse(variable=='Untreated', (nhigherou)/(nhigherou+nhigherot), 1-(nhigherou)/(nhigherou+nhigherot)))) %>%
    group_by(elapdate, age) %>%
    summarise(value = weighted.mean(value, w=weight)) %>%
    rename(variable=age) %>%
    bind_rows(checkpoints %>% filter(variable=='Untreated'))
  
  betafigure = ggplot(checkpoints3, aes(x = elapdate, y = value, col=variable)) + 
    geom_line(alpha=0.75, size=1.2) +
    coord_cartesian(xlim = c(ymd('2020-02-01'),ymd('2020-05-01')), ylim=c(0.3,0.55)) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.margin = margin(),
          legend.box.margin = margin(),
          legend.text=element_text(size=13),
          legend.title=element_blank(),
          axis.text = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.title.x = element_blank(),
          text = element_text(family='LM Roman 10')) +
    scale_shape_manual(values=c(15, 16)) +
    scale_color_manual(values=c('#394BA0', '#FAA31B', '#696969', '#CB362C')) +
    ylab('Beta (transmission rate)')
  
  checkpoints = checkpoints %>% bind_rows(checkpoints2) %>%
    mutate(variable = factor(variable, levels = c('Treated', 'Untreated', 
                                                  'Mean viewership difference (average)',
                                                  '1 SD higher viewership difference (average)')))
  
  betafigure = ggplot(checkpoints %>% filter(variable %in% c('Treated','Untreated')), aes(x = elapdate, y = value, col=variable)) +
    geom_line(alpha=0.75, size=1.2) +
    scale_x_date(limits = c(ymd('2020-02-01'), ymd('2020-05-01'))) +
    coord_cartesian(ylim = c(0, max(checkpoints$value))) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.margin = margin(),
          legend.box.margin = margin(),
          legend.text=element_text(size=13),
          legend.title=element_blank(),
          axis.text = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.title.x = element_blank(),
          text = element_text(family='LM Roman 10')) +
    scale_shape_manual(values=c(15, 16)) +
    scale_color_manual(values=c('#394BA0', '#FAA31B', '#696969', '#CB362C')) +
    ylab('Beta (transmission rate)')

  ggsave(str_interp('output/figures/${folder}-simulation-betafigure.png'),height=3, width=7.5, units='in', plot=betafigure)
  
  
  treatment = ggplot(timeseries %>% filter(variable=='Treatment effect', elapdate<=ymd('2020-05-01')), 
         aes(x = elapdate, y = value, group = interaction(variable, type))) + 
    geom_line(aes(linetype=type), size=1, alpha=0.75) +
    geom_ribbon(aes(ymin = error_more_lower, ymax = error_more_higher),
                alpha=0.07) +
    scale_color_manual(values=c('#394BA0', '#FAA31B')) +
    coord_cartesian(xlim = ymd(c('2020-02-01',ymd('2020-05-01')))) +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.box.margin = margin(),
          legend.margin = margin(),
          legend.text=element_text(size=13),
          legend.title=element_blank(),
          axis.text.y = element_text(size=12),
          axis.title.y = element_text(size=14),
          text = element_text(family='LM Roman 10')) +
    ylab('Treatment effect') + xlab('Date')
  
  ggsave(str_interp('output/figures/${folder}-simulation-treatment.png'),height=3, width=7.5, units='in', treatment)
  
}

create_figures('2SLS')
create_figures('OLS')
