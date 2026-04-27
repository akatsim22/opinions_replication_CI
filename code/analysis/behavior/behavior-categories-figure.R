##########
# CREATE FIGURE ANALYZING BEHAVIOR CATEGORIES
##########

library(lubridate)
library(tidyverse)
library(haven)
library(RColorBrewer)
library(extrafont)

period_recoding = c('2020-01-01'='2020-02-15',
                    '2020-02-15'='2020-02-29',
                    '2020-02-29'='2020-03-15',
                    '2020-03-15'='2020-03-31')

period_recoding_lag = c('2020-02-15'='2020-02-01',
                        '2020-02-29'='2020-02-15',
                        '2020-03-15'='2020-02-29',
                        '2020-03-31'='2020-03-15')

survey = read_dta('data/working/behavior-survey.dta') %>% 
  dplyr::select(change_date, change_by_survey_date, responseid, qualitative,
                hand_wash, social_distancing, stay_at_home, cancel_trips, work_from_home,
                wear_masks, less_shopping, only_date, cancel_church, no_change) %>%
  filter(change_by_survey_date==1) %>%
  filter(no_change!=1, only_date!=1) %>%
  gather(behavior, value, -c('change_date','responseid','qualitative')) %>%
  filter(behavior %in% c('hand_wash', 'social_distancing', 'stay_at_home', 'cancel_trips', 
                         'work_from_home', 'wear_masks', 'less_shopping', 'cancel_church')) %>%
  mutate(behavior = recode(behavior,
                           cancel_church = 'Limit churchgoing',
                           cancel_trips = 'Cancel planned travel',
                           hand_wash = 'Wash hands more frequently',
                           less_shopping = 'Limit shopping',
                           social_distancing = 'Physical distance when interacting with others',
                           stay_at_home = 'Stop leaving home',
                           wear_masks = 'Wear face masks',
                           work_from_home = 'Work from home')) %>%
  mutate(change_date = ymd('2020-02-01')+days(change_date)-1,
         change_period = cut(change_date, breaks = ymd(c('2020-01-01', '2020-02-15',
                                                         '2020-02-29', '2020-03-15', 
                                                         '2020-03-31')), right=T),
         change_period = recode(change_period,
                                !!!period_recoding),
         change_period = date(as.character(change_period))) %>%
  filter(change_date<'2020-04-01')

summary = survey %>% group_by(change_period, behavior) %>%
  summarise(n=sum(value))

switchers = survey %>% group_by(change_period) %>%
  summarise(total_switchers = length(unique(responseid)),
            total_behaviors = sum(value))

summary = summary %>% inner_join(switchers, by='change_period') %>%
  mutate(people_prop = n/total_switchers,
         behavior_prop = n/total_behaviors,
         behavior = factor(behavior, levels = c(
                             'Wash hands more frequently',
                             'Physical distance when interacting with others',
                             'Cancel planned travel',
                             'Stop leaving home',
                             'Limit shopping',
                             'Wear face masks',
                             'Work from home',
                             'Limit churchgoing'))) %>%
  ungroup() %>%
  mutate(change_period_lag=ymd(period_recoding_lag[as.character(summary$change_period)]))


plot = ggplot(summary, aes(y = behavior_prop, fill = behavior)) +
  geom_rect(aes(xmin=change_period_lag, xmax = change_period, ymin=0, ymax=behavior_prop)) +
  facet_wrap(~behavior, ncol=4, labeller = label_wrap_gen(25)) +
  scale_fill_brewer(palette='Dark2') +
  theme_bw() +
  theme(legend.position = 'none',
        legend.margin = margin(),
        legend.box.margin = margin(),
        legend.text=element_text(size=13),
        legend.title=element_blank(),
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size=12),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,'lines'),
        text = element_text(family='LM Roman 10'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm')) +
  ylab('Fraction of total behavior changes') +
  scale_x_date(breaks = ymd(c('2020-02-01', '2020-03-01','2020-04-01')),
               date_labels = c('Feb 1','Mar 1', 'Apr 1'),
               labels = label_wrap(width=5))

ggsave('output/figures/behavior-categories.png', width=10, height=6, plot=plot, units='in')  
