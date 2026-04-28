##########
# CREATE FOX WORD COUNT AND SERIOUSNESS FIGURES
##########

library(tidyverse)
library(haven)
library(zoo)
library(lubridate)
library(extrafont)

# Fox word count figure
counts = read_dta('data/working/transcript-word-counts.dta') %>%
  filter(network=='Fox News') %>%
  mutate(show = ifelse(show %in% c('The Five','The Ingraham Angle','The Story with Martha MacCallum', 
                                   'Special Report with Bret Baier', 'Fox News at Night'), 
                       'Other (mean)', show),
         show = dplyr::recode(show, 'Tucker Carlson'='Carlson','Sean Hannity'='Hannity')) %>%
  group_by(show, elapdate) %>%
  summarise(term_count = mean(term_count)) %>%
  ungroup() %>%
  mutate(show = factor(show, levels=c('Carlson','Other (mean)','Hannity'))) %>%
  group_by(show) %>%
  mutate(count_rolling=rollapply(log(1+term_count),7,mean,align='center',fill=NA))


plot = ggplot(counts, aes(x=elapdate, col = show)) + 
  geom_point(aes(y = log(term_count+1), shape=show), size=1, alpha=0.45) +
  geom_line(aes(y=count_rolling, linetype=show), size=1.5, alpha=0.8) +
  labs(col = 'Show', shape='Show') +
  guides(linetype=F, size=F) +
  ylab('Log (word count + 1)') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        text = element_text(family='LM Roman 10')) +
  coord_cartesian(xlim=c(ymd('2020-01-05'), ymd('2020-03-24'))) +
  scale_x_date(
    breaks = ymd(c('2020-01-05', '2020-02-01', '2020-03-01', '2020-03-24')),
    labels = c('Jan 5', 'Feb 1', 'Mar 1', 'Mar 24')
  ) +
  scale_color_manual(values=c('#39568CFF', '#404040','#440154FF'))

ggsave('output/figures/fox-word-count.png', plot, width=6, height=2.85, units='in') 


# Fox seriousness figure
seriousness = read_dta('data/working/show-seriousness.dta') %>% 
  filter(show!='The Ingraham Angle', show!='The Five') %>%
  arrange(elapdate) %>%
  mutate(show = case_when(
    show=='Tucker Carlson' ~ 'Carlson',
    show=='Sean Hannity' ~ 'Hannity',
    show != 'Tucker Carlson' & show != 'Sean Hannity' ~ 'Other (mean)'
  )) %>%
  group_by(show, elapdate) %>%
  summarise(coronavirus_serious = mean(coronavirus_serious)) %>%
  mutate(serious_rolling=rollapply(coronavirus_serious,7,mean,align='center',fill=NA)) %>%
  ungroup() %>%
  mutate(
    serious_rolling = ifelse(elapdate<ymd('2020-01-15'), 0, serious_rolling)) %>%
  filter(!is.na(serious_rolling)) %>%
  mutate(show = factor(show, levels=c('Carlson','Other (mean)','Hannity'))) 

plot = ggplot(seriousness, aes(x = elapdate, y = serious_rolling, col = show, shape=show, linetype=show)) + 
  geom_point(aes(y = coronavirus_serious), size=1, alpha=0.45) +
  geom_line(size=1.5, alpha=0.8) +
  guides(linetype=F, size=F) +
  theme(legend.position = 'bottom') +
  labs(col='Show', shape='Show') +
  xlab('Date') +
  ylab('Perceived threat of coronavirus') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),        
        text = element_text(family='LM Roman 10'),
        legend.key.size = unit(3, 'line')) +
  coord_cartesian(xlim=c(ymd('2020-01-05'), ymd('2020-03-24'))) +
  scale_x_date(
    breaks = ymd(c('2020-01-05', '2020-02-01', '2020-03-01', '2020-03-24')),
    labels = c('Jan 5', 'Feb 1', 'Mar 1', 'Mar 24')
  ) +
  scale_color_manual(values=c('#39568CFF', '#404040','#440154FF'))

ggsave('output/figures/fox-seriousness.png', plot, width=6, height=3.75, units='in')



