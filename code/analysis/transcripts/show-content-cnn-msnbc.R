##########
# CREATE CNN AND MSNBC WORD COUNT FIGURE
##########


library(haven)
library(tidyverse)
library(lubridate)
library(zoo)
library(extrafont)


# Read word count data
word_counts= read_dta('data/working/transcript-word-counts.dta') %>% 
  mutate(term_count = ifelse(show=='The Situation Room', term_count/2, term_count)) %>% # Manual fix: Situation Room is 2 hours, not 1
  arrange(elapdate) %>% 
  group_by(show) %>%
  mutate(count_rolling=rollapply(log(1+term_count),7,mean,align='center',fill=NA)) %>% 
  filter(network != 'Fox News')

plot = ggplot(word_counts, aes(x=elapdate, col=network)) + 
  geom_point(aes(y = log(term_count+1)), size=1, alpha=0.45) +
  geom_line(aes(y=count_rolling, group=show), size=1.5, alpha=0.8) +
  labs(col = 'Show', shape='Show') +
  guides(linetype=F, size=F) +
  ylab('Log (word count + 1)') +
  theme_bw() +
  theme(
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.title.x = element_blank(),
        text = element_text(family='LM Roman 10'),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=10)) +
  coord_cartesian(xlim=c(ymd('2020-01-05'), ymd('2020-03-24')),
                  ylim=c(0, 5))

ggsave('output/figures/cnn-msnbc-word-count.png', plot, width=6, height=3, units='in') 

