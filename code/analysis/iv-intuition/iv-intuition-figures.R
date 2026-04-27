##########
# CREATE IV INTUITION FIGURE
##########


library(tidyverse)
library(haven)
library(splines)
library(extrafont)

get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}
scaling_factor=0.5
intercept = 0.35

data = read_dta('data/working/iv-intuition.dta') %>%
  filter(elapdate=="2020-02-03") %>%
  mutate(time_zone = recode(timezone,
                            "ETZ"="Eastern",
                            "CTZ"="Central",
                            "MTZ"="Mountain",
                            "PTZ"="Pacific"))

data_clean = data %>%
  filter(program=='HANNITY' | program=='TUCKER CARLSON') %>%
  mutate(show = ifelse(program=='HANNITY', 'Hannity', 'Tucker Carlson Tonight'),
         show = factor(show, levels = c('Tucker Carlson Tonight','Hannity'))) %>%
  mutate(bin = cut_width(relstart, width=0.25)) %>%
  rowwise() %>%
  mutate(relstart = get_midpoint(bin)) %>%
  ungroup()

data2 = data %>% dplyr::select(relstart,hutput_NonFOX, hutput) %>%
  gather(variable, value, -relstart) %>%
  mutate(value = (value-intercept)*scaling_factor,
         variable = recode(variable, hutput = 'All TVs', hutput_NonFOX = 'Leaving out Fox'))

data_collapsed = data_clean %>%
  group_by(relstart, show) %>%
  summarise(frac = n() / 204)

plot1 = ggplot(data_collapsed, aes(x = relstart)) +
  geom_histogram(aes(y = frac, fill=show), stat='identity', position=position_dodge2(preserve='single')) +
  geom_smooth(data=data2, aes(y = value, linetype=variable), se=TRUE, col='black', method='glm', formula=y~poly(x, 2), alpha=0.4) +
  scale_fill_manual(values=c('#39568CFF', '#440154FF')) +
  ylab('Fraction of media markets') +
  xlab('Time relative to sunset') +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/scaling_factor)+intercept, name='Fraction of HHs with TVs turned on')) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size=11),
        axis.title = element_text(size=12),
        strip.text.x = element_text(size=11),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        text = element_text(family='LM Roman 10')) +
  labs(fill = '', linetype='')

ggsave('output/figures/iv-intuition-start-county.png', height=3.5, width=6, units='in', plot=plot1)

data_collapsed = data_clean %>%
  group_by(relstart, show, time_zone) %>%
  summarise(frac = n() / 204)

scaling_factor = 0.45
intercept = 0.35

data2 = data %>% dplyr::select(relstart,hutput_NonFOX, hutput, time_zone) %>%
  gather(variable, value, -relstart, -time_zone) %>%
  mutate(value = (value-intercept)*scaling_factor,
         variable = recode(variable, hutput = 'All', hutput_NonFOX = 'Leaving out Fox'))

plot2 = ggplot(data_collapsed, aes(x = relstart)) +
  geom_histogram(aes(y = frac, fill=show), stat='identity', position=position_dodge2(preserve='single')) +
  geom_smooth(data=data2, aes(y = value, linetype=variable), se=TRUE, col='black', method='glm', formula=y~poly(x, 3), alpha=0.4) +
  facet_wrap(~time_zone) +
  scale_fill_manual(values=c('#39568CFF', '#440154FF')) +
  ylab('Fraction of media markets') +
  xlab('Time relative to sunset') +
  coord_cartesian(ylim=c(0, 0.175)) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/scaling_factor)+intercept, name='Fraction of HHs with TVs turned on')) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size=11),
        axis.title = element_text(size=12),
        strip.text.x = element_text(size=11),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11),
        text = element_text(family='LM Roman 10')) +
  labs(fill = '', linetype='')

ggsave('output/figures/iv-intuition-start-tz.png', height=4, width=6, units='in', plot=plot2)


