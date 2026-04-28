##########
# CREATE TRUST FIGURE AND TABLE
##########

library(tidyverse)
library(stringr)
library(stargazer)
library(haven)
library(extrafont)

tablenotes = rjson::fromJSON(file='code/analysis/tablenotes.json')


# Read and prepare trust survey data
trust <- read_dta('data/working/trust-survey.dta') %>% 
  mutate(highincentive=amount==100,
         outcome = factor(outcome, levels = c('covid', 'general','gdp','earnings','unemployment'),
                          labels = c('COVID-19','General economy','GDP','Weekly earnings','Unemployment')),
         incentive = ifelse(highincentive, '$100 incentive', '$10 incentive'))


# Trust figure
summary = trust %>% group_by(outcome, incentive, experiment) %>% 
  summarise(mean = mean(chose_opinion, na.rm=T),
            se = sd(chose_opinion, na.rm=T)/sqrt(n())) %>% 
  mutate(eh = mean+1.96*se, el = mean-1.96*se)

ggplot(summary, aes(x = outcome, y = mean, fill=incentive)) +
  geom_bar(stat='identity', position = position_dodge2(), alpha=0.5) +
  scale_fill_manual(values=c("#6794a7", "#014d64")) +
  geom_errorbar(aes(ymin = mean-1.96*se, ymax=mean+1.96*se, col = incentive), position = position_dodge2()) + 
  scale_color_manual(values=c("#6794a7", "#014d64")) +
  coord_cartesian(ylim=c(0, 1)) +
  facet_wrap(~experiment, ncol=1) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=16),
        strip.text.x = element_text(size=12),
        axis.title.x = element_blank(),
          text = element_text(family='sans')) +
  labs(fill = '', col= '') +
  xlab('Outcome') +
  ylab('Fraction who chose opinion show') 

ggsave('output/figures/trust.png', height=7, width=7)


# Trust table

controls = 'age+gender+employ+hhi+race+hisp+educ'

create_panel = function(x) {
  m1=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x %>% filter(outcome=='COVID-19'))
  m2=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x %>% filter(outcome=='General economy'))
  m3=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x %>% filter(outcome=='GDP'))
  m4=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x %>% filter(outcome=='Weekly earnings'))
  m5=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x %>% filter(outcome=='Unemployment'))
  m6=lm(str_interp('chose_opinion~highincentive+${controls}'), data=x)
  
  means = map_dbl(c('COVID-19','General economy','GDP','Weekly earnings','Unemployment'), function(y) mean(x$chose_opinion[x$outcome==y], na.rm=T))
  means = c(means, mean(x$chose_opinion, na.rm=T)) %>% round(3) %>% format(nsmall=3) 
  
  stargazer(list(m1, m2, m3, m4, m5, m6),
            column.labels = c('COVID-19','Economy','GDP','Earnings','Unemployment', 'Pooled'),
            keep = 'highincentive',
            dep.var.labels = 'Respondent chose opinion show',
            title = 'Trust in opinion shows',
            label = 't:trust-experiment',
            add.lines = list(c('Dep.~var.~mean', means)),
            covariate.labels = '\\$100 incentive')
}

foxpanel = create_panel(trust %>% filter(experiment=='Fox News'))
msnbcpanel = create_panel(trust %>% filter(experiment=='MSNBC'))


table = c(
  foxpanel[4:6],
  '\\begin{threeparttable}',
  foxpanel[7:15],
  '\\multicolumn{7}{l}{\\textbf{Panel A}: Fox News viewers} \\\\',
  '\\midrule',
  foxpanel[c(16:18, 20:21)],
  '\\midrule',
  '\\multicolumn{7}{l}{\\textbf{Panel B}: MSNBC viewers} \\\\',
  '\\midrule',
  msnbcpanel[c(16:18, 20:21)],
  msnbcpanel[26],
  '\\midrule',
  msnbcpanel[28],
  '\\begin{tablenotes} \\small',
  paste0('\\item \\textit{Notes:} ', str_interp(tablenotes[['t:trust-experiment']])),
  '\\end{tablenotes} \\end{threeparttable} \\end{table}'
)
write_lines(table, 'output/tables/trust.tex')
