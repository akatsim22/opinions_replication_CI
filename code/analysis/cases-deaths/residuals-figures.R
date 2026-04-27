##########
# CREATE RESIDUALS FIGURES
##########

source('code/analysis/load.R')
library(statar)

for (outcome in c('deaths', 'cases')) {
 
  model1 = felm(as.formula(str_interp('l${outcome}~${base_iv}+${fullcontr}|state_fips|0|geography')), data=data %>% filter(elapdate==ifelse(str_detect(outcome, 'cases'), '2020-03-14', '2020-03-28')))
  model2 = felm(as.formula(str_interp('${iv}~${base_iv}+${fullcontr}|state_fips|0|geography')), data=data %>% filter(elapdate==ifelse(str_detect(outcome, 'cases'), '2020-03-14', '2020-03-28')))
            
  resids = data.frame(yresid = as.numeric(resid(model1)), xresid = as.numeric(resid(model2)))
            
  plot = ggplot(resids, aes(x = xresid, y = yresid)) + stat_binmean(n=40) +
    theme_bw() +
    xlab('Residualized instrument value') +
    ylab(str_interp('Residualized ${outcome}')) +
    theme(axis.text = element_text(size=10),
          axis.title.y = element_text(size=12),
          axis.title.x = element_text(size=12),
          text = element_text(family='LM Roman 10'))
  ggsave(str_interp('output/figures/V1-residuals-${outcome}.png'), height=3, width=6)
}

