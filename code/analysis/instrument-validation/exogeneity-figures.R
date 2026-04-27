##########
# CREATE FIRST STAGE TABLE
##########


source('code/analysis/load.R')

data = data %>% filter(elapdate=='2020-03-28')

main_controls = str_split(fullcontr, '\\+')[[1]] %>%
  str_remove_all(' ')


# Exogeneity figures for V1, V2, V3
recoding_dict = c(
  'pop_white_perc' = '% white',
  'pop_hispanic_perc' = '% Hispanic',
  'pop_black_perc' = '% black',
  'CensusPercentRural'='% rural',
  'pop_65_plus_perc' = '% over 65',
  'lmed_hh_inc2018' = 'Log median HHI',
  'urate_bls' = 'Unemployment rate',
  'perc_poor2018' = '% below FPL',
  'uninsured_raw' = '% uninsured',
  'edushare_male_noHS' = '% men w/o HS degree',
  'edushare_male_noCOLLEGE' = '% men w/o college degree',
  'edushare_female_noHS' = '% women w/o HS degree',
  'edushare_female_noCOLLEGE' = '% women w/o college degree',
  'poor_physical_days_raw'='Physical health',
  'beds' = 'Acute care beds per capita',
  'nurses' = 'Nurses per capita',
  'personnel' = 'Hospital personnel per capita',
  'msnbc_shr_of_cable' = 'MSNBC cable share',
  'cnn_shr_of_cable' = 'CNN cable share',
  'fox_shr_of_cable' = 'Fox News cable share',
  'repshare2016' = '2016 Rep vote share'
  )


extract = function(x, type='iv') {
  cluster = ifelse(x %in% c('beds','nurses','personnel'), 'hrrnum', 'geography')
  if (type == 'iv') {
    form = as.formula(str_interp('scale(${x})~${iv}+${base_iv}|state_fips|0|${cluster}'))
    term = iv
  } else {
    form = as.formula(str_interp('scale(${x})~${end}+${base_ols}|state_fips|0|${cluster}'))
    term = end
  }
  model = felm(form, data=data) %>% tidy
  coef = model[model$term==term, 'estimate'] %>% as.numeric
  se = model[model$term==term, 'std.error'] %>% as.numeric
  print(c(coef,se))
  return(c(coef, se))
}

for (version in c('V1', 'V2', 'V3')) {
  iv = str_interp('IV_${version}_hannity')
  base_iv = create_base_iv(version)
  contr = data.frame(controls = main_controls) %>% 
    filter(!str_detect(controls, 'popweighted')) %>% 
    mutate(models = map(controls, function(x) extract(x)),
           coef = map_dbl(models, 1),
           se = map_dbl(models, 2),
           controls = recode(controls, !!!recoding_dict),
           controls = factor(controls, levels=rev(recoding_dict)))
  
  plot1 = ggplot(contr, aes(x = controls, y = coef)) + 
    geom_point(size=3, alpha=0.8) +
    geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se)) +
    geom_hline(yintercept=0, linetype='dotted') +
    coord_flip(ylim = c(-2, 2)) +
    theme_bw() +
    ylab('Coefficient estimate') +
    theme(axis.text = element_text(size=11),
          axis.title.x = element_text(size=12),
          axis.title.y = element_blank(),
          text = element_text(family='LM Roman 10'))
  
  
  ggsave(str_interp('output/figures/${version}-exogeneity.png'), plot1, height=6.5, width=8.5)
}


# Exogeneity figure for OLS
contr = data.frame(controls = main_controls) %>% 
  filter(!str_detect(controls, 'popweighted')) %>% 
  mutate(models = map(controls, function(x) extract(x, type='ols')),
         coef = map_dbl(models, 1),
         se = map_dbl(models, 2),
         controls = recode(controls, !!!recoding_dict),
         controls = factor(controls, levels=rev(recoding_dict)))

plot1 = ggplot(contr, aes(x = controls, y = coef)) + 
  geom_point(size=3, alpha=0.8) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se)) +
  geom_hline(yintercept=0, linetype='dotted') +
  coord_flip(ylim = c(-0.5, 0.5)) +
  theme_bw() +
  ylab('Coefficient estimate') +
  theme(axis.text = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        text = element_text(family='LM Roman 10'))

ggsave(str_interp('output/figures/OLS-exogeneity.png'), plot1, height=6.5, width=8.5)



# Similarity figure
recoding_dict = c(
  'abortion_always' = 'Abortion always allowed',
  'enviro_airwateracts' = 'Strengthen EPA enforcement',
  'guns_assaultban' = 'Ban assault rifles',
  'immig_border' = 'Increase border security',
  'military_democracy' = 'Military promote democracy abroad',
  'gaymarriage' = 'Oppose gay marriage',
  'repealaca' = 'Repeal the ACA',
  'climatechange_human' = 'Belief in anthropogenic climate change',
  'amend_gender_discrim_laws' = 'Amend laws to prohibit gender discrimination',
  'white_have_advantages' = 'White people have advantages',
  'racial_problems_rare' = 'Racial problems in the U.S. are rare',
  'reduce_immigration' = 'Reduce legal immigration by 50 percent',
  'legalize_immigrants_3yr' = 'Legal status for illegal immigrants after 3 years',
  'legalize_immigrants_children' = 'Permanent resident status for children of immigrants'
)

contr = data.frame(controls = names(recoding_dict)) %>%
  mutate(models = map(controls, function(x) extract(x)),
       coef = map_dbl(models, 1),
       se = map_dbl(models, 2),
       controls = recode(controls, !!!recoding_dict),
       controls = factor(controls, levels=rev(recoding_dict)))
  
plot1 = ggplot(contr, aes(x = controls, y = coef)) + 
  geom_point(size=3, alpha=0.8) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se)) +
  geom_hline(yintercept=0, linetype='dotted') +
  coord_flip(ylim = c(-2, 2)) +
  theme_bw() +
  ylab('Coefficient estimate') +
  theme(axis.text = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        text = element_text(family='LM Roman 10'))

ggsave(str_interp('output/figures/V1-similarity.png'), plot1, height=6.5, width=8.5)

