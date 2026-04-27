##########
# CREATE EXOGENEITY FIGURES
##########


source('code/analysis/load.R')

data = data %>% filter(elapdate=='2020-03-28')

m1 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}|0|0|geography')), data=data)
m2 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}+${fullcontr}|0|0|geography')), data=data)
m3 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}|divisionfp|0|geography')), data=data)
m4 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}+${fullcontr}|divisionfp|0|geography')), data=data)
m5 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}|state_fips|0|geography')), data=data)
m6 = felm(as.formula(str_interp('zdiff_viewersHvT~${iv}+${base_iv}+${fullcontr}|state_fips|0|geography')), data=data)
  
models = list(m1, m2, m3, m4, m5, m6)
extract = function(x) {
  tidied = tidy(x)
  fstat = tidied[tidied$term==str_interp('IV_V1_hannity'), 'statistic']^2 %>% as.numeric %>% round(2) %>% sprintf("%.2f", .)
  fstat
}

fstats = c('$F$-statistic', map_chr(models[1:6], extract)) %>% paste(collapse=' & ') %>% paste0('\\\\') # last one from stata

out = stargazer(models,
                keep='IV_V',
                covariate.labels = ifelse(version=='V1', 'Non-Fox TVs on $\\times$ Fox share', 'Predicted non-Fox TVs on $\\times$ Fox share'),
                keep.stat = c('rsq', 'n'), 
                dep.var.labels = c(''),
                label=str_interp('t:first-stage-V1-2'),
                title = str_interp('First-stage regressions'),
                add.lines = list(c('Controls', rep(c('Base','Full'), 3)),
                                 c('Fixed effects', rep('None',2), rep('Division', 2), rep('State', 2)))) %>%
  star_notes_tex(note.type = 'threeparttable', note = str_interp(tablenotes[['t:first-stage-2']]))

out[12] = '& \\multicolumn{6}{c}{Difference in Hannity-Carlson viewership}\\\\'
out[13] = "\\cmidrule(lr){2-7}"  
out = c(out[1:22], fstats, out[23:length(out)])

writeLines(out, con = str_interp('output/tables/V1-first-stage.tex'))
