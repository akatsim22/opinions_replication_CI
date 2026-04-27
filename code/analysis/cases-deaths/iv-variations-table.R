##########
# CREATE TABLE WITH IV VARIATIONS
##########

source('code/analysis/load.R')

fstats = read_lines('data/working/firststageFstat.tex')[7] %>% paste0('$F$-statistic (Kleibergen-Paap)', .)

deaths = data %>% filter(elapdate == '2020-03-28')
cases =  data %>% filter(elapdate == '2020-03-14')
datasets = list('deaths'=deaths, 'cases'=cases)


create_table = function(outcome) {
  
  data = datasets[[outcome]]
  
  base_iv_leaveout = create_base_iv('V1')
  base_iv_sunset = create_base_iv('V2')
  base_iv_division = create_base_iv('V3')

  model1 = felm(as.formula(str_interp('l${outcome}~${base_iv_leaveout}+${fullcontr}|state_fips|(${end}~IV_V1_hannity)|geography')),data=data)
  model2 = felm(as.formula(str_interp('l${outcome}~${base_iv_leaveout}+${fullcontr}|state_fips|(${end}~IV_V1_hannity+IV_V1_tucker)|geography')),data=data)
  model3 = felm(as.formula(str_interp('l${outcome}~${base_iv_sunset}+${fullcontr}|state_fips|(${end}~IV_V2_hannity)|geography')),data=data)
  model4 = felm(as.formula(str_interp('l${outcome}~${base_iv_sunset}+${fullcontr}|state_fips|(${end}~IV_V2_hannity+IV_V2_tucker)|geography')),data=data)
  model5 = felm(as.formula(str_interp('l${outcome}~${base_iv_division}+${fullcontr}|state_fips|(${end}~IV_V3_hannity)|geography')),data=data)
  model6 = felm(as.formula(str_interp('l${outcome}~${base_iv_division}+${fullcontr}|state_fips|(${end}~IV_V3_hannity+IV_V3_tucker)|geography')),data=data)
  
  transformation = 'the log of one plus'
  out_iv = stargazer(list(model1, model2, model3, model4, model5, model6),
                     title = str_interp('2SLS estimates: robustness to choice of controls and instrument variations'),
                     keep='fit',
                     covariate.labels = c('H-C viewership difference (predicted)'),
                     keep.stat = c('N'), 
                     dep.var.labels = str_interp('COVID-19 outcomes'),
                     label = str_interp('t:2sls-robustness'),
                     add.lines = list(c(str_interp('Controls'), rep('Full', 6)),
                                      c('Instruments', rep(c('H', 'H\\&T'), 3)),
                                      c('State FEs', rep('Yes', 6)))) %>%
    star_notes_tex(note.type = 'threeparttable', note = str_interp(tablenotes[['t:2sls-robustness']])) 

  out_iv[12] = str_interp('& \\multicolumn{6}{c}{COVID-19 outcomes}\\\\')
  return(out_iv)
}

cases = create_table('cases')
deaths = create_table('deaths')

all = c(cases[4:14],
        '\\midrule',
        '\\multicolumn{6}{l}{\\textbf{Panel A}: \\textit{COVID-19 cases on March 14}} \\\\',
        '\\midrule',
        cases[15:17],
        '\\midrule',
        '\\multicolumn{6}{l}{\\textbf{Panel B}: \\textit{COVID-19 deaths on March 28}} \\\\',
        '\\midrule',
        deaths[15:18],
        '\\midrule',
        fstats,
        '\\addlinespace',
        deaths[19:20],
        'Instrument & Leave-out & Leave-out & Sunset & Sunset & Division sunset & Division sunset \\\\',
        deaths[21:27])

write_lines(all, 'output/tables/2SLS-robustness.tex')