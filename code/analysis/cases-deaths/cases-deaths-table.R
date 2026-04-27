##########
# CREATE IV AND OLS CASES AND DEATHS TABLE
##########


source('code/analysis/load.R')


make_ols_iv_combined = function(dates, version) {
 
  ivs = str_interp('(${end}~${iv})')
  
  # ols_spec = str_interp('${end}+${base_ols}+${fullcontr}|state_fips|0|geography')
  # iv_spec = str_interp('${base_iv}+${fullcontr}|state_fips|${ivs}|geography')
  
  outputs = list()
  for (outcome in c('cases','deaths')) {
    ols_models = map(dates, function(x) felm(as.formula(str_interp('l${outcome}~${end}+${base_ols}+${fullcontr}|state_fips|0|geography')), data=data %>% filter(elapdate==x)))
    iv_models  = map(dates, function(x) felm(as.formula(str_interp('l${outcome}~${base_iv}+${fullcontr}|state_fips|${ivs}|geography')),  data=data %>% filter(elapdate==x)))
    out_ols = stargazer(ols_models,
                        keep='zdiff',
                        covariate.labels = c('Hannity-Carlson viewership difference'),
                        keep.stat = c('n'), 
                        dep.var.labels = str_interp('COVID-19 outcomes'),
                        column.labels = map_chr(dates, function(x) format(x, "%b %d")),
                        label=str_interp('t:OLS'),
                        title = str_interp("Effect of differential viewership on COVID-19 outcomes"),
                        add.lines = list(c(str_interp('Full controls'), rep('Yes', length(dates))),
                                         c('State FEs', rep('Yes', length(dates))))) %>%
      star_notes_tex(note.type='threeparttable',note='')
    
    out_iv = stargazer(iv_models,
                       keep='fit',
                       covariate.labels = c('H-C viewership difference (predicted)'),
                       keep.stat = c('n'), 
                       dep.var.labels = str_interp('COVID-19 outcomes'),
                       column.labels = map_chr(dates, function(x) format(x, "%b %d")),
                       add.lines = list(c(str_interp('Full controls'), rep('Yes', length(dates))),
                                        c('State FEs', rep('Yes', length(dates))))) %>%
      star_notes_tex(note.type = 'threeparttable', note = str_interp(tablenotes[['t:ols-iv']]))
    
    outputs[[outcome]] = list(out_ols, out_iv)
  }
  
  out = c(outputs[[1]][[1]][1:16],
          '\\multicolumn{6}{l}{\\textbf{Panel A}: Estimates on cases} \\\\',
          '\\midrule',
          '\\multicolumn{6}{l}{\\emph{Subpanel A.1: OLS}} \\\\',
          '\\midrule',
          outputs[[1]][[1]][17:20],
          '\\multicolumn{6}{l}{\\emph{Subpanel A.2: Two-stage least squares}} \\\\',
          '\\midrule',
          outputs[[1]][[2]][17:20],
          '\\multicolumn{6}{l}{\\textbf{Panel B}: Estimates on deaths} \\\\',
          '\\midrule',
          '\\multicolumn{6}{l}{\\emph{Subpanel B.1: OLS}} \\\\',
          '\\midrule',
          outputs[[2]][[1]][17:20],
          '\\multicolumn{6}{l}{\\emph{Subpanel B.2: Two-stage least squares}} \\\\',
          '\\midrule',
          outputs[[2]][[2]][17:length(outputs[[2]][[2]])])
  
  writeLines(out, con = str_interp('output/tables/OLS-V1-cases-deaths.tex'))
}

make_ols_iv_combined(dates=seq(ymd('2020-02-29'),ymd('2020-04-15'), by = '1 week'), version="V1")

