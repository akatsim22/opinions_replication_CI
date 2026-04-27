##########
# CREATE COUNT MODEL TABLE
##########

source('code/analysis/load.R')
library(fixest)
library(pscl)


poisson_regression = function(str, data, ...) {
  formula = as.formula(str_interp(str))
  
  # Run fepois
  fepois_result = fepois(formula, data, se='standard')
  
  # Create some lm object
  dep.var = str_split(formula, " ~ ")[[2]]
  regressors = str_split(str_replace(str_split(str_split(formula, "~")[[3]], " \\|")[[1]][1], "\n    ", ""), " \\+ ")[[1]]
  obs = nobs(fepois_result)
  d <- as.data.frame(matrix(rnorm(obs * length(regressors)+1), nrow=obs, nc=length(regressors)+1))
  names(d) <- c(dep.var, regressors)
  f <- as.formula(paste(dep.var, "~ 0 +", paste(regressors, collapse = "+")))
  p <- lm(f, d)
  
  # Copy coefs and se from fepois into lm object (because stargazer cannot handle fixest object)
  p$coefficients = fepois_result$coefficients
  p$se = fepois_result$se
  
  return(p)
}


zinb_regression = function(str, data, ...) {
  formula = as.formula(str_interp(str))
  return(zeroinfl(formula, data, dist = "negbin", EM = F))
}


deaths = data %>% filter(elapdate == '2020-03-28') 
cases =  data %>% filter(elapdate == ymd('2020-03-28')-14) 
datasets = list('deaths'=deaths, 'cases'=cases)


create_panel = function(outcome) {
  models = list()
  models[[1]] = poisson_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}+${fullcontr}|0'), datasets[[outcome]], cluster='geography')
  models[[2]] = poisson_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}+${fullcontr}+divisionfp'), datasets[[outcome]], cluster='geography')
  models[[3]] = poisson_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}+${fullcontr}+state_fips'), datasets[[outcome]], cluster='geography')
  
  models[[4]] <- zinb_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}|IV_V1_hannity+${base_iv}'), data = datasets[[outcome]], cluster='geography')
  models[[5]] <- zinb_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}+divisionfp|IV_V1_hannity+${base_iv}+divisionfp'), data = datasets[[outcome]], cluster='geography')
  models[[6]] <- zinb_regression(str_interp('${outcome}~IV_V1_hannity+${base_iv}+state_fips|IV_V1_hannity+${base_iv}+state_fips'), data = datasets[[outcome]], cluster='geography')
  
  # Get correct SE for Poisson models ->input in stargazer
  se_list = list(models[[1]]$se, models[[2]]$se, models[[3]]$se)
  
  out = stargazer(models,
                  keep='IV_V1_hannity',
                  se=se_list,
                  covariate.labels = c('Non-Fox TVs on $\\times$ Fox share'),
                  keep.stat = c('n'), 
                  column.labels = c('Poisson','Zero-inflated NB'),
                  column.separate = c(3,3),
                  title = 'Effects of differential viewership on COVID-19 outcomes (count models)',
                  label = str_interp('t:count-models'),
                  add.lines = list(c('Fixed effects', rep(c('None','Division','State'), 2)))) %>%
    star_notes_tex(note.type = 'threeparttable', note = str_interp(tablenotes[['t:count-models']]))
  
  return (out)
}

cases = create_panel('cases')
deaths = create_panel('deaths')

out = c(
  cases[1:9],
  cases[16],
  " \\cmidrule(rr){2-4} \\cmidrule(rr){5-7}",
  cases[17],
  '\\midrule',
  "\\multicolumn{7}{l}{\\textbf{Panel A}: Estimates on cases} \\\\",
  '\\midrule',
  cases[19:21],
  '\\midrule',
  "\\multicolumn{7}{l}{\\textbf{Panel B}: Estimates on deaths} \\\\",
  '\\midrule',
  deaths[19:29]
)

write_lines(out, 'output/tables/V1-count-models.tex')
