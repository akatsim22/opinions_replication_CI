library(tidyverse)
library(haven)
library(lfe)
library(stargazer)
library(lubridate)
library(cowplot)
library(broom)
library(extrafont)

if (!requireNamespace("starpolishr", quietly = TRUE)) {
  message("Package 'starpolishr' not available; using built-in star_notes_tex fallback.")
  star_notes_tex <- function(x, note.type = "threeparttable", note = "") {
    if (is.null(note) || identical(note, "")) {
      return(x)
    }
    if (!identical(note.type, "threeparttable")) {
      return(c(x, paste0("% Notes: ", note)))
    }
    end_table_idx = which(grepl("\\\\end\\\\{table\\\\}", x))[1]
    if (is.na(end_table_idx)) {
      return(c(x, "\\\\begin{tablenotes} \\small", paste0("\\\\item \\textit{Notes:} ", note), "\\\\end{tablenotes}"))
    }
    c(x[1:(end_table_idx - 1)],
      "\\begin{tablenotes} \\small",
      paste0("\\item \\textit{Notes:} ", note),
      "\\end{tablenotes}",
      x[end_table_idx:length(x)])
  }
} else {
  star_notes_tex <- starpolishr::star_notes_tex
}

recode = dplyr::recode
select = dplyr::select

tablenotes = rjson::fromJSON(file='code/analysis/tablenotes.json')

data = read_dta('data/working/TuckerCarlsonIV_County_PANEL.dta') 

end = 'zdiff_viewersHvT'
end_misinfo = 'total_misinfo'
hannity_linear = 'hannity_linear_control'
poprace = 'pop_white_perc+pop_black_perc+pop_hispanic_perc'
age = 'pop_65_plus_perc'
educ = 'edushare_male_noHS+edushare_female_noHS+edushare_male_noCOLLEGE+edushare_female_noCOLLEGE'
econ = 'perc_poor2018+lmed_hh_inc2018+urate_bls'
health = 'poor_physical_days_raw+uninsured_raw'
healthcap = 'beds+nurses+personnel'
integration = 'highway_km + airport + largecity_distance'
politics = 'repshare2016+ltotvotes2016'
fullcontr = str_interp('${poprace}+${age}+${educ}+${econ}+${health}+${healthcap}+${politics}')

base_ols = 'hutput_hannity + hutput_tucker + hutput_ingraham + fox_shr_of_cable + fox_shr_Jan2020 + lpop_2019 + pop_density_2019 + msnbc_shr_of_cable + popweighted_lat + popweighted_lon'

iv = str_interp('IV_V1_hannity')
base_iv = 'pr_hutput_V1_hannity + pr_hutput_V1_tucker + pr_hutput_V1_ingraham + fox_shr_of_cable + fox_shr_Jan2020 + lpop_2019 + pop_density_2019 + msnbc_shr_of_cable + popweighted_lat + popweighted_lon'

create_base_iv = function(version) {
  str_interp('pr_hutput_${version}_hannity + pr_hutput_${version}_tucker + pr_hutput_${version}_ingraham + fox_shr_of_cable + fox_shr_Jan2020 + lpop_2019 + pop_density_2019 + msnbc_shr_of_cable + popweighted_lat + popweighted_lon')
}


make_coef_df = function(outcome, type, misinfo=FALSE, dataarg, version, begin=NA, cluster='geography', iv=NA, add_controls='', ci=1.96) {
  print('Estimating DF...')
  if (is.na(iv)) {
    iv = str_interp('IV_${version}_hannity')
  } 
  ivs = str_interp('(${end}~${iv})')
  ivs_misinfo = str_interp('(${end_misinfo}~${iv})')
  base_iv = create_base_iv(version=version)
  if (type == 'OLS') {
    spec = ifelse(misinfo,str_interp('${end_misinfo}+${base_ols}+${fullcontr}${add_controls}|state_fips|0|${cluster}'),
                  str_interp('${end}+${base_ols}+${fullcontr}${add_controls}|state_fips|0|${cluster}'))
    term = ifelse(misinfo, end_misinfo, end)
  } else if (type == '2SLS') {
    spec = ifelse(misinfo, str_interp('${base_iv}+${fullcontr}${add_controls}|state_fips|${ivs_misinfo}|${cluster}'),
                  str_interp('${base_iv}+${fullcontr}${add_controls}|state_fips|${ivs}|${cluster}'))
    term = ifelse(misinfo, str_interp('`${end_misinfo}(fit)`'), str_interp('`${end}(fit)`'))
  } else if (type == 'RF') {
    spec = str_interp('${base_iv}+${iv}+${fullcontr}${add_controls}|state_fips|0|${cluster}')
    term = iv
  }
  
  if (str_detect(outcome, 'cases') & is.na(begin)) {
    begin = '2020-02-24'
  } else if (str_detect(outcome, 'deaths') & is.na(begin)) {
    begin = '2020-03-01'
  } else if (str_detect(outcome, 'stay') & is.na(begin)) {
    begin = '2020-02-05'
  } else if (str_detect(outcome, 'outcome') & is.na(begin)) {
    begin = '2020-02-05'
  }
  estimate_model = function(x) {
    model = tryCatch({
      x = x %>% drop_na((str_interp('l${outcome}')), zdiff_viewersHvT, poor_physical_days_raw)
      model1 = felm(as.formula(str_interp('l${outcome}~${spec}')), data=x, weights=x$weights)
      tidy(model1) %>% 
        filter(str_detect(term, str_interp('IV|${end}|${end_misinfo}'))) %>% 
        rename(coef=estimate, se=std.error) %>% 
        dplyr::select(term, coef, se)
      
    }, error = function(err) {
        data.frame('term'=NA, 'coef'=NA, 'se'=NA)
      })
    
  }

  df = dataarg %>%
    filter(elapdate>=begin) %>% 
    group_by(elapdate) %>% 
    nest() %>%
    mutate(model = map(data, estimate_model)) %>%
    select(-data) %>%
    ungroup() %>%
    unnest(cols = c(model, elapdate)) %>%
    mutate(error_high = coef+ci*se,
           error_low = coef-ci*se)
  
  return(df)
}