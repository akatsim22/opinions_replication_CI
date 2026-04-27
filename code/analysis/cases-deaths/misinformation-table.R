##########
# CREATE MISINFORMATION TABLE
##########


source('code/analysis/load.R')


process = function(o, position, lab) {
  before1 = paste0('\\\\[-2.1ex] ', lab, paste(rep('&', position), collapse=''))
  before2 = paste0(paste(rep('&', position), collapse=''))
  after = paste0(paste(rep('&', 5-position), collapse=''), '\\\\')
  o = o[14:17]
  o[1] = str_replace_all(str_split_fixed(o[1], '&', 2)[,2], '\\\\', '')
  o[2] = str_replace_all(str_split_fixed(o[2], '&', 2)[,2], '\\\\', '')
  o[1] = paste0(before1, o[1], after)
  o[2] = paste0(before2, o[2], after)
  return(o)
}


notes = str_interp(tablenotes[['t:misinformation']])

# Panel A: OLS shifter
m1 = felm(as.formula(str_interp('${end_misinfo}~${end}+${base_ols}|state_fips|0|geography')), 
          data=data %>% filter(elapdate == '2020-03-28'))
m2 = felm(as.formula(str_interp('${end_misinfo}~${end}+${base_ols}+${fullcontr}|state_fips|0|geography')), 
          data=data %>% filter(elapdate == '2020-03-28'))
o1 = process(stargazer(list(m1, m2), keep=end), 1, 'H-C viewership difference')
      
# Panel B: RF shifter
m3 = felm(as.formula(str_interp('${end_misinfo}~${iv}+${base_iv}|state_fips|0|geography')), 
          data=data %>% filter(elapdate=='2020-03-28')) 
m4 = felm(as.formula(str_interp('${end_misinfo}~${iv}+${base_iv}+${fullcontr}|state_fips|0|geography')), 
          data=data %>% filter(elapdate=='2020-03-28')) 
o2 = process(stargazer(list(m3, m4), keep=iv), 3, 'Non-Fox TVs on $\\times$ Fox share') 
    
# Panel C: 2SLS: cases and deaths on misinfo index, instrumented
m5 = felm(as.formula(str_interp('lcases~${base_iv}+${fullcontr}|state_fips|(${end_misinfo}~${iv})|geography')), 
          data=data %>% filter(elapdate=='2020-03-14')) 
m6 = felm(as.formula(str_interp('ldeaths~${base_iv}+${fullcontr}|state_fips|(${end_misinfo}~${iv})|geography')), 
          data=data %>% filter(elapdate=='2020-03-28'))  
o3 = process(stargazer(list(m5, m6), keep=end_misinfo), 5, '$-1 \\times$ coverage index (predicted)')   

nobs_m1 = format(nobs(m1), big.mark=",")
nobs_m2 = format(nobs(m2), big.mark=",")
nobs_m3 = format(nobs(m3), big.mark=",")
nobs_m4 = format(nobs(m4), big.mark=",")
nobs_m5 = format(nobs(m5), big.mark=",")
nobs_m6 = format(nobs(m6), big.mark=",")
 


out = c('\\begin{table}[!htbp] \\centering',
        '\\caption{Differential coverage and COVID-19 outcomes across all Fox News evening shows}',
        str_interp('\\label{t:misinformation-V1} '),
        '\\begin{threeparttable}',
        '\\begin{tabular}{@{\\hspace{5pt}}l@{\\hspace{5pt}}ccccccc} ',
        '\\toprule ',
        '& \\multicolumn{6}{c}{\\textit{Dependent variable:}} \\\\ ',
        '\\cmidrule(rr){2-7}',
        '& & & & & Cases & Deaths \\\\ ',
        '& \\multicolumn{4}{c}{Inverse pandemic coverage index} & Mar 14 & Mar 28 \\\\ ',
        '\\cmidrule(rr){2-5} \\cmidrule(rr){6-7} ',
        '& (1) & (2) & (3) & (4) & (5) & (6) \\\\',
        '\\midrule \\midrule',
        '\\multicolumn{7}{l}{\\textbf{Panel A}: \\textit{OLS: inverse pandemic coverage index on relative viewership}} \\\\',
        '\\midrule',
        o1,
        '\\multicolumn{7}{l}{\\textbf{Panel B}: \\textit{RF: inverse pandemic coverage index on instrument}} \\\\',
        '\\midrule',
        o2,
        '\\multicolumn{7}{l}{\\textbf{Panel C}: \\textit{2SLS: cases and deaths on inverse predicted pandemic coverage index}} \\\\',
        '\\midrule  ',
        o3,
        'Controls & Base & Full & Base & Full & Full & Full \\\\ ',
        'State FEs & Yes & Yes & Yes & Yes & Yes & Yes  \\\\ ',
        str_interp('Observations & ${nobs_m1} & ${nobs_m2} & ${nobs_m3} & ${nobs_m4} & ${nobs_m5} & ${nobs_m6}  \\\\ '),
        '\\bottomrule ',
        '\\end{tabular} ',
        '\\begin{tablenotes}',
        '\\footnotesize',
        str_interp('\\item \\textit{Notes:} ${notes} '),
        '\\end{tablenotes}',
        '\\end{threeparttable}',
        '\\end{table}' )
writeLines(out, con = str_interp('output/tables/V1-misinformation.tex'))
