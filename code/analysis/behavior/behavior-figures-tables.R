##########
# CREATE BEHAVIOR FIGURES AND TABLES
##########

library(tidyverse)
library(stargazer)
library(lfe)
library(starpolishr)
library(haven)
library(lubridate)
library(multcomp)
library(broom)
library(starbility)
library(extrafont)

tablenotes = rjson::fromJSON(file='code/analysis/tablenotes.json')

survey = read_dta('data/working/behavior-survey.dta') %>% 
  filter(foxviewer==1) %>%
  mutate(watch_other = watch_ingraham | watch_thefive | watch_story | watch_otherfoxshow) %>%
  mutate(stations_4_text = tolower(stations_4_text),
         broadcast = str_detect(stations_4_text, 'nbc|cbs|abc|bbc|local|broadcast'))

controls = 'age + male + retired + fulltime + hhi + whitenohisp + educ'
watch_other_shows = 'cnnviewer+msnbcviewer'

get_hannity_carlson_p_value = function(model) {
  ftest = summary(glht(model, 'watch_hannity-watch_carlson=0'))
  p = ftest$test$pvalues[1]
  if (p<0.001) {
    return('$<0.001$')
  } else{
    p = round(p, 3)
    return(sprintf("%.3f", p))
  }
}

# Behavior table
intensive_margin = function(survey) {
  survey$change_date[survey$change_by_survey_date==0] = max(survey$change_date, na.rm = T)
  
  m1 = felm(as.formula(str_interp('change_date~watch_hannity+watch_carlson+watch_other+${watch_other_shows}+${controls}')), data=survey)
  m2 = felm(as.formula(str_interp('change_before_mar1~watch_hannity+watch_carlson+watch_other+${controls}+${watch_other_shows}')), data=survey)
  m3 = felm(as.formula(str_interp('change_before_mar15~watch_hannity+watch_carlson+watch_other+${controls}+${watch_other_shows}')), data=survey)
  m4 = felm(as.formula(str_interp('change_before_apr1~watch_hannity+watch_carlson+watch_other+${controls}+${watch_other_shows}')), data=survey)
  
  models = list(m1, m2, m3, m4)
  means = c(mean(survey$change_date, na.rm=T), 
            mean(survey$change_before_mar1, na.rm=T), 
            mean(survey$change_before_mar15, na.rm=T), 
            mean(survey$change_before_apr1, na.rm=T)) %>% round(3) %>% sprintf("%.3f",.) %>%
  paste(collapse='&')
  
  out = stargazer(models,
                  omit='age|male|retired|fulltime|hhi|white|educ|watch_ingraham|watch_thefive|watch_story|cnn|msnbc|Constant|watch_other',
                  covariate.labels = c('Watches Hannity', 'Watches Carlson'),
                  keep.stat = c('rsq', 'n'), 
                  title = str_interp("Correlation between show viewership and timing of behavior change"), label = str_interp('t:survey'),
                  add.lines = list(c('Demographic controls', rep(c('Yes'), 4)),
                                   c('Other viewership controls', rep(c('Yes'), 4))))
  
  out = c(out[4:10], 
          ' & --- & \\multicolumn{3}{c}{Changed before...} \\\\',
          '\\cmidrule(rr){2-2} \\cmidrule(rr){3-5}',
          '& Change day & March 1 & March 15 & April 1 \\\\', 
          '\\cmidrule(rr){2-5}', 
          out[12:20],
          paste0('p-value (Hannity=Carlson) &', paste(map_chr(models, get_hannity_carlson_p_value), collapse=' & '), '\\\\'),
          '\\addlinespace',
          paste0('DV mean &', means, '\\\\'),
          '\\addlinespace',
          out[24:length(out)])
  
  out = star_notes_tex(out, note.type = 'threeparttable', note = tablenotes[['t:survey']]) 
  writeLines(out, 'output/tables/behavior.tex')
}

intensive_margin(survey)


# Density figure
make_density_figure = function(survey) {
  survey$change_date[survey$change_by_survey_date==0] = max(survey$change_date, na.rm = T)
  
  carlsonhannity = survey %>% 
    filter(watch_carlson==1 & watch_hannity!=1) %>%
    mutate(show='Carlson') %>%
    bind_rows(survey %>% filter(watch_hannity==1 & watch_carlson!=1) %>% mutate(show='Hannity')) %>%
    bind_rows(survey %>% filter(watch_other==1) %>% mutate(show='Other')) %>%
    mutate(date = ymd('2020-01-31')+days(change_date)) %>%
    mutate(show=factor(show, levels=c('Carlson','Other','Hannity')))
    
    plot1 = ggplot(carlsonhannity, aes(x = date, fill=show)) + 
      geom_density(alpha=0.8, col=NA) +
      labs(fill='Show viewership') +
      xlab('Date of behavior change') +
      ylab('Density') +
      facet_wrap(~show, dir='v') +
      theme_bw() +
      guides(fill=FALSE) +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(size=12),
            axis.title.y = element_text(size=16),
            strip.text.x = element_text(size=12),
            axis.title.x = element_blank(),
            text = element_text(family='LM Roman 10')) +
      geom_vline(data=filter(carlsonhannity, show=="Carlson"), aes(xintercept=mean(carlsonhannity$date[carlsonhannity$show=='Carlson'])), linetype='dashed') +
      geom_vline(data=filter(carlsonhannity, show=="Other"), aes(xintercept=mean(carlsonhannity$date[carlsonhannity$show=='Other'])), linetype='dashed') +
      geom_vline(data=filter(carlsonhannity, show=="Hannity"), aes(xintercept=mean(carlsonhannity$date[carlsonhannity$show=='Hannity'])), linetype='dashed') +
      scale_fill_manual(values=c('#39568CFF', '#404040','#440154FF'))
    ggsave('output/figures/behavior-density.png', plot1, width=6, height=4, units='in')
    
    
}
make_density_figure(survey)


# Coef plot
make_daily_figure = function(survey) {
  df = tibble(elapdate=seq(ymd('2020-02-01'), ymd('2020-04-03'), by = '1 day'))
  
  estimate_model = function(x) {
    survey$outcome = (I(survey$change_date<=as.numeric(x-ymd('2020-02-01'))) & survey$change_by_survey_date==1)
    model = lm(as.formula(str_interp('outcome ~ watch_hannity+watch_carlson+watch_other+${controls}+${watch_other_shows}')), data=survey) %>% tidy
    hannity_coef = model[model$term=='watch_hannity', 'estimate'] %>% as.numeric()
    hannity_se = model[model$term=='watch_hannity', 'std.error'] %>% as.numeric()
    carlson_coef = model[model$term=='watch_carlson', 'estimate'] %>% as.numeric()
    carlson_se = model[model$term=='watch_carlson', 'std.error'] %>% as.numeric()
    
    return(c(hannity_coef, hannity_se, carlson_coef, carlson_se))
  }
  
  df = df %>% mutate(model = map(elapdate, estimate_model),
                     hannity_coef = map_dbl(model, 1),
                     hannity_se = map_dbl(model, 2),
                     hannity_error_high = hannity_coef+1.96*hannity_se,
                     hannity_error_low = hannity_coef-1.96*hannity_se,
                     carlson_coef = map_dbl(model, 3),
                     carlson_se = map_dbl(model, 4),
                     carlson_error_high = carlson_coef+1.96*carlson_se,
                     carlson_error_low = carlson_coef-1.96*carlson_se) %>%
    dplyr::select(-model)
  
  
  hannity = df %>% dplyr::select(elapdate:hannity_error_low) %>% mutate(type = 'Watch Hannity')
  names(hannity) = c('elapdate','coef','se','error_high','error_low', 'type')
  carlson = df %>% dplyr::select(elapdate, carlson_coef:carlson_error_low) %>% mutate(type = 'Watch Carlson')
  names(carlson) = c('elapdate','coef','se','error_high','error_low', 'type')
  df_to_plot = bind_rows(hannity, carlson) %>%
    mutate(type = factor(type, levels=c('Watch Carlson','Watch Hannity')))
  
  plot = ggplot(df_to_plot, aes(x = elapdate, y = coef)) + geom_point(aes(col=type, shape=type))  +
    geom_ribbon(aes(ymin=error_low, ymax=error_high, fill=type), alpha=0.2) +
    scale_shape_manual(values=c(15, 16)) +
    scale_fill_manual(values=c('#39568CFF', '#440154FF')) +
    scale_color_manual(values=c('#39568CFF', '#440154FF')) +
    labs(fill='Show', col='Show', shape='Show') +
    theme_bw() +
    ylab('Coefficient estimate') +
    theme(legend.position = 'bottom',
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          axis.text.x = element_text(size=12),
          axis.title.y = element_text(size=16),
          strip.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          text = element_text(family='LM Roman 10'))
  
  ggsave('output/figures/behavior-timeseries.png', plot=plot, height=3.25, width=6, units='in')
}

make_daily_figure(survey)


# Viewership characteristics table
make_demographic_table = function(survey) {
  exc = survey %>% 
    filter(watch_carlson_no_hannity | watch_hannity_no_carlson) %>% 
    mutate(show = ifelse(watch_carlson_no_hannity, 'Tucker Carlson Tonight', 'Hannity'),
           hhi = case_when(
             str_detect(hhi, '100,') ~ 125000,
             str_detect(hhi, '15,') ~ 20000,
             str_detect(hhi, '150,') ~ 175000,
             str_detect(hhi, '25,') ~ 37500,
             str_detect(hhi, '74,') ~ 62500,
             str_detect(hhi, '99') ~ 87500,
             str_detect(hhi, 'Less') ~ 10000,
             str_detect(hhi, 'More') ~ 250000
           ),
           educ = case_when(
             str_detect(educ, 'Associate') ~ 14,
             str_detect(educ, 'Bachelor') ~ 16,
             str_detect(educ, 'Doctoral') ~ 22,
             str_detect(educ, 'High') ~ 12,
             str_detect(educ, 'Less') ~ 10,
             str_detect(educ, 'Master') ~ 18,
             str_detect(educ, 'Professional') ~ 19,
             str_detect(educ, 'Some college') ~ 14,
           )) %>% 
    dplyr::select(show, age, male, retired, fulltime, hhi, whitenohisp, educ, cnnviewer, msnbcviewer, broadcast) %>% 
    group_by(show) %>% 
    summarise(across(everything(), mean, na.rm=T)) %>% 
    rename(Age = age, `Years of education` = educ, `Household income (\\$)` = hhi,
           `White` = whitenohisp, `Works full time` = fulltime, `Retired` = retired, 
           Male = male, `Watches CNN` = cnnviewer, `Watches MSNBC` = msnbcviewer,
           `Watches broadcast news` = broadcast) %>% 
    t() %>% 
    as.data.frame() 
  exc$demo = rownames(exc)
  exc = exc[-1,c(3,2,1)]
  if (!("V1" %in% names(exc))){
    exc = exc %>% rename(V1 = "1", V2="2")
  }
  
  exc = exc %>% mutate(across(c(V1, V2), function(x) round(as.numeric(x), 2)))
  
  exc = exc %>% 
    unite(latex, demo:V1, sep=' & ') %>% 
    mutate(latex = paste0(latex, '\\\\'))
  
  notes = tablenotes[['t:viewer-characteristics']]
  table = c(
    '\\begin{table}[!htbp] \\centering',
    '\\caption{Demographics of Tucker Carlson Tonight vs. Hannity viewers}',
    '\\label{t:viewer-characteristics}',
    '\\begin{tabular}{@{}lll@{}}',
    '\\toprule',
    '\\textbf{Demographic} & \\textbf{\\emph{Tucker Carlson Tonight}} & \\textbf{\\emph{Hannity}} \\\\ \\midrule',
    exc$latex,
    '\\bottomrule',
    '\\end{tabular}',
    '\\footnotesize',
    '\\begin{tablenotes}',
    str_interp('\\item \\textit{Notes:} ${notes}'),
    '\\end{tablenotes}',
    '\\end{table}'
  )
  
  write_lines(table, 'output/tables/behavior-characteristics.tex')

}
make_demographic_table(survey)

