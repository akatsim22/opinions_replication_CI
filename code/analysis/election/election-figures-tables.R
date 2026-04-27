##########
# CREATE ELECTION FIGURES AND TABLES
##########

library(tidyverse)
library(stargazer)
library(haven)
library(ggpubr)
library(starbility)
library(extrafont)

tablenotes = rjson::fromJSON(file='code/analysis/tablenotes.json')

survey <- read_dta('data/working/election-survey.dta') %>%
  mutate(foxviewer=(networks=='Fox News'))


# Election beliefs figure (for fox viewers and all viewers)
create_barplot = function(x) {
  summary = x %>% 
    group_by(foxopinion) %>% 
    summarise(across(c(votingmachines:inaugurate), list(mean=function(x) mean(x, na.rm=T), se=function(x) sd(x, na.rm=T)/sqrt(n()) ))) %>% 
    gather(outcome, mean, -foxopinion) %>% 
    separate(outcome, c('outcome', 'measure'), sep='_') %>% 
    pivot_wider(id_cols=c(foxopinion, outcome), names_from=measure, values_from=mean) %>% 
    mutate(outcome = recode(outcome, `inaugurate` = 'Trump will be\ninaugurated',
                            `popvote` = 'Trump won\npopular vote',
                            `votingmachines` = 'Voting machines\nswitched votes to Biden'),
           foxopinion = ifelse(foxopinion, 'Watches Fox opinion show','Does not watch\nFox opinion show'))
  
  
  ggplot(summary, aes(x = outcome, y = mean, fill=foxopinion)) +
    geom_bar(stat='identity', position = position_dodge2(), alpha=0.5) +
    scale_fill_manual(values=c("#6794a7", "#014d64")) +
    geom_errorbar(aes(ymin = mean-1.96*se, ymax=mean+1.96*se, col = foxopinion), position = position_dodge2()) + 
    scale_color_manual(values=c("#6794a7", "#014d64")) +
    coord_cartesian(ylim=c(0, 1)) +
    theme_bw() +
    theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        text = element_text(family='LM Roman 10')) +
    labs(fill = '', col= '') +
    xlab('Outcome') +
    ylab('Fraction who agree') 
}

create_barplot(survey) %>% ggsave('output/figures/election-allviewers.png', plot=., width=6, height=4)
create_barplot(survey %>% filter(foxviewer)) %>% ggsave('output/figures/election-foxviewers.png', plot=., width=6, height=4)


# Election table
create_panel = function(x, foxonly) {
  if (foxonly) {
    m1 = lm(votingmachines~age+male+white+hispanic+married+foxopinion+education+hhi+party, data=x) 
    m2 = lm(popvote~age+male+white+hispanic+married+foxopinion+education+hhi+party, data=x)
    m3 = lm(inaugurate~age+male+white+hispanic+married+foxopinion+education+hhi+party, data=x)
    labels = c('Watches Fox opinion show')
    keep = c('foxopinion')
  } else {
    m1 = lm(votingmachines~age+male+white+hispanic+married+foxopinion+msnbcopinion+foxviewer+msnbcviewer+cnnviewer+education+hhi+party, data=x) 
    m2 = lm(popvote~age+male+white+hispanic+married+foxopinion+msnbcopinion+foxviewer+msnbcviewer+cnnviewer+education+hhi+party, data=x)
    m3 = lm(inaugurate~age+male+white+hispanic+married+foxopinion+msnbcopinion+foxviewer+msnbcviewer+cnnviewer+education+hhi+party, data=x)
    labels = c('Watches Fox opinion show', 'Watches MSNBC opinion show', 'Watches Fox News', 'Watches MSNBC', 'Watches CNN')
    keep = c('opinion','foxviewer','msnbcviewer','cnnviewer')
  }
  
  means = map_dbl(c('votingmachines','popvote','inaugurate'), function(y) mean(x[[y]])) %>% round(3) %>% format(nsmall=3) 

  stargazer(list(m1, m2, m3),
            column.labels = c('Voting machines','Popular vote','Inauguration'),
            title = 'Correlation between opinion show viewership and election conspiracism',
            label = 't:elections-survey',
            keep = keep,
            covariate.labels = labels,
            add.lines = list(
              c('Dep.~var.~mean', means)
            )
            )
}

panela = create_panel(survey, foxonly=F)
panelb = create_panel(survey %>% filter(foxviewer), foxonly=T)

table = c(
  panela[4:6],
  '\\begin{threeparttable}',
  panela[7:10],
  panela[12:14],
  '\\multicolumn{4}{l}{\\textbf{Panel A}: All respondents} \\\\',
  '\\midrule',
  panela[c(15:29, 31:32)],
  '\\midrule',
  '\\multicolumn{4}{l}{\\textbf{Panel B}: Fox News viewers only} \\\\',
  '\\midrule',
  panelb[c(15:17, 19:20)],
  panelb[25],
  panelb[27],
  '\\begin{tablenotes} \\small',
  paste0('\\item \\textit{Notes:} ', str_interp(tablenotes[['t:elections-survey']])),
  '\\end{tablenotes} \\end{threeparttable} \\end{table}'
)
write_lines(table, 'output/tables/election.tex')



# Election survey characteristics 
table_demographics = function(){
  demographics = c("age", "hhi", "hhsize", "white", "hispanic", "educ", "married", "party_rep", "vote16_trump", "vote20_trump")
  
  # Show viewers
  survey = survey %>% mutate(hannityviewer = grepl("Hannity", foxshows),
                             carlsonviewer = grepl("Tucker Carlson Tonight", foxshows),
                             hannitycarlsonviewer = (hannityviewer==1 & carlsonviewer==1),
                             hannityonlyviewer    = (hannityviewer==1 & carlsonviewer==0),
                             carlsononlyviewer     = (hannityviewer==0 & carlsonviewer==1)) %>% 
    filter(hannityonlyviewer | carlsononlyviewer) %>% 
    mutate(show = ifelse(hannityonlyviewer, 'Hannity', 'Tucker Carlson Tonight')) %>% 
    dplyr::select(any_of(demographics), show) %>% 
    group_by(show) %>% 
    summarise(across(everything(), mean, na.rm=T)) %>% 
    rename(Age = age, `Years of education` = educ, `Household income (\\$)` = hhi,
           `White` = white, `Hispanic`=hispanic, `Married` = married, `Household size` = hhsize, `Republican`=party_rep, `2016 Trump voter`=vote16_trump, `2020 Trump voter`=vote20_trump) %>% 
    t() %>% 
    as.data.frame() 
  
  survey$demo = rownames(survey)
  survey = survey[-1,c(3,2,1)]
  if (!("V1" %in% names(survey))){
    survey = survey %>% rename(V1 = "1", V2="2")
  }
  
  survey = survey %>% mutate(across(c(V1, V2), function(x) round(as.numeric(x), 2)))
  
  survey = survey %>% 
    unite(latex, demo:V1, sep=' & ') %>% 
    mutate(latex = paste0(latex, '\\\\'))
  
  notes = tablenotes[['t:elections-characteristics']]
  
  table = c(
    '\\begin{table}[!htbp] \\centering',
    '\\caption{Demographics of Tucker Carlson Tonight vs. Hannity viewers (Dec 2020 survey)}',
    '\\label{t:election-characteristics}',
    '\\begin{tabular}{@{}lll@{}}',
    '\\toprule',
    '\\textbf{Demographic} & \\textbf{\\emph{Tucker Carlson Tonight}} & \\textbf{\\emph{Hannity}} \\\\ \\midrule',
    survey$latex,
    '\\bottomrule',
    '\\end{tabular}',
    '\\footnotesize',
    '\\begin{tablenotes}',
    str_interp('\\item \\textit{Notes:} ${notes}'),
    '\\end{tablenotes}',
    '\\end{table}'
  )
  
  write_lines(table, 'output/tables/election-characteristics.tex')
}

table_demographics()


# Election stability
perm_controls = c(
  'Age and marital status' = 'age + married',
  'Gender' = 'male',
  'Race and ethnicity' = 'white+hispanic',
  'Education' = 'education',
  'Income' = 'hhi',
  'Partisanship' = 'party'
)

make_stability_plot = function(outcome) {

  plot = stability_plot(
    data = survey,
    lhs = outcome,
    rhs = 'foxopinion',
    perm = perm_controls,
    font = 'LM Roman 10',
    error_geom = 'ribbon',
    rel_height = 0.6,
    trim_top = 1,
    run_to = 6,
    cores=4)

  
  return(plot)
}
plot1 = make_stability_plot('inaugurate')
plot2 = make_stability_plot('popvote')
plot3 = make_stability_plot('votingmachines')

coefs = map(list(plot1, plot2, plot3), function(x) x[[1]]+geom_hline(yintercept=0, linetype='dashed'))

plot = cowplot::plot_grid(coefs[[1]], coefs[[2]], coefs[[3]], plot3[[2]], 
                          labels = c('Inauguration', 'Popular vote', 'Voting machines', ''), 
                          label_x = 0.002,
                          hjust = -0.0,
                          label_fontface = 'bold',
                          label_size = 10,
                          ncol=1, 
                          align='v')
cowplot::save_plot('output/figures/election-stability.png', base_height=4, base_width=8, plot=plot)
