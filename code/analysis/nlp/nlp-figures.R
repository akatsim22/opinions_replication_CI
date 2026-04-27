##########
# CREATE NLP FIGURES
##########


library(tidyverse)
library(extrafont)

folder = 'raw'  # change to 'working' if downloading LexisNexis data and running code in additional-code

topics = list(
  '2019' = c('Mueller investigation', 
           'Natural disasters', 'Russian interference', 'Economic policy', 
           'Counterintelligence', 'Sexual assault', 'Hate crime', 'Border security',
           'Legislation', 'Trump impeachment'),
  '2020' = c('Schools', 'Presidential campaign', 'COVID-19 policy', 'Policing and BLM', 
             'Mueller investigation', 'Trump COVID-19 treatment', 'Election integrity', 'Trump impeachment',
             'Hunter Biden', 'Trump campaign'))

shows = c(
  'Carlson' = 'Tucker Carlson Tonight',
  '11thHour' = '11th Hour',
  'LastWord' = 'The Last Word',
  'Maddow' = 'Rachel Maddow',
  'MSNBCLive' = 'MSNBC Live',
  'Special' = 'Special Report',
  'Story' = 'The Story',
  'TheBeat' = 'The Beat')

create_topic_figure = function(type, year) {

  if (type == 'aggregate') {
    files = str_interp('data/${folder}/nlp/${year}/pairwise-L2.csv')
  } else if (type == 'separate') {
    files = paste0(str_interp('data/${folder}/nlp/${year}/pairwise-L2-topic'), 0:9, '.csv')
  }
  
  similarities = list()
  
  for (f in files) {
    similarities[[length(similarities)+1]] = read_csv(f) %>% 
      mutate(week = lubridate::ymd( str_interp("${year}-01-01") ) + lubridate::weeks( week - 1 )) %>% 
      rename(L2_Hannity_11thHour = L2_11thHour_Hannity) %>% 
      select(week, contains('Hannity')) %>% 
      pivot_longer(-week, names_to='pair') %>% 
      mutate(show = str_split(pair, '_') %>% map_chr(3),
             show = factor(shows[show], levels = shows)) %>% 
      group_by(week) %>% 
      mutate(value = value - mean(value, na.rm=T)) %>% 
      ungroup()
    
    if (type=='separate') {
      similarities[[length(similarities)]]$topic = topics[[as.character(year)]][as.numeric(substring(f, nchar(f)-4, nchar(f)-4))+1]
    }
  }
  
  if (type=='aggregate') {
    similarity = bind_rows(similarities) %>% 
      filter(week>lubridate::ymd( str_interp("${year}-01-15") ))
  } else {
    similarity = bind_rows(similarities) %>% 
      filter(week>lubridate::ymd( str_interp("${year}-01-15") )) %>% 
      mutate(topic = factor(topic, levels=topics[[as.character(year)]]))
  }
  
  
  if (year==2020) {
    similarity = similarity %>%
      filter(week<lubridate::ymd( str_interp("${year}-05-01") )) 
  }

  plot = ggplot(similarity, aes(x = week, y = value, col=show)) + 
    geom_line(aes(alpha = show=='Tucker Carlson Tonight', size=show=='Tucker Carlson Tonight')) +
    geom_point(aes(alpha = show=='Tucker Carlson Tonight', size=show=='Tucker Carlson Tonight')) +
    scale_alpha_manual(values=c(0.6, 1)) +
    scale_size_manual(values=c(0.5, 1)) +
    guides(alpha=F, size=F) +
    labs(col='Program') +
    theme_bw() +
    ylab('L2 distance') +
    xlab('Date') +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          text = element_text(family='LM Roman 10'))
  
  if (type!='aggregate') {
    plot = plot + facet_wrap(~topic, ncol=2, scales='free')
  }
  
  ggsave(str_interp('output/figures/topic-${type}-${year}.png'), width=8,
         height=ifelse(type == 'aggregate', 5, 10), 
         plot)
  
}

create_bert_figure = function(type, year) {
  range = ifelse(year==2019, '2019-01-01-2019-11-30', '2020-01-01-2020-11-30')
  if (type == 'aggregate') {
    files = str_interp('data/${folder}/nlp/${year}/pairwise-L2-BERT.csv')
  } else {
    files = paste0(str_interp('data/${folder}/nlp/${year}/pairwise-L2-BERT-topic'), 0:9, '.csv')
  }
  
  similarities = list()
  
  for (f in files) {
    similarities[[length(similarities)+1]] = read_csv(f) %>% 
      mutate(week = lubridate::ymd( str_interp("${year}-01-01") ) + lubridate::weeks( week - 1 )) %>% 
      rename(L2_Hannity_11thHour = L2_11thHour_Hannity) %>% 
      select(week, contains('Hannity')) %>% 
      pivot_longer(-week, names_to='pair') %>% 
      mutate(show = str_split(pair, '_') %>% map_chr(3),
             show = factor(shows[show], levels = shows)) %>% 
      group_by(week) %>% 
      mutate(value = value - mean(value, na.rm=T)) %>% 
      ungroup()
    
    if (type=='separate') {
      similarities[[length(similarities)]]$topic = topics[[as.character(year)]][as.numeric(substring(f, nchar(f)-4, nchar(f)-4))+1]
    }
  }
  
  similarity = bind_rows(similarities) %>% filter(week>lubridate::ymd( str_interp("${year}-01-08") ))
  
  if (type=='separate') {
    similarity = similarity%>% 
    mutate(topic = factor(topic, levels=topics[[as.character(year)]]))
  }
  
  plot = ggplot(similarity, aes(x = week, y = value, col=show)) + 
    geom_line(aes(alpha = show=='Tucker Carlson Tonight', size=show=='Tucker Carlson Tonight')) +
    scale_alpha_manual(values=c(0.6, 1)) +
    scale_size_manual(values=c(0.5, 1)) +
    guides(alpha=F, size=F) +
    labs(col='Program') +
    theme_bw() +
    ylab('L2 distance') +
    xlab('Date') +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          text = element_text(family='LM Roman 10'))
  
  if (type=='separate') {
    plot = plot + facet_wrap(~topic, ncol=2, scales='free')
  }
  
  tag = ifelse(type == 'aggregate', 'aggregate', 'separate')
  ggsave(str_interp('output/figures/bert-${tag}-${year}.png'), width=8,
         height=ifelse(type == 'aggregate', 5, 10), 
         plot)
  
}

create_topic_figure('aggregate', 2019)  # Figure A1, Panel A
create_topic_figure('separate', 2019)   # Figure C1
create_topic_figure('separate', 2020)   # Figure C3
create_bert_figure('aggregate', 2019)   # Figure A1, Panel B
create_bert_figure('separate', 2019)    # Figure C2
