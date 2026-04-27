##########
# CREATE SOCIAL DISTANCING FIGURE
##########

source('code/analysis/load.R')

bts = read_dta('data/working/BTS.dta')
bts_merged = bts %>% 
  inner_join(data %>% select(fipscode,
                             county:state_fips,
                             starts_with("pr_hutput_V1"),
                             fox_shr_Jan2020, msnbc_shr_of_cable, fox_shr_of_cable, IV_V1_hannity,
                             popweighted_lat, popweighted_lon,
                             repshare2016, ltotvotes2016,
                             starts_with("pop_"),
                             starts_with("lpop_"),
                             starts_with("edushare_"),
                             poor_physical_days_raw, uninsured_raw, urate_bls, perc_poor2018,
                             beds, nurses, personnel, zdiff_viewersHvT, lmed_hh_inc2018
  ) %>% distinct() , by=c('state_fips','county')) %>% 
  rename(lshare_stayhome = share_stayhome ) %>%  # just to make this play nice with the already-defined function
  arrange(county, state_fips, elapdate) %>% 
  group_by(county, state_fips) %>% 
  mutate(lshare_stayhome = zoo::rollmean(lshare_stayhome, 7, na.pad=T),
         lshare_stayhome_2019 = zoo::rollmean(share_stayhome_2019, 7, na.pad=T)) %>% 
  mutate(weights=1)

bts_df = make_coef_df('share_stayhome', '2SLS', misinfo=F, version='V1', bts_merged, cluster='geography', add_controls='+lshare_stayhome_2019')

plot = ggplot(bts_df %>% filter(elapdate<='2020-03-20'), aes(x = elapdate, y = coef)) + 
  geom_point(fill='black', size=2) + 
  geom_ribbon(aes(ymin=error_low, ymax=error_high), alpha=0.15) +
  ylab(str_interp('2SLS estimate')) +
  labs(fill='Outcome', col='Outcome', shape='Outcome') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        text = element_text(family='LM Roman 10')) +
  coord_cartesian(xlim=c(ymd('2020-01-05'),ymd('2020-03-19')))

ggsave('output/figures/social-distancing.png', plot, width=6, height=4)

