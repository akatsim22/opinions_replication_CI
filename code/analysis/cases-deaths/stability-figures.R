##########
# CREATE STABILITY FIGURES
##########

source('code/analysis/load.R')
if (!requireNamespace("starbility", quietly = TRUE)) {
  message("Package 'starbility' not available; continuing without it.")
}

deaths = data %>% filter(elapdate == '2020-03-28') 
cases =  data %>% filter(elapdate == '2020-03-14') 

datasets = list('deaths'=deaths, 'cases'=cases)

base_controls_ols = c('Base controls' = base_ols)
base_controls_iv = c('Base controls' = base_iv)

perm_controls = c('Rurality'='CensusPercentRural',
                  'Race controls'=poprace,
                  'Age controls'=age,
                  'Integration controls'=integration,
                  'Economic controls'=econ,
                  'Education controls'=educ,
                  'Health status controls'=health,
                  'Health capacity controls'=healthcap,
                  'Political controls'=politics)

nonperm_fe_controls = c('Division FE' = 'divisionfp',
                        'State FE' = 'state_fips')


# Stability figure
make_stability_plot = function(outcome, approach){
  lab = substring(outcome, 2, nchar(outcome))
    
  if (approach == "OLS"){
    basecontrols = base_controls_ols
    plot = stability_plot(
      data = datasets[[lab]],
      lhs = outcome,
      rhs = end,
      perm = perm_controls,
      base = basecontrols,
      nonperm_fe = nonperm_fe_controls,
      font = 'LM Roman 10',
      error_geom = 'ribbon',
      rel_height = 0.6,
      trim_top = 1,
      run_to = 6,
      cores=8)
  } else{
    basecontrols = base_controls_iv
    plot = stability_plot(
      data = datasets[[lab]],
      lhs = outcome,
      rhs = end,
      iv = 'IV_V1_hannity',
      perm = perm_controls,
      base = basecontrols,
      nonperm_fe = nonperm_fe_controls,
      font = 'LM Roman 10',
      error_geom = 'ribbon',
      rel_height = 0.6,
      trim_top = 1,
      run_to = 6,
      cores=8)
  }
    
  return(plot)
}
  
plot1 = make_stability_plot("lcases",  "OLS")
cowplot::plot_grid(plot1[[1]], plot1[[2]], align='v', ncol=1)
plot2 = make_stability_plot("lcases",  "IV" )
cowplot::plot_grid(plot2[[1]], plot2[[2]], align='v', ncol=1)
plot3 = make_stability_plot("ldeaths", "OLS")
cowplot::plot_grid(plot3[[1]], plot3[[2]], align='v', ncol=1)
plot4 = make_stability_plot("ldeaths", "IV" )
cowplot::plot_grid(plot4[[1]], plot4[[2]], align='v', ncol=1)
  
options(future.globals.maxSize= 2*1000000*1024^2) # needed to prevent crashing
  
coefs = map(list(plot1, plot2, plot3, plot4), function(x) x[[1]]+geom_hline(yintercept=0, linetype='dashed'))
  
plot = cowplot::plot_grid(coefs[[1]], coefs[[2]], coefs[[3]], coefs[[4]], plot4[[2]], 
                          labels = c('Cases OLS', 'Cases IV', 'Deaths OLS', 'Deaths IV', ''), 
                          label_x = 0.002,
                          hjust = -0.0,
                          label_fontface = 'bold',
                          label_size = 12,
                          ncol=1, 
                          align='v')
cowplot::save_plot(str_interp('output/figures/OLS-V1-stability.png'), plot, base_height=10, base_width=13)



# Oster figures
for (outcome in c('lcases','ldeaths')) {
  lab = substring(outcome, 2, nchar(outcome))

  dfs = stability_plot(data = datasets[[lab]],
                       lhs = outcome,
                       rhs = end,
                       perm = perm_controls,
                       base = base_controls_ols,
                       nonperm_fe = nonperm_fe_controls,
                       cluster = 'geography',
                       run_to = 5)

  oster = oster_plot(data=datasets[[lab]],
                     lhs = outcome,
                     rhs = end,
                     perm = perm_controls,
                     coef_grid = dfs[[1]],
                     font = 'LM Roman 10', run_from = 5)

  ggsave(str_interp('output/figures/oster-${lab}.png'), oster, width=8, height=4, units='in')

}

