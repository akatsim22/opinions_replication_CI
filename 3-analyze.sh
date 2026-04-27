# Trust survey
Rscript        code/analysis/trust/trust-figure-table.R

# Transcripts
Rscript        code/analysis/transcripts/show-content-fox.R
Rscript        code/analysis/transcripts/show-content-cnn-msnbc.R

# Behavior survey
Rscript        code/analysis/behavior/behavior-figures-tables.R
Rscript        code/analysis/behavior/behavior-categories-figure.R
stata-mp -e do code/analysis/behavior/behavior-representativeness.do

# Election survey
Rscript        code/analysis/election/election-figures-tables.R

# IV intuition
#Rscript        code/analysis/iv-intuition/iv-map.R
Rscript        code/analysis/iv-intuition/iv-intuition-figures.R 

# Instrument validation
Rscript        code/analysis/instrument-validation/first-stage-table.R 
Rscript        code/analysis/instrument-validation/exogeneity-figures.R 
stata-mp -e    code/analysis/instrument-validation/instrument-variations-fstats.do

# Social distancing
Rscript        code/analysis/distancing/social-distancing.R 

# Cases and deaths
Rscript        code/analysis/cases-deaths/cases-deaths-table.R 
Rscript        code/analysis/cases-deaths/timeseries-figures.R 
Rscript        code/analysis/cases-deaths/timing-figure.R
Rscript        code/analysis/cases-deaths/count-models.R
Rscript        code/analysis/cases-deaths/iv-variations-table.R 
Rscript        code/analysis/cases-deaths/misinformation-table.R
#Rscript        code/analysis/cases-deaths/timeseries-randomization.R
Rscript        code/analysis/cases-deaths/residuals-figures.R
Rscript        code/analysis/cases-deaths/stability-figures.R

# NLP
Rscript        code/analysis/nlp/nlp-figures.R

# EPI model
Rscript        code/analysis/epi-model/magnitudes.R
python3 -O     code/analysis/epi-model/run-model.py
Rscript        code/analysis/epi-model/simulations-figure.R
