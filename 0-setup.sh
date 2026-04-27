# Delete existing folders
rm -r data/working
rm -r output


# Create folder structure
mkdir data/working
mkdir data/working/epi
mkdir data/working/nlp
mkdir data/working/nlp/2019
mkdir data/working/nlp/2020
mkdir output
mkdir output/tables
mkdir output/figures

# Setup Stata, R, and Python
#stata-mp -e do code/setup/setup-stata.do
Rscript code/setup/setup-R.R
pip install -r code/setup/requirements.txt