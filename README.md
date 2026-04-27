Setup

The original replication package includes a full pipeline implemented in Stata, R, and Python.

As this replication focuses on reproducing the main analysis results using the provided processed datasets, we do not run the Stata-based data cleaning and construction steps.

Instead, we install the required R and Python dependencies using:

Rscript code/setup/setup-R.R  
pip install -r code/setup/requirements.txt

All analysis scripts run successfully using the provided datasets.



