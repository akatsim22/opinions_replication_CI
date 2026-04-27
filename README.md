Setup

The original replication package includes a full pipeline implemented in Stata, R, and Python.

As this replication focuses on reproducing the main analysis results using the provided processed datasets, we do not run the Stata-based data cleaning and construction steps.

Instead, we install the required R and Python dependencies using:

Rscript code/setup/setup-R.R  
pip install -r code/setup/requirements.txt

Note: 

From setup-R.R the following packages were not found and therefore eliminated from the script: 

"starbility”

"starpolishr"

"cutr”

And the package "stargazer" was added, as it is used in the trust-figure-table.R script.

All analysis scripts run successfully using the provided working datasets.



