Setup

The original replication package includes a full pipeline implemented in Stata, R, and Python.

As this replication focuses on reproducing the main analysis results using the provided processed datasets, we do not run the Stata-based data cleaning and construction steps.

Instead, we install the required R and Python dependencies using:

Rscript code/setup/setup-R.R  
pip install -r code/setup/requirements.txt

For trust analysis, the following fixes were applied to run code/analysis/trust/trust-figure-table.R successfully:

- Added stargazer to the R dependency list in code/setup/setup-R.R.
- Added rjson to the R dependency list in code/setup/setup-R.R.
- Set a default CRAN mirror in code/setup/setup-R.R:
	options(repos = c(CRAN = "https://cran.rstudio.com/"))
- Updated the plot font in code/analysis/trust/trust-figure-table.R from LM Roman 10 to sans for cross-platform compatibility.

General setup compatibility changes:

- Removed starpolishr from required package installation in code/setup/setup-R.R because it is not available for current R versions.
- Added an internal fallback for star_notes_tex in code/analysis/load.R and code/analysis/behavior/behavior-figures-tables.R so table-note rendering still works when starpolishr is unavailable.
- Removed starbility from required package installation in code/setup/setup-R.R to prevent setup failures on current R versions.
- Updated scripts that referenced starbility to avoid hard-failing at import time when that package is unavailable.
- Updated package installation in code/setup/setup-R.R to continue on unavailable packages (with warnings) instead of stopping the entire setup.
- Added a final skipped-package summary warning in code/setup/setup-R.R so missing optional dependencies are explicitly reported.
- Removed devtools as a hard setup dependency in code/setup/setup-R.R to avoid fs namespace/version lock failures.
- Made the stargazer-booktabs GitHub installation optional via remotes with tryCatch, so setup continues even if that optional step fails.
- Made extrafont import non-interactive in code/setup/setup-R.R (`prompt = FALSE`) to avoid setup pausing for terminal input.
- Configured code/setup/setup-R.R to skip the optional stargazer-booktabs GitHub install during non-interactive runs (such as Rscript setup runs), preventing setup failures tied to build tools.

With data/working/trust-survey.dta present, code/analysis/trust/trust-figure-table.R now completes and generates output in output/figures and output/tables.



