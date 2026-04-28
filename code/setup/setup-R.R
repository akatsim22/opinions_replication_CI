####################################
# global libraries used everywhere #
####################################

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# mran.date <- "2019-09-01"
# options(repos=paste0("https://cran.microsoft.com/snapshot/",mran.date,"/"))

# Note: Using this with a specific version number may fail, since not all dependencies might be met.
# Debug interactively, then identify all installed packages that needed to be pinned.

pkgTest <- function(x,y="")
{
	if (!require(x,character.only = TRUE))
	{
		if ( y == "" ) 
			{
		        tryCatch(
		          install.packages(x, dep = TRUE),
		          error = function(e) message(sprintf("Install failed for '%s': %s", x, e$message))
		        )
			} else {
			tryCatch(
			  {
			    if (!requireNamespace("remotes", quietly = TRUE)) {
			      install.packages("remotes")
			    }
			    remotes::install_version(x, y)
			  },
			  error = function(e) message(sprintf("Versioned install failed for '%s': %s", x, e$message))
			)
			}
		if(!require(x,character.only = TRUE)) {
		  warning(sprintf("Package '%s' not found for this R setup; skipping.", x))
		  return(FALSE)
		}
	}
	return(TRUE)
}

global.libraries <- c("tidyverse", "haven", "lfe", "lubridate", "cowplot", "broom",
                      "extrafont", "lubridate", "stringr", "ggpubr", "zoo", "tidyr", "RColorBrewer", "multcomp",
					  "fixest", "pscl", "statar", "doParallel", "gridExtra", "tmap", "sf", "cutr",
					  "splines", "stargazer", "rjson")

results <- vapply(global.libraries, pkgTest, logical(1))
names(results) <- global.libraries
if (any(!results)) {
	skipped <- names(results)[!results]
	warning(sprintf("Setup completed with skipped packages: %s", paste(skipped, collapse = ", ")))
}

# Optional: install stargazer booktabs fork for specific table formatting
if (interactive()) {
	tryCatch(
		{
			if (!requireNamespace("remotes", quietly = TRUE)) {
				install.packages("remotes")
			}
			remotes::install_github("markwestcott34/stargazer-booktabs", force = TRUE)
		},
		error = function(e) message(sprintf("Optional install failed for stargazer-booktabs: %s", e$message))
	)
} else {
	message("Skipping optional stargazer-booktabs install in non-interactive setup.")
}

# Font
# Run once
install.packages("extrafont")
library(extrafont)
tryCatch(
	font_import(pattern = "lmodern*", prompt = FALSE),
	error = function(e) message(sprintf("Optional font import skipped: %s", e$message))
)