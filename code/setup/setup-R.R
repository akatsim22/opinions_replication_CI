####################################
# global libraries used everywhere #
####################################

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
		        install.packages(x,dep=TRUE)
			} else {
			remotes::install_version(x, y)
			}
		if(!require(x,character.only = TRUE)) stop("Package not found")
	}
	return("OK")
}

global.libraries <- c("tidyverse", "haven", "lfe", "starpolishr", "lubridate", "cowplot", "broom",
                      "extrafont", "lubridate", "stringr", "ggpubr", "zoo", "tidyr", "RColorBrewer", "multcomp",
                      "starbility", "fixest", "pscl", "statar", "doParallel", "gridExtra", "tmap", "sf", "cutr",
                      "splines", "devtools")

results <- sapply(as.list(global.libraries), pkgTest)

# Install stargazer booktabs
install.packages("devtools")
library(devtools)
install_github("markwestcott34/stargazer-booktabs", force=T)

# Font
# Run once
install.packages("extrafont")
library(extrafont)
font_import(pattern = "lmodern*")