# Define required packages
requirements <- c('data.table', 'fasttime', 'ggplot2')

# Check requirements
missing <- requirements[!(requirements %in% installed.packages()[, 'Package'])]
if (length(missing)) {
	message('Installing missing packages...')
	install.packages(missing)
}

# Load requirements
slibrary <- function(pkg) { suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE)) }
lapply(requirements, slibrary)

# Set working directory to source directory
sourceDir <- getSrcDirectory(function() {}) # R is dumb
setwd(sourceDir)

# if RDS exists:
	# load
# else:
	# process data/ files
	# ls <- list.files(pattern = '\\.so6$')

source('processor.R')
file <- 'data/20160222_20160226_0000_2359_____m3.so6'
system.time(
	test <- processor(file, getairplanes = TRUE)
)
