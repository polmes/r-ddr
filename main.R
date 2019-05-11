# Define required packages
requirements <- c('data.table', 'fasttime', 'ggplot2')

# Check requirements
missing <- requirements[!(requirements %in% installed.packages()[,'Package'])]
if (length(missing)) {
	message('Installing missing packages...')
	install.packages(missing)
}

# Load requirements
slibrary <- function(pkg) { suppressPackageStartupMessages(library(pkg, character.only=TRUE, quietly=TRUE)) }
lapply(requirements, slibrary)

# Set working directory to source directory
sourceDir <- getSrcDirectory(function() {}) # R is dumb
setwd(sourceDir)
