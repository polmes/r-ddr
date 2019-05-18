# Define required packages
requirements <- c('data.table', 'fasttime', 'ggplot2', 'maps', 'mapproj')

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

# Load functions in 'util/'
lapply(list.files(path = 'util', full.names = TRUE), source)

# if RDS exists:
	# load
# else:
	# process data/ files
	# ls <- list.files(pattern = '\\.so6$')

# Test data processing
file <- 'data/20160222_20160226_0000_2359_____m3.so6'
test <- ddr.process(file, getairplanes = TRUE)

# Test map plotting
ddr.map(test$routes[1:1000], autocenter = FALSE)
