# Input date range
day1 <- '2016/02/22'
day2 <- '2016/05/03'

# Define required packages
requirements <- c('data.table', 'fasttime', 'ggplot2', 'rstudioapi', 'htmltools', 'xts',
				  'maps', 'mapproj', 'dygraphs', 'DT', 'leaflet', 'gganimate')

# Check requirements
missing <- requirements[!(requirements %in% installed.packages()[, 'Package'])]
if (length(missing)) {
	message('Installing missing packages...')
	install.packages(missing)
}

# Load requirements
slibrary <- function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
lapply(requirements, slibrary)

# Set working directory to source directory
sourceDir <- getSrcDirectory(function() {}) # R is dumb
setwd(sourceDir)

# Load functions in 'util/'
lapply(list.files(path = 'util', full.names = TRUE), source)

# Get file path util function
ddr.path <- function(name, ext = '.RDS') return(file.path('data', paste0(name, ext)))

# Load or process 'data/' files
day1 <- as.Date(day1, '%Y/%m/%d')
day2 <- as.Date(day2, '%Y/%m/%d')

name <- paste0(format(day1, '%Y%m%d'), '_', format(day2, '%Y%m%d'))
file <- ddr.path(name)
if (file.exists(file)) {
	message(paste('Loading full file:', basename(file)))
	data <- readRDS(file)
} else {
	# Divide in intervals of 5 days
	days <- seq(day1, day2, by = 'days')
	int1 <- days[seq(1, length(days), 5)]
	int2 <- if (length(days) >= 5) days[seq(5, length(days), 5)] else day2
	names <- paste0(format(int1, '%Y%m%d'), '_', format(unique(c(int2, day2)), '%Y%m%d'))

	# Try loading *.RDS, process *.so6 if it doesn't exist
	data <- lapply(names, ddr.load)
	names(data) <- names

	# Merge tables + Ensure uniqueness
	data <- list(real = list(flights = unique(rbindlist(lapply(data, function(x) x$real$flights)), by = c('id')),
							 routes = unique(rbindlist(lapply(data, function(x) x$real$routes)))),
				 plan = list(flights = unique(rbindlist(lapply(data, function(x) x$plan$flights)), by = c('id')),
				 			 routes = unique(rbindlist(lapply(data, function(x) x$plan$routes)))))

	# Save for next time
	message(paste('Saving to full file:', basename(file)))
	saveRDS(data, file)
}

# Call analysis
message('Performing data analysis...')
source('analysis.R')

# Render report
message('Rendering report...')
ddr.render('paper.Rmd', format = 'pdf')

# End
message('Done.')
