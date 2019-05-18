ddr.load <- function(name) {
	file <- ddr.path(name)
	if (file.exists(file)) {
		# Load *.RDS file
		message(paste('Loading file:', basename(file)))
		datedata <- readRDS(file) # includes both real and plan data
	} else {
		# Get full *.so6 filenames
		both <- list.files(path = 'data', pattern = paste0('^', name, '.*\\.so6$'), full.names = TRUE)
		if (length(both) != 2) stop('Did not find expected files')

		# Process both *.so6 files
		realdata <- ddr.process(both[grepl('m3', both)]) # m3 = real
		plandata <- ddr.process(both[grepl('m1', both)]) # m1 = plan

		# Join real and plan data
		datedata <- list(real = realdata, plan = plandata)

		# Save for next time
		filename <- ddr.path(name)
		message(paste('Saving to file:', basename(filename)))
		saveRDS(datedata, filename)
	}
	return(datedata)
}
