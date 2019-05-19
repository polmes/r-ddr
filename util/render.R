ddr.render <- function(file, dir = 'report') {
	rep <- rmarkdown::render(file.path(dir, file), quiet = TRUE)
	tmp <- tempfile(file, fileext = '.html')
	file.copy(rep, tmp)
	rstudioapi::viewer(tmp)
}
