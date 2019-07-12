ddr.render <- function(file, dir = 'report', format = 'html') {
	rep <- rmarkdown::render(file.path(dir, file), quiet = TRUE)
	if (format == 'html') {
		tmp <- tempfile(file, fileext = paste0('.', format))
		file.copy(rep, tmp)
		rstudioapi::viewer(tmp)
	} else if (format == 'pdf') {
		system2(Sys.getenv('R_PDFVIEWER'), paste0(file.path(dir, tools::file_path_sans_ext(file)), '.pdf'))
	}
}
