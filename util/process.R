ddr.process <- function(file, getairplanes = FALSE) {
	# Extract filename from path
	filename <- basename(file)
	message(paste('Processing file:', filename))

	# m1 = plan / m3 = real
	# isreal <- if (substr(file, nchar(file) - 4, nchar(file) - 4) == '3') TRUE else FALSE

	colnames <- c('pts', 'orig', 'dest', 'aircraft', 'time1', 'time2', 'FL1', 'FL2', 'callsign',
				  'date1', 'date2', 'lat1', 'lon1', 'lat2', 'lon2', 'id', 'dist')
	colclasses <- list(character = c(5,6,11,12)) # time1, time2, date1, date2

	message('Reading file...')
	ddr <- fread(file, drop = c(9, 18, 20), stringsAsFactors = TRUE, col.names = colnames, colClasses = colclasses)

	message('Removing flights with special origins or destinations...')
	ddr <- ddr[!orig %in% c('ZZZZ', 'AFIL') & !dest %in% c('ZZZZ', 'AFIL')]

	message('Removing non-commercial traffic...')
	airlines <- readRDS(ddr.path('airlines'))
	availablemonths <- unique(airlines$month)
	filedate <- as.Date(substr(filename, 1, 8), '%Y%m%d')
	filemonth <- availablemonths[which.min(abs(availablemonths - filedate))]
	airlines <- airlines[month == filemonth]
	airlines <- airlines[, month := NULL]
	ddr[grepl('^[[:upper:]]{3}[[:digit:]]', ddr$callsign), airline := substr(callsign, 1, 3)]
	ddr <- ddr[airline %in% airlines$ICAO]
	rm(airlines)

	message('Computing cumulative distance...')
	ddr[, cumdist := cumsum(dist), by = id]

	# message('Removing technical-technical pairs of points...')
	# ddr <- ddr[!grepl('^[$%#!].+_[$%#!]', ddr$pts),]

	# message('Splitting IDs...')
	# ddr[, c('pt1', 'pt2') := tstrsplit(pts, '_', fixed = TRUE)]

	message('Converting dates...')
	ddr[, dt1 := fastPOSIXct(paste(substr(date1,1,2),'-',substr(date1,3,4),'-',substr(date1,5,6),'T', substr(time1,1,2),':',substr(time1,3,4),':',substr(time1,5,6)))]
	ddr[, dt2 := fastPOSIXct(paste(substr(date2,1,2),'-',substr(date2,3,4),'-',substr(date2,5,6),'T', substr(time2,1,2),':',substr(time2,3,4),':',substr(time2,5,6)))]
	ddr[, c('date1', 'date2', 'time1', 'time2') := NULL]

	# FLIGHTS: id orig dest callsign airline aircraft cumdist takeoff landing
	message('Processing flight data...')
	flights <- ddr[, .(id, orig, dest, callsign, airline, aircraft, cumdist)]
	ddr[, c('orig', 'dest') := NULL]
	flights <- flights[, .SD[.N], by = id]
	flights[, takeoff := ddr[, .SD[1], by = id, .SDcols = c('dt1')][, .(dt1)]]
	flights[, landing := ddr[, .SD[.N], by = id, .SDcols = c('dt2')][, .(dt2)]]
	# flights[, isreal := isreal]
	flights <- unique(flights)
	setorder(flights, id)

	# ROUTES: id dt lat lon FL
	message('Processing route (trajectory) data...')
	start <- ddr[, .SD[1], by = id, .SDcols = c('pts', 'dt1', 'lat1', 'lon1', 'FL1')]
	start[, pts := sub('^(.+)_.*$', '\\1', pts)]
	ddr[, c('lat1', 'lon1', 'FL1') := NULL]
	rest <- ddr[!grepl('_[$%#!]', ddr$pts), c('id', 'pts', 'dt2', 'lat2', 'lon2', 'FL2')]
	rest[, pts := sub('^.*_(.+)$', '\\1', pts)]
	ddr[, c('pts', 'lat2', 'lon2', 'FL2') := NULL]
	routecols <- c('id', 'pt', 'dt', 'lat', 'lon', 'FL')
	setnames(start, colnames(start), routecols)
	setnames(rest, colnames(rest), routecols)
	routes <- rbindlist(list(start, rest))
	routes[, `:=` (lat = lat/60, lon = lon/60)]
	rm(start, rest)
	routes <- unique(routes)
	setorder(routes, id, dt)

	# Export list of tables
	if (!getairplanes) {
		rm(ddr)

		tb <- list(flights = flights, routes = routes)
	} else {
		# AIRPLANES: callsign airline aircraft mindate maxdate
		message('Processing airplane data...')
		airplanes <- ddr[, .(callsign, airline, aircraft, dt1, dt2)]
		rm(ddr)
		airplanes[, `:=` (mindate = min(dt1), maxdate = max(dt2)), by = c('callsign')]
		airplanes[, c('dt1', 'dt2') := NULL]
		airplanes <- unique(airplanes)

		tb <- list(flights = flights, routes = routes, airplanes = airplanes)
	}

	message('Done.')
	return(tb)
}
