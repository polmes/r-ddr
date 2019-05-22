ddr.times <- function(flights, airport, event, onlydest = FALSE, title = 'Air Traffic Evolution', group = NULL) {
	cols <- c('date', 'N')
	flightsWithDest <- flights[dest == airport, .N, by = as.Date(landing)]
	setnames(flightsWithDest, cols)

	if (!onlydest) {
		flightsWithOrig <- flights[orig == airport, .N, by = as.Date(takeoff)]
		setnames(flightsWithOrig, cols)
		allFlights <- merge(flightsWithOrig, flightsWithDest, by = c('date'))
	} else {
		allFlights <- flightsWithDest
	}

	if (!onlydest) {
		dygraph(allFlights,
				main = paste(title, 'at', airport),
				ylab = 'Number of Flights',
				group = group,
				width = '100%') %>%
			dySeries('N.x', label = paste(airport, 'Departures')) %>%
			dySeries('N.y', label = paste(airport, 'Arrivals')) %>%
			dyRangeSelector() %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(width = '330') %>%
			dyEvent(event[1], 'Airport closes', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Airport reopens', labelLoc = 'bottom')
	} else {
		dygraph(allFlights,
				main = paste(title, 'at', airport),
				ylab = paste('Number of Flights'),
				group = group,
				width = '100%') %>%
			dySeries('N', label = paste(airport, 'Arrivals')) %>%
			dyRangeSelector() %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(show = 'always', width = '240') %>%
			dyEvent(event[1], 'Airport closes', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Airport reopens', labelLoc = 'bottom')
	}
}
