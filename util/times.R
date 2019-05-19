ddr.times <- function(flights, airport, event, onlydest = FALSE, title = 'Air Traffic Evolution') {
	cols <- c('date', 'N')
	flightsWithDest <- flights[dest == airport, .N,  by = as.Date(landing)]
	setnames(flightsWithDest, cols)

	if (!onlydest) {
		flightsWithOrig <- flights[orig == airport, .N,  by = as.Date(takeoff)]
		setnames(flightsWithOrig, cols)
		allFlights <- merge(flightsWithOrig, flightsWithDest, by = c('date'))
	} else {
		allFlights <- flightsWithDest
	}

	if (!onlydest) {
		dygraph(allFlights,
				main = paste(title, 'at', airport),
				ylab = 'Number of Flights',
				width = '100%') %>%
			dySeries('N.x', label = paste(airport, 'Departures')) %>%
			dySeries('N.y', label = paste(airport, 'Arrivals')) %>%
			dyRangeSelector(dateWindow = event) %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(width = '450') %>%
			dyEvent(event[1], 'Disruption begins', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Disruption ends', labelLoc = 'bottom')
	} else {
		dygraph(allFlights,
				main = paste(title, 'at', airport),
				ylab = paste('Number of Flights'),
				width = '100%') %>%
			dySeries('N', label = paste(airport, 'Arrivals')) %>%
			dyRangeSelector(dateWindow = event) %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(show = 'always', width = '260') %>%
			dyEvent(event[1], 'Disruption begins', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Disruption ends', labelLoc = 'bottom')
	}
}
