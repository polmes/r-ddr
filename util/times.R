ddr.times <- function(flights, airport, event, onlydest = FALSE) {
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
				main = paste('Air Traffic Evolution at', airport),
				ylab = 'Number of Flights',
				width = '100%') %>%
			dySeries('N.x', label = paste(airport, 'as Origin')) %>%
			dySeries('N.y', label = paste(airport, 'as Destination')) %>%
			dyRangeSelector(dateWindow = event) %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(width = '500') %>%
			dyEvent(event[1], 'Disruption begins', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Disruption ends', labelLoc = 'bottom')
	} else {
		dygraph(allFlights,
				main = paste('Air Traffic Evolution at', airport),
				ylab = paste('Number of Flights')) %>%
			dySeries('N', label = paste(airport, 'as Destination')) %>%
			dyRangeSelector(dateWindow = event) %>%
			dyOptions(colors = c('royalblue', 'coral')) %>%
			dyLegend(show = 'always', width = '500') %>%
			dyEvent(event[1], 'Disruption begins', labelLoc = 'bottom') %>%
			dyEvent(event[2], 'Disruption ends', labelLoc = 'bottom')
	}

}
