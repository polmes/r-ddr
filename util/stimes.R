ddr.stimes <- function(flights, airport, event, onlydest = FALSE) {
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
		ggplot(data = allFlights, aes(x = date)) +
			geom_line(aes(y = N.x, color = paste(airport, 'Departures'))) +
			geom_line(aes(y = N.y, color = paste(airport, 'Arrivals'))) +
			scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
			geom_vline(xintercept = event, linetype = 2) +
			labs(x = NULL, y = 'Number of Flights', color = NULL) +
			annotate('text', x = event[1], label = 'Airport closes', y = 10, angle = 90, vjust = -1, hjust = 0) +
			annotate('text', x = event[2], label = 'Airport reopens', y = 10, angle = 90, vjust = -1, hjust = 0) +
			theme(legend.position = 'top')
	} else {
		ggplot(data = allFlights, aes(x = date)) +
			geom_line(aes(y = N, color = paste(airport, 'Arrivals'))) +
			scale_x_date(date_breaks = '1 week', date_labels = '%d/%m') +
			geom_vline(xintercept = event, linetype = 2) +
			labs(x = NULL, y = 'Number of Flights', color = NULL) +
			annotate('text', x = event[1], label = 'Airport closes', y = 10, angle = 90, vjust = -1, hjust = 0) +
			annotate('text', x = event[2], label = 'Airport reopens', y = 10, angle = 90, vjust = -1, hjust = 0) +
			theme(legend.position = 'top')
	}
}
