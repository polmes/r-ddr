# Define event
bru <- 'EBBR'
event <- c('2016-03-22', '2016-04-03')

# Find diverted flights
div <- merge(data$real$routes[, .SD[.N], by = id, .SDcols = c('pt')],
			 data$real$flights[, .(id, dest, as.Date(landing))], by = c('id'))
div <- div[dest == bru & pt != dest & V3 == event[1]] # v3 = date
div <- div[, c('dest', 'V3') := NULL]
