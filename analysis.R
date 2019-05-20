# Define event
bru <- 'EBBR'
coord <- c(50.90139, 4.484444) # lat, lon
event <- c('2016-03-22', '2016-04-03')

# Find diverted flights
div <- merge(data$real$routes[, .SD[.N], by = id, .SDcols = c('pt')],
			 data$real$flights[, .(id, dest, as.Date(landing))], by = c('id'))
div <- div[dest == bru & pt != dest & V3 == event[1]] # v3 = date
div <- div[, c('dest', 'V3') := NULL]

# Fuel burn
fuelburn <- 6.1264 # fuel burn [kg/NM] average of A320's and B737's for short-haul flights
info <- merge(data$real$flights[id %in% div[, id], .(id, airline, orig, cumdist)],
			  data$plan$flights[id %in% div[, id], .(id, cumdist)],
			  by = c('id'))
info[, diff := cumdist.x - cumdist.y]
info[, c('cumdist.x', 'cumdist.y') := NULL]
info <- merge(div, info, by = c('id'))
info[, fuel := fuelburn * diff]

# Airlines
airlines <- readRDS(ddr.path('airlines'))
# airlines <- airlines[month == unique(airlines$month)[which.min(abs(unique(airlines$month) - as.Date(event[1], '%Y-%m-%d')))]]
airlines <- airlines[month == paste0(format(as.Date(event[1], '%Y-%m-%d'), '%Y-%m'), '-01')]
info <- merge(info, airlines[, .(ICAO, name)], by.x = c('airline'), by.y = c('ICAO'))
info <- info[, airline := NULL]

# Final table
info <- info[, .(id, orig, pt, name, diff, fuel)]
setnames(info, c('id', 'orig', 'dest', 'airline', 'diff', 'fuel'))
