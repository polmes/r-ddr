# Define event
bru <- 'EBBR' # airport ICAO code
coord <- c(50.90139, 4.484444) # location (lat, lon)
event <- c('2016/03/22', '2016/04/03') # interval (closes, reopens)
nearby <- c('EBCI', 'EBLG', 'EBAW', 'EHEH', 'LFQQ') # nearby airports

# Convert to dates
event <- as.Date(event, '%Y/%m/%d')

# Airlines
airlines <- readRDS(ddr.path('airlines'))
airlines <- airlines[month == paste0(format(event[1], '%Y-%m'), '-01')]

# Fuel burn
fuelburn <- 6.1264 # fuel burn [kg/NM] average of A320's and B737's for short-haul flights

# Diverted flights
div <- merge(data$real$flights[, .(id, as.Date(landing), orig, dest)],
			 data$real$routes[, .SD[.N], by = id, .SDcols = c('pt')], by = c('id'))
div <- div[dest == bru & pt != dest & V2 == event[1]] # V2 = date
div <- div[, c('dest', 'V2') := NULL]
div <- merge(div, data$real$flights[id %in% div[, id], .(id, airline, cumdist)], by = c('id'))
div <- merge(div, data$plan$flights[id %in% div[, id], .(id, cumdist)], by = c('id'))
div[, diff := cumdist.x - cumdist.y]
div[, c('cumdist.x', 'cumdist.y') := NULL]
div[, fuel := fuelburn * diff]
div <- merge(div, airlines[, .(ICAO, name)], by.x = c('airline'), by.y = c('ICAO'))
div <- div[, airline := NULL]
div <- div[, .(id, orig, pt, name, diff, fuel)]
setnames(div, c('id', 'orig', 'dest', 'airline', 'diff', 'fuel'))

# Special flights while airport was closed ("UFOs")
ufo <- data$real$flights[(orig == bru | dest == bru) &
						 (as.Date(takeoff) > event[1] & as.Date(takeoff) < event[2]),
						 .(id, orig, dest, airline, aircraft, takeoff, landing)]
ufo <- merge(ufo, airlines[, .(ICAO, name)], by.x = c('airline'), by.y = c('ICAO'))
ufo[, airline := NULL]
ufo <- ufo[, .(id, orig, dest, name, aircraft, takeoff, landing)]
setnames(ufo, c('id', 'orig', 'dest', 'airline', 'aircraft', 'takeoff', 'landing'))

# Before/During/After
before <- merge(data$real$flights[orig %in% c(bru, nearby) & as.Date(takeoff) < event[1], .N,
								  by = .(as.Date(takeoff), orig)],
				data$real$flights[dest %in% c(bru, nearby) & as.Date(landing) < event[1], .N,
								  by = .(as.Date(landing), dest)],
				by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))
during <- merge(data$real$flights[orig %in% nearby & as.Date(takeoff) >= event[1] & as.Date(takeoff) <= event[2],
								  .N, by = .(as.Date(takeoff), orig)],
				data$real$flights[dest %in% nearby & as.Date(landing) >= event[1] & as.Date(landing) <= event[2],
								  .N, by = .(as.Date(landing), dest)],
				by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))
after <- merge(data$real$flights[orig %in% c(bru, nearby) & as.Date(takeoff) > event[2], .N,
								 by = .(as.Date(takeoff), orig)],
			   data$real$flights[dest %in% c(bru, nearby) & as.Date(landing) > event[2], .N,
								 by = .(as.Date(landing), dest)],
			   by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))
