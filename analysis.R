# Define event
bru <- 'EBBR' # airport ICAO code
coord <- c(50.90139, 4.484444) # location (lat, lon)
event <- c('2016/03/22', '2016/04/03') # interval (closes, reopens)
nearby <- c('EBCI', 'EBLG', 'EBAW', 'EHEH', 'LFQQ') # nearby airports
bcn <- 'LEBL' # airport for comparison

# Convert to dates
event <- as.Date(event, '%Y/%m/%d')

# Airlines
airlines <- readRDS(ddr.path('airlines'))
airlines <- airlines[month == paste0(format(event[1], '%Y-%m'), '-01')]
airlines <- airlines[, name := trimws(name)]

# Fuel burn
fuelburn <- 6.1264 # fuel burn [kg/NM] average of A320's and B737's for short-haul flights

# All European Traffic
eu <- data$real$flights[as.Date(takeoff) >= day1 & as.Date(takeoff) <= day2, .N, by = as.Date(takeoff)]
setnames(eu, c('date', 'N'))

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
before <- merge(data$real$flights[orig %in% c(bru, nearby, bcn) & as.Date(takeoff) < event[1], .N,
								  by = .(as.Date(takeoff), orig)],
				data$real$flights[dest %in% c(bru, nearby, bcn) & as.Date(landing) < event[1], .N,
								  by = .(as.Date(landing), dest)],
				by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))
during <- merge(data$real$flights[orig %in% c(bru, nearby, bcn) & as.Date(takeoff) >= event[1] & as.Date(takeoff) <= event[2],
								  .N, by = .(as.Date(takeoff), orig)],
				data$real$flights[dest %in% c(bru, nearby, bcn) & as.Date(landing) >= event[1] & as.Date(landing) <= event[2],
								  .N, by = .(as.Date(landing), dest)],
				by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))
after <- merge(data$real$flights[orig %in% c(bru, nearby, bcn) & as.Date(takeoff) > event[2], .N,
								 by = .(as.Date(takeoff), orig)],
			   data$real$flights[dest %in% c(bru, nearby, bcn) & as.Date(landing) > event[2], .N,
								 by = .(as.Date(landing), dest)],
			   by.x = c('as.Date', 'orig'), by.y = c('as.Date', 'dest'))

# Delays
depdel <- merge(data$real$flights[orig %in% c(bru, nearby), .(id, orig, takeoff)],
				data$plan$flights[orig %in% c(bru, nearby), .(id, takeoff)], by = c('id'))
depdel[, delay := as.numeric(takeoff.x - takeoff.y) / 60] # convert to minutes
depdel[, c('id', 'takeoff.y') := NULL]
depdel <- depdel[, round(mean(delay)), by = .(as.Date(takeoff.x), orig)]
arrdel <- merge(data$real$flights[dest %in% c(bru, nearby), .(id, dest, landing)],
				data$plan$flights[dest %in% c(bru, nearby), .(id, landing)], by = c('id'))
arrdel[, delay := as.numeric(landing.x - landing.y) / 60] # convert to minutes
arrdel[, c('id', 'landing.y') := NULL]
arrdel <- arrdel[, round(mean(delay)), by = .(as.Date(landing.x), dest)]
lapply(list(depdel, arrdel), setnames, c('date', 'airport', 'delay'))
delays <- merge(depdel, arrdel, by = c('date', 'airport'))
rm(depdel, arrdel)
setnames(delays, c('delay.x', 'delay.y'), c('depdel', 'arrdel'))

# Affected Airlines
fucked <- data$real$flights[, .N, by = .(as.Date(takeoff), airline)]
fucked <- merge(merge(fucked[as.Date < event[1], round(mean(N)), by = airline],
					  fucked[as.Date >= event[1] & as.Date <= event[2], round(mean(N)), by = airline],
					  by = c('airline')),
				fucked[as.Date > event[2], round(mean(N)), by = airline],
				by = c('airline'))
fucked[airline %in% unique(data$real$flights[orig == bru | dest == bru, airline])]
fucked <- merge(fucked, airlines[, .(ICAO, name)], by.x = c('airline'), by.y = c('ICAO'))
setnames(fucked, c('icao', 'before', 'during', 'after', 'airline'))
fucked <- fucked[, .(airline, icao, before, during, after)]
fucked[, bd_abs := during - before]
fucked[, ba_abs := after - before]
fucked[, bd_rel := (during - before) / before]
fucked[, ba_rel := (after - before) / before]

# Most Affected Airlines
mostfucked <- fucked[order(bd_abs), icao][1:5]
fuckedplot <- data$real$flights[airline %in% mostfucked & as.Date(takeoff) >= day1 & as.Date(takeoff) <= day2,
								.N, by = .(as.Date(takeoff), airline)]
fuckedplot <- reshape(fuckedplot, timevar = 'airline', idvar = 'as.Date', direction = 'wide')
fuckedplot[is.na(fuckedplot)] = 0 # deal with <NA> values
setnames(fuckedplot, c('date', mostfucked))
