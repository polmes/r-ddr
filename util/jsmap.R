ddr.jsmap <- function(routes, info, coord) {
	map <- leaflet(width = '100%') %>%
		addTiles() %>%
		addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
		# addProviderTiles(providers$CartoDB.Positron) %>%
		# addProviderTiles(providers$CartoDB.PositronNoLabels) %>%

		# addMarkers(lat = ~lat, lng = ~lon)
		addAwesomeMarkers(lat = coord[1], lng = coord[2], icon = awesomeIcons(icon = 'plane',
																			  iconColor = '#ffffff',
																			  library = 'fa',
																			  markerColor = 'red')) %>%
		flyTo(lat = coord[1], lng = coord[2], zoom = 4)

	# addPolylines(data = routes, lat = ~lat, lng = ~lon, group = ~id)
	for (idx in unique(routes[, id])) {
		infox <- info[id == idx]
		map <- map %>%
			addPolylines(data = routes[id == idx], lat = ~lat, lng = ~lon,
						 color = paste0('#', paste(as.hexmode(c(col2rgb(colors(distinct = TRUE)[idx %% 502 + 1]))),
						 						   collapse = '')),
						 weight = 2, opacity = 1,
						 highlightOptions = highlightOptions(weight = 5),
						 label = HTML(paste0('<strong>', infox[1, orig], ' &ndash; ', infox[1, dest], '</strong>',
						 					 '<br>', infox[1, airline])))
	}

	# print(map)
	return(map)
}
