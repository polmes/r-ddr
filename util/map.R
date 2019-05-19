ddr.map <- function(routes, autocenter = TRUE) {
	map <- ggplot(map_data('world'), aes(x = long, y = lat, group = group)) +
		geom_polygon(fill = 'white', color = 'black', size = 0.2) +
		geom_path(data = routes,
				  aes(x = lon, y = lat, group = id, color = colors(distinct = TRUE)[id %% 502 + 1]),
				  lwd = 0.25) +
		theme(legend.position = 'none',
			axis.line = element_blank(), axis.ticks = element_blank(),
			axis.text.x = element_blank(), axis.text.y = element_blank(),
			axis.title.x = element_blank(), axis.title.y = element_blank(),
			panel.border = element_rect(fill = NA, color = 'black')) +
		if (autocenter) {
			coord_map('ortho',
					  orientation = c(median(routes[, lat], na.rm = TRUE), median(routes[, lon], na.rm = TRUE), 0),
					  xlim = c(min(routes[, lon], na.rm = TRUE) - 2, max(routes[, lon], na.rm = TRUE) + 2),
					  ylim = c(min(routes[, lat], na.rm = TRUE) - 2, max(routes[, lat], na.rm = TRUE) + 2))
		} else {
			coord_map('ortho',
					  orientation = c(50.8, 4.4, 0), # Brussels
					  xlim = c(-20, 50), ylim = c(32, 70)) # Europe
		}

	# print(map)
	return(map)
}
