ddr.animate <- function(routes, filename, nf = 100, autocenter = TRUE) {
	map <- ggplot(map_data('world'), aes(x = long, y = lat, group = group)) +
		geom_polygon(fill = 'white', color = 'black', size = 0.2) +
		geom_point(data = routes,
				   aes(x = lon, y = lat, group = id, color = colors(distinct = TRUE)[id %% 502 + 1])) +
		transition_components(dt) +
		labs(title = 'Time: {frame_time}') +
		shadow_wake(wake_length = 0.02 * 100/nf) +
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
					  xlim = c(-2, 10), ylim = c(48, 54)) # Belgium
		}

	animate(map, width = 800, height = 600, detail = 5, nframes = nf)
	anim_save(filename)
}
