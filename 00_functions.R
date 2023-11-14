
plot_by <- function(d, id = NULL, add = NULL, by) {

  if(by == "logger") {
    l <- d |>
      group_by(logger_id) |>
      summarize(n = n(), date_min = min(time), date_max = max(time))
    
    g <- ggplot() +
      theme_bw() +
      geom_errorbar(data = l, aes(xmin = date_min, xmax = date_max, y = logger_id), width = 0.5, linewidth = 0.5) +
      scale_x_datetime(date_breaks = "3 days", date_minor_breaks = "1 day") +
      geom_point(data = filter(d, animal_id %in% id), aes(x = time, y = logger_id, colour = animal_id))
    
    if(!is.null(add) && add == "animals") {
      a <-  d |>
        group_by(animal_id, logger_id) |>
        summarize(n = n(), date_min = min(time), date_max = max(time))
    g <- g + geom_errorbar(data = a, aes(xmin = date_min, xmax = date_max, y = logger_id, colour = animal_id), 
                             linewidth = 2, alpha = 0.3)
    }
    
    if(!is.null(id)) g <- g + scale_colour_viridis_d(end = 0.8)
    
  } else if(by == "animal") {
    if(!is.null(id)) d1 <- filter(d, logger_id %in% id)
    g <- ggplot(data = d1, aes(y = time, x = animal_id, colour = logger_id)) +
      theme_bw() +
      geom_point(position = position_dodge(width = 0.1)) +
      scale_colour_viridis_d(end = 0.8) +
      coord_flip() +
      scale_y_datetime(date_breaks = "3 days", date_minor_breaks = "1 day")
  }
  g
}
