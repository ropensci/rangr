#' Plot `sim_results` Object
#'
#' @description Plots abundances obtained during simulation.
#'
#' @param x `sim_results` object; returned by [`sim`]
#' @param template [`SpatRaster`][terra::SpatRaster-class] object;
#' can be used as template to create returned object
#' @param time_points numeric vector; specifies points in time from which
#' plots will be generated
#' @param ... further arguments passed to [`terra::plot`]
#' @param range numeric vector of length 2; range of values to be used for the
#' legend (if `type = "continuous"`), which by default is calculated from
#' the N_map slot of `sim_result` object
#' @param type character vector of length 1; type of map:
#' "continuous" (default), "classes" or "interval"  (case-sensitive)
#'
#' @returns [`SpatRaster`][terra::SpatRaster-class] object with as many layers
#' as the length of `time_points` parameter
#'
#' @export
#' @method plot sim_results
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))
#'
#' sim_data <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#' sim_res <- sim(sim_data, time = 10)
#' plot(sim_res)
#'
#' # plot specific area
#' plot(sim_res, xlim = c(4, 10), ylim = c(0, 10))
#' plot(sim_res, ext = c(4, 10, 0, 10))
#' plot(sim_res, template = n1_small, ext = c(274000, 280000, 610000, 620000))
#' }
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, G2.3, G2.3b, SP2.6} documented types expectation
#' @srrstats {G2.13} check for missing data
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP5.0} `plot` method for sim_result
#'
plot.sim_results <- function(
    x, template = NULL, time_points = NULL, range, type, ...) {

  default_n_panels <- 4

  if (!is.null(time_points)) {
    if (x$simulated_time < time_points[length(x)]) {
      stop(
        "Invalid \"time_points\" argument: ",
        "some of its values exceed the simulation time."
      )
    }
  } else {
    if (x$simulated_time < default_n_panels) {
      warning(
        "There are not enough time points simulated to generate the default (",
        default_n_panels, ") number of maps. Ploting all available time points."
      )
      time_points <- seq(from = 1, to = x$simulated_time, by = 1)
    } else {
      time_points <- round(seq(from = 1, to = x$simulated_time, length = 4))
    }
  }


  if(missing(range)) {
    range <- base::range(x$N_map, na.rm = TRUE)
  }

  if(missing(type)) {
    type <- "continuous"
  }

  x_rast <- to_rast(
    sim_results = x,
    time_points = time_points,
    template = template
  )

  plot(x_rast, type = type, range = range, ...)

  return(x_rast)
}
