#' Transformation sim_results to raster
#'
#' This function transforms selected subset of abundance matrices from
#' `sim_results` into [`SpatRaster`][terra::SpatRaster-class] object. Layers are
#' specified by `time_points` that can be one point in time or many.
#'
#' @param sim_results `sim_results` object created by [`sim`]
#' @param time_points numeric or numeric vector; specifies points in time from
#' which [`SpatRaster`][terra::SpatRaster-class] will be created - as default
#' the last year of simulation; if `length(time_points) > 0`
#' [`SpatRaster`][terra::SpatRaster-class] will be returned with layers for
#' each element of `time_points`
#' @param template [`SpatRaster`][terra::SpatRaster-class] object; can be used
#' as template to create returned object
#'
#' @return [`SpatRaster`][terra::SpatRaster-class] based on `sim_results` object
#' with layers corresponding to `time_points`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # data preparation
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
#'
#' # simulation
#' sim_1 <- sim(obj = sim_data, time = 100)
#'
#' # raster construction
#' my_rast <- to_rast(
#'   sim_1,
#'   time_points = c(1, 10, 20, 100),
#'   template = sim_data$K_map
#' )
#'
#' # visualization
#' plot(my_rast, range = range(sim_1$N_map, na.rm = TRUE))
#'
#' }
#'
to_rast <- function(
    sim_results, time_points = sim_results$simulated_time, template = NULL) {


  if(is.null(template)) {

    out <- rast(sim_results$N_map[, , time_points])

  } else {

    out <- template
    nlyr(out) <- length(time_points)
    values(out) <- sim_results$N_map[, , time_points]

  }

  names(out) <- paste("t", time_points, sep = "_")

  return(out)
}

