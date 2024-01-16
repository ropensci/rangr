#' Transformation `sim_results` To Raster
#'
#' This function transforms selected subset of abundance matrices from
#' `sim_results` into [`SpatRaster`][terra::SpatRaster-class] object. Layers are
#' specified by `time_points` that can be one point in time or many.
#'
#' @param sim_results `sim_results` object created by [`sim`]
#' @param time_points numeric vector of length 1 or more; specifies points in
#' time from which [`SpatRaster`][terra::SpatRaster-class] will be created
#' - as default the last year of simulation; if `length(time_points) > 0`
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
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, SP2.6} documented types expectation
#' @srrstats {SP2.0a} conversion to [`SpatRaster`][terra::SpatRaster-class]
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP4.0, SP4.0b} returns [`SpatRaster`][terra::SpatRaster-class]
#' object
#' @srrstats {SP4.1} returned object has the same unit as the input
#' (if the template is provided)
#' @srrstats {SP4.2} returned values are documented
#'
to_rast <- function(
    sim_results, time_points = sim_results$simulated_time, template = NULL) {

  #' @srrstats {SP2.7} validate input class
  assert_that(inherits(sim_results, "sim_results"))

  if(is.null(template)) {

    #' @srrstats {G2.9} make default raster and show warning

    warning("No template provided. Returned SpatRaster lacks geographical information (you can use one of the input maps from the sim_data object as template)") #nolint
    out <- rast(sim_results$N_map[, , time_points])

  } else {

    out <- template
    nlyr(out) <- length(time_points)
    values(out) <- sim_results$N_map[, , time_points]

  }

  names(out) <- paste("t", time_points, sep = "_")

  return(out)
}

