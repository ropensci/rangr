#' Generic conversion to SpatRaster
#'
#' A generic method to convert simulation result objects into
#' [`SpatRaster`][terra::SpatRaster-class] format.
#'
#' @param obj An object to convert.
#' @param ... Additional arguments passed to methods.
#'
#' @return A [`SpatRaster`][terra::SpatRaster-class] or a list of such objects.
#' @export
#'
#' @seealso [`to_rast.sim_results()`]
#'
#' @examples
#' \dontrun{
#' to_rast(sim_results_object)
#' }
to_rast <- function(obj, ...) {
  UseMethod("to_rast")
}


#' Convert `sim_results` To SpatRaster
#'
#' Converts selected subset of abundance matrices from
#' `sim_results` into [`SpatRaster`][terra::SpatRaster-class] object. Layers are
#' specified by `time_points`, which can be one or multiple points in time.
#'
#'
#'
#' @param obj `sim_results` object created by [`sim`]
#' @param time_points numeric vector of length 1 or more; specifies points in
#' time from which [`SpatRaster`][terra::SpatRaster-class] will be created - as
#' default the last year of simulation; if `length(time_points) > 0`
#' [`SpatRaster`][terra::SpatRaster-class] will be returned with layers for
#' each element of `time_points`
#' @param template [`SpatRaster`][terra::SpatRaster-class] object; can be used
#' as a template to create returned object
#' @param ... Currently unused.
#'
#' @return [`SpatRaster`][terra::SpatRaster-class] based on `sim_results` object
#' with layers corresponding to `time_points`.
#' @export
#'
#' @method to_rast sim_results
#'
#' @references Hijmans R (2024). terra: Spatial Data Analysis. R package version
#' 1.7-81, \url{https://rspatial.github.io/terra/}, \url{https://rspatial.org/}
#'
#' @examples
#' \donttest{
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
to_rast.sim_results <- function(
    obj, time_points = obj$simulated_time, template = NULL, ...) {

  #' @srrstats {SP2.7} validate input class
  assert_that(inherits(obj, "sim_results"))

  if(is.null(template)) {
    # if no template provide - make raster only with values

    #' @srrstats {G2.9} make default raster and show warning

    warning("No template provided. Returned SpatRaster lacks geographical information (you can use one of the input maps from the sim_data object as template)") #nolint
    out <- rast(obj$N_map[, , time_points])

  } else {
    # if template provided

    # check if template and x have the same dimension
    if (!all(dim(obj$N_map)[c(1, 2)] == dim(template)[c(1, 2)])) {
      stop("sim_resulst and template are not compatible with each other - dimensions of the study area do not match")
    }


    # make raster bases on the template
    out <- unwrap(template)
    nlyr(out) <- length(time_points)
    values(out) <- obj$N_map[, , time_points]

  }

  # name layers
  names(out) <- paste("t", time_points, sep = "_")

  return(out)
}

