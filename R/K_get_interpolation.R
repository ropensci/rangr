#' Prepare Time-Varying Carrying Capacity Maps
#'
#' This function linearly interpolates values in a series of carrying
#' capacity maps.
#'
#'
#' To simulate dynamic environmental scenarios (e.g. climate change, land use
#' change, ecological disturbance, etc.) one needs to provide time-varying
#' carrying capacity maps.
#'
#' Either `K_time_points` or the `time` parameter is needed to perform
#' interpolation. If the interpolation should be calculated between two carrying
#' capacity maps, there is no need to pass the time points, because 1 will
#' be set as the starting time point and `time` will be used as the ending point.
#' On the other hand, in the absence of the `time` argument, the maximum element
#' of `K_time_points` is considered to be the ending point for the interpolation.
#'
#'
#'
#' @param K_map [`SpatRaster`][terra::SpatRaster-class] object with
#' carrying capacity maps for each `K_time_points`
#' @param K_time_points integer vector; time for each layer in `K_map`
#' @param time integer vector of length 1; number of total time steps required
#' (this is defined when evoking the function `sim`).
#'
#' @return [`SpatRaster`][terra::SpatRaster-class] object with number of layers
#' equal to `time`.
#'
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' # data preparation
#' library(terra)
#'
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
#' package = "rangr"))
#'
#' K_interpolated_01 <- K_get_interpolation(
#'   K_small_changing,
#'   K_time_points = c(1, 10, 15)
#' )
#'
#' K_two_layers <- subset(
#'   K_small_changing,
#'   c(1, 2)
#' )
#' K_interpolated_02 <- K_get_interpolation(
#'   K_two_layers,
#'   time = 15
#' )
#'
#' }
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, SP2.6} documented types expectation
#' @srrstats {SP1.1} documented dimensional domain of applicability
#' @srrstats {SP2.0, SP2.0b} check if K_map is
#' [`SpatRaster`][terra::SpatRaster-class] and error otherwise
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP4.0, SP4.0a} returns object of the same class as the input
#' @srrstats {SP4.1} returned object has the same unit as the input
#' @srrstats {SP4.2} returned values are documented
#'
K_get_interpolation <- function(K_map, K_time_points = NULL, time = NULL) {

  # Validation of arguments
  check_results <- K_check(K_map, K_time_points, time)

  # interpolation
  K_interpolated <- K_interpolate(
    K_map,
    check_results$K_time_points,
    check_results$time
    )

  return(K_interpolated)
}




# Internal functions for K_get_interpolation function --------------------------

#' Validates K_map And K_time_points
#'
#' This internal function validates the arguments passed to
#' the `K_get_interpolation` function and assigns default values if needed.
#'
#'
#' @inheritParams K_get_interpolation
#' @return List with two elements corresponding to `K_time_points`
#' and `time` respectively.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
K_check <- function(K_map, K_time_points, time) {


  #' @srrstats {G2.1, G2.6, SP2.7} assert input type
  assert_that(inherits(K_map, "SpatRaster"))

  assert_that(
    nlyr(K_map) > 1,
    msg = "K_map has only one layer - nothing to interpolate")

  assert_that(
    !is.null(time) || (!is.null(K_time_points)),
    msg = "Either \"K_time_points\" or \"time\" must be specified")

  assert_that(
    !any(duplicated(K_time_points)),
    msg = "\"K_time_points\" should contain only unique values")

  # get number of layers
  nls <- nlyr(K_map)

  # get number of time points
  ntp <- ifelse(is.null(K_time_points),
    0,
    ifelse(!is.numeric(K_time_points),
      stop("Time points must be of class \"numeric\""),
      length(K_time_points)
    )
  )

  #' @srrstats {G2.0, G2.2, G2.13} assert input length

  # check if time_points and number of layers are compatible
  if (nls != ntp) {
    # number of layers and number of time points are different

    # acceptable only if number time point are not provided
    # and K_map has 2 layers - default time point are: 1 and time
    assert_that(
      nls == 2 & ntp == 0,
      msg = "Incorrect number of elements in \"K_time_points\"")

  } else {
    # number of layers and number of time points are equal

    assert_that(
      K_time_points[1] == 1,
      msg = "First element of \"K_time_points\" should be equal to 1")

    assert_that(
      all(K_time_points > 0),
      msg = "Elements of \"K_time_points\" must be positive integers")


    assert_that(all(K_time_points %% 1 == 0),
      msg = "Elements of \"K_time_points\" must be integers")

  }


  # check time
  if (is.null(time)) {
    # set default value for time if necessary

    #' @srrstats {G2.9} add default time and show warning

    warning(
      "Argument \"time\" is no specified - ",
      "last number from \"K_time_points\" is used as \"time\""
    )
    time <- max(K_time_points)

  } else {
    # check provided time

    #' @srrstats {G2.0, G2.2} assert input length
    #' @srrstats {G2.1, G2.3, G2.3a, G2.6} assert input type

    assert_that(length(time) == 1)
    assert_that(is.numeric(time))

    if (nls == 2 & ntp == 0) {

      # default time points for 2-layered K_map
      K_time_points <- c(1, time)

    } else {

      assert_that(
        max(K_time_points) == time,
        msg = "Last element of \"K_time_points\" should be equal to \"time\"")
    }
  }

  return(list(K_time_points = K_time_points, time = time))
}



#' Create Interpolated Carrying Capacity Maps
#'
#' This internal function transforms carrying capacity maps from
#' [`SpatRaster`][terra::SpatRaster-class] with number of layers equal
#' to number of time points given in `K_time_points` to
#' [`SpatRaster`][terra::SpatRaster-class] where each layer corresponds to
#' one time step of the simulation. Intermediate carrying capacities are
#' calculated using [linear interpolation][zoo::na.approx].
#'
#'
#' @inheritParams K_get_interpolation
#'
#' @return [`SpatRaster`][terra::SpatRaster-class] with as many layers as
#' specified in `time`
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
K_interpolate <- function(K_map, K_time_points, time) {

  # for every grid cell (across all layers)
  K_interpolated <- app(K_map, function(cell) {

    # vector for interpolated values filled with NAs
    na_cell <- rep(NA, time)

    if (all(is.na(cell))) {

      # if no values in current cell - return NAs
      return(na_cell)

    } else {

      # assign known values to specified time point
      na_cell[K_time_points] <- cell

      # interpolate values for other time points
      na.approx(na_cell, xout = seq_len(time))

    }
  })

  return(K_interpolated)
}
