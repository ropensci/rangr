#' Prepare time-varying carrying capacity maps
#'
#' This function linearly interpolates values in a series of carrying
#' capacity maps.
#'
#'
#' To simulate dynamic environmental scenarios (i.e. climate change, land use
#' change, ecological disturbance, etc.) one needs to provide time-varying
#' carrying capacity maps.
#'
#' Either `K_time_points` or `time` parameter are needed to calculate
#' interpolation. If interpolation should  be calculated between two carrying
#' capacity maps, there is no need to pass the time points, because 1 will
#' be set as a starting time point and `time` will be used as the ending point.
#' On the other hand, in the absence of the `time` argument, the last element of
#' `K_time_points` is considered to be the ending point for the interpolation.
#'
#'
#'
#' @param K_map [`SpatRaster`][terra::SpatRaster-class] object with
#' carrying capacity maps for each `K_time_points`
#' @param K_time_points vector of integers; time for each layer in `K_map`
#' @param time integer; number of total time steps required (this is defined
#' when evoking the function `sim`).
#'
#' @return [`SpatRaster`][terra::SpatRaster-class] object with number of layers
#' equal to `time`
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
K_get_interpolation <- function(K_map, K_time_points = NULL, time = NULL) {

  # arguments validation
  check_results <- K_check(K_map, K_time_points, time)

  K_interpolated <- K_interpolate(
    K_map,
    check_results$K_time_points,
    check_results$time
    )

  return(K_interpolated)
}




# Internal functions for K_get_interpolation function --------------------------

#' Validates K_map and time points
#'
#' This internal function validates the arguments passed to
#' the `K_get_interpolation` function and assigns default values if needed.
#'
#'
#' @inheritParams K_get_interpolation
#' @return List with two elements corresponding to `K_time_points`
#' and `time` respectively.
#'
#' @noRd
#'
K_check <- function(K_map, K_time_points, time) {

  if (!inherits(K_map, "SpatRaster")) {
    stop("K_map should be SpatRaster class")
  }

  if (is.null(time) & is.null(K_time_points)) {
    stop(
      "Either \"K_time_points\" or \"time\" must be specified ",
      "to perform interpolation"
    )
  }

  # number of layers
  nls <- nlyr(K_map)

  # number of time points
  ntp <- ifelse(is.null(K_time_points),
    0,
    ifelse(!is.numeric(K_time_points),
      stop("Time points must be of class \"numeric\""),
      length(K_time_points)
    )
  )


  # check time_points and number of layers
  if (nls != ntp) { # number of layers and number of time points are different

    if (!(nls %in% c(1, 2) & ntp == 0)) {
      stop(("Incorrect number of elements in \"K_time_points\""))
    }
  } else { # number of layers and number of time points are equal

    if (K_time_points[1] != 1) {
      stop("First element of \"K_time_points\" should be equal to 1")
    }

    if (!all(K_time_points > 0)) {
      stop("Elements of \"K_time_points\" must be positive numbers")
    }

    if (!all(K_time_points %% 1 == 0)) {
      stop("Elements of \"K_time_points\" be whole numbers")
    }
  }


  # set default value for time if possible
  if (is.null(time)) {
    warning(
      "Argument \"time\" is no specified - ",
      "last number from \"K_time_points\" is used as \"time\""
    )
    time <- K_time_points[ntp]
  } else if (ntp == 0 & nls == 2) {
    K_time_points <- c(1, time) # default time points for 2-layered K_map
  } else if (K_time_points[ntp] != time) {
    stop("Last element of \"K_time_points\" should be equal to \"time\"")
  }

  return(list(K_time_points = K_time_points, time = time))
}



#' Create interpolated carrying capacity maps
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
#' @noRd
#'
K_interpolate <- function(K_map, K_time_points, time) {

  K_interpolated <- app(K_map, function(cell) {

    na_cell <- rep(NA, time)

    if (all(is.na(cell))) {

      return(na_cell)

    } else {

      na_cell[K_time_points] <- cell
      na.approx(na_cell, xout = seq_len(time))

    }
  })

  return(K_interpolated)
}
