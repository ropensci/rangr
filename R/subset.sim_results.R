#' Subset of the given time points from `sim_results` object
#'
#' @param x `sim_results` object; returned by [`sim`] function
#' @param from numeric vector of length 1; indicates the starting time point from which all
#' time point should be kept
#' @param time_points numeric vector; indicates all time points to keep
#' @param ... further arguments to be passed to or from other methods
#'
#' @details
#' Either `from` or `time_points` argument has to be specified.
#' Time point passed by the `from` argument will be set as a cutoff point
#' and all abundances for previous time points will be discarded.
#'
#' @returns `sim_results` object with only selected `time_points` present
#'  in the `N_map` slot
#'
#' @export
#'
#' @method subset sim_results
#'
#' @examples
#'
#' # data preparation
#' library(terra)
#'
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))
#'
#' sim_data <- initialise(
#'   n = n1_small,
#'   r = log(2),
#'   K_map = K_small,
#'   max_dist = 1000,
#'   rate = 1 / 1e3
#' )
#'
#' sim_results <- sim(sim_data, time = 10)
#' summary(sim_results)
#'
#' sim_results_cropped <- subset(sim_results, time_points = c(1:2))
#' summary(sim_results_cropped)
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {SP2.3} load data in spatial formats
#'
subset.sim_results <- function(x, from = NULL, time_points = NULL, ...) {

  available_time_points <- ifelse(length(dim(x$N_map)) == 3, dim(x$N_map)[3], 1)

  if (is.null(from) & is.null(time_points)) {
    stop(
      "Subsetting can't be preformed without \"from\" ",
      "or \"time_points\" argument"
    )
  }

  # from arg validation
  if (!is.null(from)) {
    if (from <= 0) {
      stop("Invalid \"from\" argument: it can't be less than or equal to 0")
    }
    if (from > available_time_points) {
      stop(
        "Invalid \"from\" argument: it can't be greater",
        "than available number of time points"
      )
    }
  }

  # time_points arg validation
  if (!is.null(time_points)) {
    if (any(time_points <= 0)) {
      stop(
        "Invalid \"time_points\" argument: none of the values can be",
        "less than or equal to 0"
      )
    }
    if (any(time_points > available_time_points)) {
      stop(
        "Invalid \"time_points\" argument: none of the values can be",
        "greater than available number of time points"
      )
    }
  }

  # if anything to do
  if (length(time_points) == available_time_points) {
    stop("Nothing to subset")
  }


  if (is.null(time_points)) {
    time_points <- c(from:x$simulated_time)
  }


  x$N_map <- x$N_map[, , time_points]
  x$simulated_time <- length(time_points)

  return(x)
}
