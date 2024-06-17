#' Population Growth Functions
#'
#' Population growth functions are used during simulation
#' conducted by the [`sim`] function.
#' The user is required  to specify the name of a growth function while initialising the
#' `sim_data` object using [`initialise`].
#'
#'`x` can be a vector, matrix, [`SpatRaster`][terra::SpatRaster-class]
#' or any other `R` object for which basic arithmetic operations produce valid results.
#' These functions are intended to be used in the [`sim`] function, where `x`
#' is a matrix of the same dimensions as the [`SpatRaster`][terra::SpatRaster-class]
#' object specified in `n1_map` parameter.
#'
#' @name growth
#'
#' @param x number of individuals
#' @param r intrinsic population growth rate
#' @param K carrying capacity
#' @param A coefficient of Allee effect (A <= 0: weak, A > 0: strong, NA: none)
#' @param ... not used, added for compatibility reasons
#'
#' @srrstats {G1.0} Reference for Allee effect:
#' @references Boukal, D. S., & Berec, L. (2002). Single-species models
#' of the Allee effect: extinction boundaries, sex ratios and mate encounters.
#' Journal of Theoretical Biology, 218(3), 375-394.
#'  https://doi.org/10.1006/jtbi.2002.3084
#'
#' @return Object of the same dimensions as `x` that contains expected number
#' of individuals in the next time step.
#' @export
#'
#' @examples
#' x <- 1:10
#' exponential(x, r = 0.4)
#'
#' ricker(x, r = 2, K = 5)
#' ricker(x, r = 2, K = 5, A = -5)
#'
#' gompertz(x, r = 1.2, K = 5)
#' gompertz(x, r = 1.2, K = 5, A = 5)
#'
#' @srrstats {G1.4} uses roxygen documentation
#'
exponential <- function(x, r, ...) {

  x <- x * exp(r)
  x[is.nan(x)] <- 0

  return(x)
}


#' @rdname growth
#' @export
#'
ricker <- function(x, r, K, A = NA) {

  if (is.na(A)) {
    x <- x * exp(r * (1 - x / K))
  } else {
    x <- x * exp(r * (1 - x / K) * (x / K - A / K))
  }
  x[is.nan(x)] <- 0

  return(x)
}


#' @rdname growth
#' @export
#'
gompertz <- function(x, r, K, A = NA) {

  if (is.na(A)) {
    x <- x * exp(r * (1 - log1p(x) / log1p(K)))
  } else {
    x <- x * exp(r * (1 - log1p(x) / log1p(K)) * (x / K - A / K))
  }
  x[is.nan(x)] <- 0

  return(x)
}
