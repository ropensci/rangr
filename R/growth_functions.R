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
#'  \url{https://doi.org/10.1006/jtbi.2002.3084}
#'
#' Gompertz, B. (1825) On the Nature of the Function Expressive of the Law
#' of Human Mortality, and on a New Mode of Determining the Value of Life
#' Contigencies. Philosophical Transactions of the Royal Society of London,
#' 115, 513-583. \url{http://dx.doi.org/10.1098/rstl.1825.0026}
#'
#' Ricker, W.E. (1954) Stock and Recruitment. Journal of the Fisheries
#' Research Board of Canada, 11, 559-623. \url{http://dx.doi.org/10.1139/f54-039}
#'
#' Hostetler, J.A. and Chandler, R.B. (2015), Improved state-space models
#' for inference about spatial and temporal variation in abundance from
#' count data. Ecology, 96: 1713-1723. \url{https://doi.org/10.1890/14-1487.1}
#'
#' Courchamp, F., L. Berec and J. Gascoigne. 2008. Allee Effects in Ecology
#' and Conservation. Oxford University Press, New York. 256 pp. ISBN
#' 978-0-19-857030-1
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
