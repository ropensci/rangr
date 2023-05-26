#' Print `sim_data` object
#'
#' @param x `sim_data` object; returned by the [`initialise`] function
#' @param ... further arguments passed to or from other methods;
#' currently none specified
#'
#' @returns `sim_data` object is invisibly returned (the `x` param)
#'
#' @export
#'
#' @method print sim_data
#'
#' @examples
#'
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
#' print(sim_data)
#'
print.sim_data <- function(x, ...) {

  # prepare output
  n1_sm <- summary(as.numeric(x$n1_map))
  K_sm <- x$K_map

  names <- names(x)
  names <- names[(names %in% c(
    "resolution", "r", "r_sd", "K_sd", "growth", "A",
    "dens_dep", "border", "max_dist", "kernel_fun"
  ))]

  values <- x[names]
  values$A <- ifelse(is.na(values$A), "-", values$A)
  values$dlist <- ifelse(is.null(x$dlist), FALSE, TRUE)
  params_sm <- data.frame(unlist(values))
  colnames(params_sm) <- NULL

  # print output
  cat("Class: sim_data\n\n")

  cat("n1_map:\n")
  print(n1_sm)

  cat("\nK_map:\n")
  print(K_sm)

  print(params_sm)

  invisible(x)
}
