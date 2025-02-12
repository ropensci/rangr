#' Print `sim_results` Object
#'
#' @param x `sim_results` object; returned by the [`sim`] function
#' @param ... further arguments passed to or from other methods; none specified
#'
#' @returns `sim_results` object is invisibly returned (the `x` param)
#'
#' @export
#'
#' @method print sim_results
#'
#' @examples
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
#' sim_res <- sim(obj = sim_data, time = 20, burn = 5)
#' print(sim_res)
#'
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {SP2.3} load data in spatial formats
#'
print.sim_results <- function(x, ...) {

  # prepare output
  values <- list(x$simulated_time, as.character(x$extinction))
  params_sm <- data.frame(unlist(values))
  colnames(params_sm) <- NULL
  rownames(params_sm) <- c("simulated time steps", "extinction")

  # print output
  cat("Class: sim_results\n\n")

  cat("N_map: \n")
  print(x$N_map)

  print(params_sm)

  invisible(x)
}
