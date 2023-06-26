#' Summary of `sim_data` object
#'
#' @param object `sim_data` object; returned by [`initialise`] function
#' @param ... further arguments passed to or from other methods;
#' currently none specified
#'
#' @return `summary.sim_data` object
#'
#' @export
#'
#' @method summary sim_data
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
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#' summary(sim_data)
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {SP2.3} load data in spatial formats
#'
summary.sim_data <- function(object, ...) {

  # prepare summaries
  n1_sm <- summary(as.numeric(object$n1_map))
  K_sm <- summary(as.numeric(as.matrix(object$K_map)))

  names <- c(
    "growth", "r", "A", "kernel_fun", "dens_dep", "border",
    "max_dist", "changing_env"
  )
  values <- object[names]
  values$dlist <- ifelse(is.null(object$dlist), FALSE, TRUE)
  values$A <- ifelse(is.na(values$A), "-", values$A)
  params_sm <- data.frame(unlist(values))
  colnames(params_sm) <- NULL


  # print summaries
  output <- list()

  output$n1_map_summary <- n1_sm
  if (!object$changing_env) {
    output$K_map_summary <- K_sm
  } else {
    output$K_maps_summary <- K_sm
  }

  output$params_summary <- params_sm

  class(output) <- "summary.sim_data"


  return(output)
}


#' Print `summary.sim_data` object
#'
#' @param x `summary.sim_data` object; returned by [`summary.sim_data`] function
#' @param ... further arguments passed to or from other methods;
#' currently none specified
#'
#' @return None
#'
#' @export
#' @method print summary.sim_data
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
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#' summary_sim_data <- summary(sim_data)
#' print(summary_sim_data)
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {SP2.3} load data in spatial formats
#'
print.summary.sim_data <- function(x, ...) {

  cat("Summary of sim_data object\n\n")
  cat("n1 map summary: \n")
  print(x$n1_map_summary)

  if (!is.null(x$K_map_summary)) {
    cat("\nCarrying capacity map summary: \n")
    print(x$K_map_summary)
  } else {
    cat("\nCarrying capacity maps summary (all maps): \n",
      sep = ""
    )
    print(x$K_maps_summary)
  }

  print(x$params_summary)
}
