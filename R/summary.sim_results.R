#' Summary Of `sim_results` Object
#'
#' @param object `sim_results` object; returned by [`sim`] function
#' @param ... further arguments passed to or from other methods; none specified
#'
#' @return `summary.sim_results` object
#'
#' @export
#'
#' @method summary sim_results
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
#'
#' # simulation
#' sim_results <- sim(sim_data, time = 10)
#' summary(sim_results)
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {SP2.3} load data in spatial formats
summary.sim_results <- function(object, ...) {

  # prepare summaries
  values <- list(object$simulated_time, as.character(object$extinction))
  params_sm <- data.frame(unlist(values))
  colnames(params_sm) <- NULL
  rownames(params_sm) <- c("simulated time", "extinction")

  N_sm <- summary(object$N_map)


  # print summaries
  output <- list()

  output$params_summary <- params_sm
  output$abundance_summary <- N_sm

  class(output) <- "summary.sim_results"


  ## plot
  if (object$simulated_time > 1) {
    means <- apply(object$N_map, 3, mean, na.rm = TRUE)
    plot(1:object$simulated_time, means,
      type = "l",
      main = "Mean abundance from each time step",
      xlab = "Time step",
      ylab = "Mean abundance"
    )
    abline(h = mean(means), col = "red", lty = 2)
  }

  return(output)
}



#' Print `summary.sim_results` Object
#'
#' @param x `summary.sim_results` object; returned by
#' [`summary.sim_results`] function
#' @param ... further arguments passed to or from other methods;
#'            currently none specified
#'
#' @return None
#'
#' @export
#' @method print summary.sim_results
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
#' sim_results <- sim(sim_data, time = 10)
#' summary_sim_results <- summary(sim_results)
#' print(summary_sim_results)
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {SP2.3} load data in spatial formats
print.summary.sim_results <- function(x, ...) {

  cat("Summary of sim_results object\n\n")

  cat("Simulation summary: \n")
  print(x$params_summary)

  cat("\nAbundances summary: \n")
  print(x$abundance_summary)
}
