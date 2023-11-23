#' Mechanistic metapopulation simulator
#'
#' This function simulates population growth and dispersal providing a given
#' environmental scenario. All parameters for the simulation must be set
#' in advance using [`initialise`].
#'
#' This is the main simulation module. It takes the `sim_data` object prepared
#' by [`initialise`] and runs simulation for a given number of time steps.
#' The initial (specified by the `burn` parameter) time steps are skipped
#' and discarded from the output. Computations can be done in parallel if
#' the name of a cluster created by [`makeCluster`][parallel::makeCluster()]
#' is provided.
#'
#' Generally, at each time step, simulation consists of two phases: local
#' dynamics and dispersal.
#'
#' Local dynamics (which connects habitat patches in time) is defined by
#' the function [`growth`][growth]. This parameter is specified while creating
#' the `obj` using [`initialise`], but can be later modified by using
#' the [`update`][update] function. Population growth can be either exponential
#' or density-dependent, and the regulation is implemented by the use of
#' Gompertz or Ricker models (with a possibility of providing any other,
#' user defined function). For every cell, the expected population density
#' during the next time step is calculated from the corresponding number
#' of individuals currently present in this cell, and the actual number
#' of individuals is set by drawing a random number from the Poisson
#' distribution using this expected value. This procedure introduces a realistic
#' randomness, however additional levels of random variability can be
#' incorporated by providing parameters of both demographic and environmental
#' stochasticity while specifying the `sim_data` object using the [`initialise`]
#' function (parameters `r_sd` and `K_sd`, respectively).
#'
#' To simulate dispersal (which connects habitat patches in space), for each
#' individual in a given cell, a dispersal distance is randomly drawn from
#' the dispersal kernel density function. Then, each individual is translocated
#' to a randomly chosen cell at this distance apart from the current location.
#' For more details, see the [`disp`] function.
#'
#'
#'
#' @param obj `sim_data` object created by [`initialise`] containing all
#' simulation parameters and necessary data
#' @param time positive integer vector of length 1; number of time steps
#' simulated
#' @param burn positive integer vector of length 1; the number of burn-in time
#' steps that are
#' discarded from the output
#' @param cl an optional cluster object created by
#' [`makeCluster`][parallel::makeCluster()] needed for parallel calculations
#' @param progress_bar logical vector of length 1 determines if progress bar
#' for simulation
#' should be displayed
#' @param quiet logical vector of length 1; determines if warnings should
#' be displayed
#'
#' @return This function returns an object of class `sim_results` which is
#' a list containing the following components:
#' \itemize{
#'   \item{`extinction` - `TRUE` if population is extinct or `FALSE` otherwise}
#'   \item{`simulated_time` - number of simulated time steps without
#'   the burn-in ones}
#'   \item{`N_map` - 3-dimensional array representing spatio-temporal
#'   variability in population numbers. The first two dimensions correspond to
#'   the spatial aspect of the output and the third dimension represents time.}
#' }
#'
#' In case of a total extinction, a simulation is stopped before reaching
#' the specified no. of time steps. If the population died out before reaching
#' the `burn` threshold, then nothing can be returned and an error occurs.
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
#' # simulation with burned time steps and progress bar
#' sim_2 <- sim(obj = sim_data, time = 100, burn = 20, progress_bar = TRUE)
#'
#' # example with parallelization
#' library(parallel)
#' cl <- makeCluster(detectCores())
#'
#' # parallelized simulation
#' sim_3 <- sim(obj = sim_data, time = 100, cl = cl, progress_bar = TRUE)
#' stopCluster(cl)
#'
#'
#' # visualisation
#' plot(n1_small)
#'
#' plot(to_rast(
#'   sim_1,
#'   time_points = 100,
#'   template = sim_data$K_map
#' ))
#'
#' plot(to_rast(
#'   sim_1,
#'   time_points = c(1, 10, 50, 100),
#'   template = sim_data$K_map
#' ), range = range(sim_1$N_map, na.rm = TRUE))
#'
#' }
#'
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, SP2.6} documented types expectation
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP4.0, SP4.0b} returns sim_results object
#' @srrstats {SP4.1} returned object has the same unit as the input
#' @srrstats {SP4.2} returned values are documented
#'
sim <- function(
    obj, time, burn = 0, cl = NULL, progress_bar = FALSE, quiet = TRUE) {

  #' @srrstats {G2.0, G2.2} assert input length
  #' @srrstats {G2.1, G2.6, SP2.7} assert input type
  # Validation of arguments
  ## obj
  assert_that(inherits(obj, "sim_data"))

  ## time
  assert_that(length(time) == 1)
  assert_that(is.numeric(time))
  assert_that(time > 1)

  ## burn
  assert_that(length(burn) == 1)
  assert_that(is.numeric(burn))
  assert_that(burn >= 0)

  ## progress_bar
  assert_that(length(progress_bar) == 1)
  assert_that(is.logical(progress_bar))

  ## quiet
  assert_that(length(quiet) == 1)
  assert_that(is.logical(quiet))


  # options
  options(warn = -1)

  # Extract data from the sim_data object
  K_map <- obj$K_map # carrying capacity
  K_sd <- obj$K_sd # sd of carrying capacity (additional cell specific variation) #nolint
  dynamics <- obj$dynamics # population growth function
  n1_map <- obj$n1_map # population numbers at the first time step
  r <- obj$r # intrinsic population growth rate
  r_sd <- obj$r_sd # sd of intrinsic growth rate (time specific variation)
  A <- obj$A # Allee effect coefficient
  id <- obj$id # square identifiers
  ncells <- obj$ncells # number of cells in the study area
  data_table <- obj$data_table
  changing_env <- obj$changing_env
  resolution <- obj$resolution


  # Specify other necessary data
  ## additional demographic stochasticity (time specific)
  r <- rnorm(time, r, r_sd)

  if (changing_env) {
    if (nlyr(K_map) != time) {
      stop("Number of layers in \"K_map\" and \"time\"  are not equal ")
    }
  } else {
    K_map <- as.matrix(K_map, wide = TRUE)
  }


  # Empty data structures to store simulation outputs
  mu <- array(data = 0, dim = c(nrow(id), ncol(id), time)) # expectations
  N <- array(data = 0L, dim = c(nrow(id), ncol(id), time)) # numbers


  # Matrix of population numbers at t = 1
  N[, , 1] <- n1_map


  # Loop through time

  if (progress_bar) {
    pb <- txtProgressBar(min = 1, max = time, style = 3)
    setTxtProgressBar(pb, 1)
  }

  for (t in 2:(time)) {

    # 1. Local dynamics

    # Carrying capacity for t
    K <- get_K(K_map, t, changing_env)

    # Demographic processes
    mu[, , t - 1] <- dynamics(N[, , t - 1], r[t - 1], K, A)

    # demographic stochasticity (random numbers drown from a Poisson distribution) #nolint
    # of no. of individuals in each square predicted by the deterministic model)
    N[, , t] <- rpois(ncells, mu[, , t - 1])


    # check for extinction
    if (extinction_status <- extinction_check(N, t)) {
      if (t < burn) {
        stop(
          "Simulation failed to reach specified time steps treshold ",
          "(by \"burn\" parameter) - nothing to return."
        )
      }

      break
    }



    # update data
    data_table[, "K"] <- as.numeric(K)
    data_table[, "N"] <- as.numeric(as.matrix(N[, , t]))


    # 2. Dispersal

    m <- disp(
      N_t = N[, , t], id = id,
      data_table = data_table, kernel = obj$kernel,
      dens_dep = obj$dens_dep, dlist = obj$dlist, id_within = obj$id_within,
      within_mask = obj$within_mask, border = obj$border,
      max_dist = obj$max_dist, resolution = resolution,
      ncells_in_circle = obj$ncells_in_circle, cl = cl
    )

    # net effect (population numbers - emigration + immigration)
    N[, , t] <- N[, , t] - m[["em"]] + m[["im"]]

    if (progress_bar) setTxtProgressBar(pb, t)
  }
  if (progress_bar) close(pb)


  out <- list(
    extinction = extinction_status,
    simulated_time = t - burn,
    N_map = N[, , (burn + 1):t]
  )


  class(out) <- c("sim_results", class(out))


  if (extinction_status && !quiet) {
    ext_time_total <- paste0(out$simulated_time, "/", time)
    ext_time_after_burn <- paste0(
      "(including burned: ", t, "/",
      time, ")"
    )

    message(paste0(
      "Population extinct after ", ext_time_total,
      " time steps ", ext_time_after_burn, "\n"
    ))
  }


  return(out)
}



# Internal functions for sim function ------------------------------------------


#' Get K_matrix
#'
#' This internal function extracts carrying capacity map for particular time
#' and applies environmental stochasticity (if specified).
#'
#'
#' `K_map` is a matrix if environment isn't changing during the simulation
#' (`changing_env = FALSE`). Otherwise `K_map` is
#' an [`SpatRaster`][terra::SpatRaster-class] object.
#'
#' Environmental stochasticity is applied if `K_sd > 0`
#' using [`rlnorm`][stats::rlnorm()].
#'
#' @param K_map numeric matrix or [`SpatRaster`][terra::SpatRaster-class]
#' object; see 'Details' for more information
#' @param t integer vector of length 1; positive integer value that represent
#' the number of current time step
#' @param changing_env logical vector of length 1; indicates if environment
#' is static or changing during the simulation
#'
#' @return Matrix with carrying capacity for the time steps specified
#' by `t` parameter.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
get_K <- function(K_map, t, changing_env) {

  # get current K map
  if (changing_env) {
    K <- as.matrix(subset(K_map, t), wide = TRUE)
  } else {
    K <- K_map
  }

  return(K)
}



#' Check if population is extinct
#'
#' This internal function checks if any individuals are present in the
#' simulation. The `t` parameter is the current time step in which extinction
#' should be checked.
#'
#'
#' @param N 3-dimensional integer array; first two dimensions corresponds
#' to space, the third one is time and the values represent the number
#' of specimens
#' @param t integer vector of length 1; positive integer value that represents
#' time step
#'
#' @return `TRUE` if population is extinct or `FALSE` otherwise
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
extinction_check <- function(N, t) {

  if (sum(N[, , t], na.rm = TRUE) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
