#' Observation process
#'
#' This function simulates observation process.
#'
#' @param sim_data  `sim_data` object from [`initialise`] containing simulation
#' parameters
#' @param sim_results `sim_results` object; returned by [`sim`] function
#' @param type character vector of length 1; describes the sampling type (case-sensitive):
#' \itemize{
#'   \item "random_one_layer" - cells from which the population numbers
#'   will be sampled are selected randomly; selected cells will be the same
#'   for all time steps
#'   \item "random_all_layers" - cells from which the abundance will be sampled
#'   are selected randomly; a new set of cells will be selected
#'   for each time step
#'   \item "from_data" - cells for which abundance will be sampled are provided
#'   by the user in a `data.frame` with three columns: "x", "y" and "time_step"
#'   \item "monitoring_based" - cells from which abundance will be sampled
#'   are provided by the user in a matrix object with two columns: “x” and “y”;
#'   abundance from given cell is then sampled by different virtual observers
#'   in different time steps; whether a survey will be made by
#'   the same observer for several years and whether it will not be made at all
#'   is defined using a geometric distribution ([`rgeom`][stats::rgeom()])
#' }
#' @param sdlog numeric vector of length 1; standard deviation (on a log scale) of the random noise in
#' observation process generated from the log-normal distribution
#' ([`rlnorm`][stats::rlnorm()])
#' @param ... other necessary internal parameters:
#' \itemize{
#'   \item{`prop`
#'
#'   numeric vector of length 1; proportion of cells to be sampled (default `prop = 0.1`);
#'   used when `type = "random_one_layer" or "random_all_layers"`,}
#'
#'   \item{`points`
#'
#'   `data.frame` or `matrix` with 3 named numeric columns ("x", "y" and "time_step") containing
#'   coordinates and time steps from which observations should be obtained;
#'   used when `type = "from_data"`,}
#'
#'   \item{`cells_coords`
#'
#'   `data.frame` or `matrix` with 2 named columns: "x" and "y"; survey plots coordinates;
#'   used when `type = "monitoring_based"`}
#'
#'   \item{`prob`
#'
#'    numeric vector of length 1; a parameter defining the shape of - [`rgeom`][stats::rgeom()]
#'    distribution - it defines whether an observation will be made by the same
#'    observer for several years and whether it will not be made at all
#'    (default `prob = 0.3`); used when `type = "monitoring_based"`}
#'
#'   \item{`progress_bar`
#'
#'    logical vector of length 1; determines if progress bar for observational process should be
#'    displayed (default `progress_bar = FALSE`);
#'    used when `type = "monitoring_based"`}
#' }
#'
#' @return `data.frame` object with geographic coordinates, time steps,
#' estimated abundances, including observation error (if `sdlog > 0`)
#' and observer identifiers (if `type = "monitoring_based"`)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(terra)
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))
#'
#' # prepare data
#' sim_data <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#'
#' sim_1 <- sim(obj = sim_data, time = 110, burn = 10)
#'
#' # 1. random_one_layer
#' sample1 <- get_observations(
#'   sim_data,
#'   sim_1,
#'   type = "random_one_layer",
#'   prop = 0.1
#' )
#'
#' # 2. random_all_layers
#' sample2 <- get_observations(
#'   sim_data,
#'   sim_1,
#'   type = "random_all_layers",
#'   prop = 0.15
#' )
#'
#' # 3. from_data
#' sample3 <- get_observations(
#'   sim_data,
#'   sim_1,
#'   type = "from_data",
#'   points = observations_points
#' )
#'
#' # 4. monitoring_based
#' # define observations sites
#' all_points <- xyFromCell(sim_data$id, cells(sim_data$K_map))
#' sample_idx <- sample(1:nrow(all_points), size = 20)
#' sample_points <- all_points[sample_idx, ]
#'
#' sample4 <- get_observations(
#'   sim_data,
#'   sim_1,
#'   type = "monitoring_based",
#'   cells_coords = sample_points,
#'   prob = 0.3,
#'   progress_bar = TRUE
#' )
#'
#' # 5. noise
#' sample5 <- get_observations(sim_data,
#'   sim_1,
#'   type = "random_one_layer",
#'   sdlog = log(1.2)
#' )
#' }
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, G2.3, G2.3b, SP2.6} documented types expectation
#' @srrstats {G2.7} points and cells_coords can be a dataframe or matrix
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP4.1} returned object has the same unit as the input
#' @srrstats {SP4.2} returned values are documented
#'
#'
get_observations <- function(
    sim_data, sim_results, type = c("random_one_layer", "random_all_layers",
    "from_data", "monitoring_based"), sdlog = log(1), ...) {

  #' @srrstats {G2.0, G2.2} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6, SP2.7} assert input type
  # arguments validation
  type <- match.arg(type)
  assert_that(inherits(sim_data, "sim_data"))
  assert_that(inherits(sim_results, "sim_results"))
  assert_that(length(sdlog) == 1)
  assert_that(is.numeric(sdlog))

  # transform N_map to raster based on id
  N_rast <- rast(
    sim_results$N_map, extent = ext(sim_data$id), crs = crs(sim_data$id)
  )

  # call the right sampling function
  if (type %in% c("random_one_layer", "random_all_layers")) {
    out <- get_observations_random(N_rast, type, ...)
  } else if (type == "from_data") {
    out <- get_observations_from_data(N_rast, ...)
  } else if (type == "monitoring_based") {
    out <- get_observations_monitoring_based(N_rast, ...)
  }

  # add noise
  if (sdlog > 0) {
    out$value <- rlnorm(nrow(out), log(out$value), sdlog)
  }

  return(out)
}



# Internal functions for get_observations function -----------------------------



#' Random sampling
#'
#' `get_observations` calls this function if sampling type equals to
#' "random_one_layer" or "random_all_layers".
#'
#' @param N_rast [`SpatRaster`][terra::SpatRaster-class] object
#' with abundances for each time step
#' @param prop numeric vector of length 1; proportion of cells to be sampled
#' @inheritParams get_observation
#'
#' @return `data.frame` object with coordinates, time steps, abundances
#' without noise
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
get_observations_random <- function(N_rast, type, prop = 0.1) {

  #' @srrstats {G2.0, G2.2} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6} assert input type
  assert_that(length(prop) == 1)
  assert_that(is.numeric(prop))
  assert_that(prop > 0 && prop <= 1,
              msg = "prop parameter must be greater than 0 but less than or equal to 1")

  # set sample size
  size <- ncell(N_rast) * prop

  # sample
  if (type == "random_one_layer") { # the same cells in each layer

    out <- spatSample(N_rast, size, xy = TRUE, na.rm = TRUE)
    colnames(out) <- c("x", "y", seq_len(nlyr(N_rast)))

    out <- as.data.frame(reshape(
      out,
      direction = "long",
      varying = list(as.character(seq_len(nlyr(N_rast)))),
      v.names = "value",
      idvar = c("x", "y"),
      timevar = "time_step",
      times = seq_len(nlyr(N_rast))))
    rownames(out) <- seq_len(nrow(out))

  } else if (type == "random_all_layers") { # different cells in each layer

    out <- lapply(
      seq_len(nlyr(N_rast)),
      function(i) {
        N_layer <- N_rast[[i]]
        N_layer_sample <- cbind(
          spatSample(N_layer, size, xy = TRUE, na.rm = TRUE), i)
        colnames(N_layer_sample) <- c("x", "y", "value", "time_step")

        return(N_layer_sample)
      }
    )

    out <- do.call(rbind, out)
  }

  return(out[c("x", "y", "time_step", "value")])
}


#' Sampling based on given data.frame
#'
#' [get_observations] calls this function if sampling type
#' equals to "from_data".
#'
#' @param points `data.frame` or `matrix` with 3 named numeric columns ("x", "y" and "time_step")
#' containing coordinates and time steps from which observations
#' should be obtained`
#' @inheritParams get_observations_random
#'
#' @return `data.frame` object with coordinates, time steps numbers,
#' abundances without noise
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
get_observations_from_data <- function(N_rast, points) {

  #' @srrstats {G2.0, G2.2} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6} assert input type
  #' @srrstats {G2.8} matrix to dataframe
  assert_that(is.data.frame(points) || is.matrix(points))
  points <- as.data.frame(points)
  assert_that(ncol(points) == 3)
  assert_that(nrow(points) > 0)
  assert_that(
    all(names(points) == c("x", "y", "time_step")),
    msg = "columns in points parameter should have the following names: \"x\", \"y\", \"time_step\"")
  assert_that(
    all(sapply(points, is.numeric)),
    msg = "some element of point are not numeric")

  value <- unlist(lapply(
    seq_len(nlyr(N_rast)),
    function(i) {
      tmp_points <- points[points$time_step == i, ]
      extract(
        N_rast[[i]],
        tmp_points[c("x", "y")])[, 2]
    }
  ))

  out <- cbind(points, value = unlist(value))
}



#' Sampling that mimics a real survey programmes
#'
#' [get_observations] calls this function if sampling type
#' equals to "monitoring_based".
#'
#' @param cells_coords matrix object with two columns: "x" and "y"
#' @param prob probability of success in each trial - [stats::rgeom()] parameter
#' @param progress_bar logical vector of length 1; determines if progress bar for observation
#' should be displayed (if `type = "monitoring_based"`)
#' @inheritParams get_observations_random
#'
#' @return `data.frame` object with coordinates, time steps, abundances without
#' noise and observer_id
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
get_observations_monitoring_based <- function(
    N_rast, cells_coords, prob = 0.3, progress_bar = FALSE) {

  #' @srrstats {G2.0, G2.2} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6} assert input type
  #' @srrstats {G2.8} matrix to dataframe
  assert_that(is.data.frame(cells_coords) || is.matrix(cells_coords))
  cells_coords <- as.data.frame(cells_coords)
  assert_that(ncol(cells_coords) == 2)
  assert_that(nrow(cells_coords) > 0)
  assert_that(
    all(names(cells_coords) == c("x", "y")),
    msg = "columns in cells_coords parameter should have the following names: \"x\", \"y\"")
  assert_that(
    all(sapply(cells_coords, is.numeric)),
    msg = "some element of cells_coords are not numeric")


  ncells <- nrow(cells_coords)
  time_steps <- nlyr(N_rast)


  points <- data.frame(
    x = rep(cells_coords[, "x"], each = time_steps),
    y = rep(cells_coords[, "y"], each = time_steps),
    time_step = rep(1:time_steps, ncells),
    obs_id = NA
  )

  if (progress_bar) {
    pb <- txtProgressBar(
      min = 1, max = ncells, style = 3
    )
  }

  for (i in 1:ncells) {
    curr_time_steps <- 1
    curr_id <- 1

    while (curr_time_steps < time_steps) {
      observers_sequence <- rgeom(1, prob)

      if (observers_sequence > 0) {

        # the last observer for current cell
        if (curr_time_steps + observers_sequence > time_steps) {
          observers_sequence <- time_steps - curr_time_steps
        }

        row_id <- (i - 1) * time_steps + curr_time_steps
        points$obs_id[row_id:(row_id + observers_sequence - 1)] <-
          rep(paste0("obs", curr_id), observers_sequence)

        curr_time_steps <- curr_time_steps + observers_sequence
        curr_id <- curr_id + 1
      } else {
        curr_time_steps <- curr_time_steps + 1
      }
    }
    if (progress_bar) setTxtProgressBar(pb, i)
  }
  if (progress_bar) close(pb)

  points <- points[!is.na(points$obs_id), ]

  value <- lapply(
    seq_len(nlyr(N_rast)),
    function(i) {
      tmp_points <- points[points$time_step == i, ]
      tmp_vals <- extract(
        N_rast[[i]],
        tmp_points[c("x", "y")])[,2]

      cbind(tmp_points, value = tmp_vals)
    }
  )

  out <- do.call(rbind, value)
  out <- out[order(as.numeric(rownames(out))),]

  return(out)
}
