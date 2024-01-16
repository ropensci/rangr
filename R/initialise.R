#' Prepare Data Required To Perform A Simulation
#'
#' This function generates a `sim_data` object which contains all the necessary
#' information needed to run a simulation by the [`sim`] function. Note that the
#' input maps (`n1_map` and `K_map`) must be in the Cartesian coordinate system.
#'
#'
#' The most time-consuming part of computations performed by the [`sim`]
#' function is the simulation of dispersal. To speed it up, a list containing
#' indexes of target cells at a specified distance from a focal cell,
#' is calculated in advance and stored in a `dlist` slot. To speed things up
#' even more, these calculations can be done in parallel, providing that
#' a cluster object created by [`makeCluster`][parallel::makeCluster()] is
#' specified using the `cl` parameter. The parameter `max_dist` sets
#' the maximum distance at which this pre-calculation is done. If `max_dist`
#' is `NULL` then it is set to 0.9 quantile from the `kernel_fun`.
#'
#' NA in input maps stands for cells that are outside the study area.
#'
#' [`K_get_interpolation`] function can be used to prepare `K_map` that changes
#' in time. This may be useful, when simulating environmental change or
#' exploring the effects of ecological disturbances.
#'
#'
#' @param n1_map [`SpatRaster`][terra::SpatRaster-class] object with one layer;
#' population numbers in every square at the first time step
#' @param K_map [`SpatRaster`][terra::SpatRaster-class] object with one layer;
#' carrying capacity map (if K is constant across time) or maps (if K is
#' time-varying)
#' @param K_sd numeric vector of length 1 with value `>= 0` (default 0);
#' this parameter can be used if additional environmental stochasticity
#' is required; if `K_sd > 0`, a random numbers are generated from a log-normal
#' distribution with the mean `K_map` and standard deviation `K_sd`
#' @param r numeric vector of length 1; intrinsic population growth rate
#' @param r_sd numeric vector of length 1 with value `>= 0` (default `0`);
#' if additional demographic stochasticity is required, `r_sd > 0` is
#' the standard deviation for a normal distribution around `r`
#' (defined for each time step)
#' @param growth character vector of length 1; the name of a population growth
#' function, either defined in [`growth`] or provided by
#' the user (case-sensitive, default [`"gompertz"`][growth])
#' @param A numeric vector of length 1; strength of the Allee effect
#' (see the [`growth`] function)
#' @param dens_dep character vector of length 1 specifying if the probability
#' of settling in a target square is (case-sensitive, default `"K2N"`):
#' \itemize{
#'   \item{"none" - fully random,}
#'   \item{"K" - proportional to the carrying capacity of a target square,}
#'   \item{"K2N" - density-dependent, i.e. proportional to the ratio of
#'   carrying capacity of a target square to the number of individuals
#'   already present in a target square}
#' }
#' @param border character vector of length 1 defining how to deal
#' with borders (case-sensitive, default `"absorbing"`):
#' \itemize{
#'   \item "absorbing" - individuals that disperse outside the study area
#'   are removed from the population
#'   \item "reprising" - cells outside the study area are not allowed
#'   as targets for dispersal
#' }
#' @param kernel_fun character vector of length 1; name of a random number
#' generation function defining a dispersal kernel (case-sensitive, default `"rexp"`)
#' @param ... any parameters required by `kernel_fun`
#' @param max_dist numeric vector of length 1; maximum distance of dispersal
#' to pre-calculate target cells
#' @param calculate_dist logical vector of length 1; determines if target cells
#' will be precalculated
#' @param dlist list; target cells at a specified distance calculated
#' for every cell within the study area
#' @param progress_bar logical vector of length 1; determines if progress bar
#' for calculating distances should be displayed (used only if dlist is `NULL`)
#' @param quiet logical vector of length 1; determines if messages for
#' calculating distances should be displayed (used only if dlist is `NULL`)
#' @param cl cluster object created by [`makeCluster`][parallel::makeCluster()]
#'
#' @return Object of class `sim_data` which inherits from `list`. This object
#' contains all necessary information to perform a simulation using
#' [`sim`] function.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' # input maps
#' library(terra)
#'
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))
#' K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
#'                          package = "rangr"))
#'
#' # basic example
#' sim_data_1 <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#'
#' # example with changing environment
#' K_interpolated <- K_get_interpolation(
#'   K_small_changing,
#'   K_time_points = c(1, 25, 50)
#' )
#'
#' sim_data_2 <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_interpolated,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#'
#' # example with progress bar
#' sim_data_3 <- initialise(
#'   n1_map = n1_small, K_map = K_small, K_sd = 5, r = log(5),
#'   r_sd = 4, growth = "ricker", rate = 1 / 200,
#'   max_dist = 5000, dens_dep = "K2N", progress_bar = TRUE
#' )
#'
#' # example with parallelization
#' library(parallel)
#' cl <- makeCluster(detectCores())
#'
#' sim_data_4 <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3,
#'   cl = cl,
#'   progress_bar = TRUE
#' )
#' stopCluster(cl)
#' }
#'
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, G2.3, G2.3b, SP2.6} documented types expectation
#' @srrstats {SP1.0} specified domain of applicability
#' @srrstats {SP1.1} documented dimensional domain of applicability
#' @srrstats {SP2.0, SP2.0b} check if K_map and n1_map is
#' [`SpatRaster`][terra::SpatRaster-class] and error otherwise
#' @srrstats {SP2.3} load data in spatial formats
#' @srrstats {SP2.8, SP2.9} simple pre-processing routine that validates and
#' transforms input data while maintaining necessary metadata
#' @srrstats {SP4.0, SP4.0b} returns sim_data object
#' @srrstats {SP4.1} returned object has the same unit as the input
#' @srrstats {SP4.2} returned values are documented
#'

initialise <- function(
    n1_map, K_map, K_sd = 0, r, r_sd = 0, growth = "gompertz", A = NA,
    dens_dep = c("K2N", "K", "none"), border = c("absorbing", "reprising"),
    kernel_fun = "rexp", ..., max_dist = NA, calculate_dist = TRUE,
    dlist = NULL, progress_bar = FALSE, quiet = TRUE, cl = NULL) {

  #' @srrstats {G2.0, G2.2, G2.13} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6, SP2.7} assert input type
  # Validation of arguments
  ## input maps
  assert_that(inherits(K_map, "SpatRaster"))
  assert_that(inherits(n1_map, "SpatRaster"))

  resolution <- res(n1_map)
  if(resolution[1] != resolution[2]) {
    stop("Currently, rangr only supports rasters with square cells.\n",
         "You may want to change the resolution of your input maps ",
         "before proceeding")
  } else {
    resolution <- resolution[1]
  }

  changing_env <- nlyr(K_map) != 1
  K_n1_map_check(K_map, n1_map, changing_env)

  ## K_sd
  assert_that(length(K_sd) == 1)
  assert_that(is.numeric(K_sd))
  assert_that(K_sd >= 0)

  ## r
  assert_that(length(r) == 1)
  assert_that(is.numeric(r))

  ## r_sd
  assert_that(length(r_sd) == 1)
  assert_that(is.numeric(r_sd))
  assert_that(r_sd >= 0)

  ## growth
  assert_that(length(growth) == 1)
  assert_that(is.character(growth))

  ## A
  assert_that(length(A) == 1)
  assert_that(
    is.numeric(A) | is.na(A),
    msg = "parameter A can be set either as NA or as a single number")

  ## dens_dep
  dens_dep <- match.arg(dens_dep)

  ## border
  border <- match.arg(border)

  ## kernel_fun
  assert_that(length(kernel_fun) == 1)
  assert_that(is.character(kernel_fun))

  ## max_dist
  assert_that(length(max_dist) == 1)
  assert_that(
    (is.numeric(max_dist) && !(max_dist < 0)) || is.na(max_dist),
    msg = "parameter max_dist can be set either as NA or as a single positive number") #nolint

  ## calculate_dist
  assert_that(length(calculate_dist) == 1)
  assert_that(is.logical(calculate_dist))


  ## dlist
  assert_that(
    is.list(dlist) || is.null(dlist),
    msg = "parameter dlist can be set either as NULL or as a list with integers") #nolint

  ## progress_bar
  assert_that(length(progress_bar) == 1)
  assert_that(is.logical(progress_bar))

  ## quiet
  assert_that(length(quiet) == 1)
  assert_that(is.logical(quiet))



  #' @srrstats {G2.16} Check for NaNs and convert them to Nas
  # classify NaN to NA for input maps
  if (any(is.nan(values(n1_map))) || any(is.nan(values(K_map)))) {

    message("NaN values were found in input maps and replaced with NA (cells outside the study area)") #nolint
    n1_map <- classify(n1_map, cbind(NaN, NA))
    K_map <- classify(K_map, cbind(NaN, NA))

  }

  # define other data
  ncells <- ncell(n1_map)
  id <- n1_map
  values(id) <- matrix(1:ncells, nrow(n1_map), ncol(n1_map))



  dynamics <- function(x, r, K, A) match.fun(growth)(x, r, K, A)
  kernel <- function(n) match.fun(kernel_fun)(n, ...)

  max_dist <- ifelse(
    is.na(max_dist),
    round(quantile(kernel(1e4), 0.9, names = FALSE) / resolution) * resolution,
    max_dist
  )

  # apply environmental stochasticity if specified (space specific)
  if (K_sd > 0) {
    K_map <- app(K_map, function(x) {
      suppressWarnings(rlnorm(length(x), log(x), log(K_sd)))
    })
  }



  data_table <- as.matrix(data.frame(
    values(id),
    xyFromCell(id, 1:ncells),
    K_get_init_values(K_map, changing_env),
    values(n1_map)
  ))
  colnames(data_table) <- c("id", "x", "y", "K", "N")
  data_table <- data_table[order(data_table[, "id"]), ]


  id_within <- data_table[!is.na(data_table[, "K"]), "id"]
  within_mask <- as.matrix(!is.na(n1_map), wide = TRUE) # bool matrix -  the study area #nolint

  if (is.null(dlist)) {
    dlist <- calc_dist(
      calculate_dist, id, data_table, resolution, id_within,
      max_dist, progress_bar, quiet, cl
    )
  }

  ncells_in_circle <- switch(border, # no. of cells at each distance
                             absorbing = ncell_in_circle(n1_map),
                             reprising = NULL
  )
  # output list
  out <- list(
    n1_map = as.matrix(n1_map, wide = TRUE),
    id = id,
    resolution = resolution,
    r = r,
    r_sd = r_sd,
    K_map = K_map,
    K_sd = K_sd,
    growth = growth,
    A = A,
    dynamics = dynamics,
    dens_dep = dens_dep,
    border = border,
    max_dist = max_dist,
    kernel_fun = kernel_fun,
    kernel = kernel,
    dlist = dlist,
    data_table = data_table,
    id_within = id_within,
    within_mask = within_mask,
    ncells = ncells,
    ncells_in_circle = ncells_in_circle,
    changing_env = changing_env,
    call = get_initialise_call(match.call())
  )

  class(out) <- c("sim_data", class(out))

  return(out)
}

# alias for initialise
#' @rdname initialise
#' @export
initialize <- initialise



# internal functions -----------------------------------------------------------


#' Validating K_map And n1_map
#'
#' This internal function checks if `K_map` and `n1_map` are correct (contain
#' only non-negative values or NAs) and corresponds to each other. If `K_map`
#' has more than one layer, object comparison is performed based on the first
#' layer.
#' In case of any mistake in given data, suitable error message is printed.
#'
#' @inheritParams initialise
#' @param changing_env logical vector of length 1; determines if carrying
#' capacity map is changing during the [`sim`]ulation
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
K_n1_map_check <- function(K_map, n1_map, changing_env) {

  # compare n1_map and K_map
  ifelse(!changing_env,
         compareGeom(n1_map, K_map),
         compareGeom(n1_map, subset(K_map, 1)))

  # check if values are non-negative
  if (!all(values(n1_map) >= 0, na.rm = TRUE)) {
    stop("n1_map can contain only non-negative values or NAs (which will be automatically reclassified to NA)") #nolint
  }

  if (!all(values(K_map) >= 0, na.rm = TRUE)) {
    stop("K_map can contain only non-negative values or NAs (which will be automatically reclassified to NA)") #nolint
  }
}


#' Get Carrying Capacity For The First Time Step
#'
#' [K_get_init_values] returns all values from map of carrying capacity
#' in the first time step.
#'
#' @inheritParams K_n1_map_check
#'
#' @return Numeric vector with values of all cells carrying capacity
#' in the first time step.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
K_get_init_values <- function(K_map, changing_env) {

  if (!changing_env) {
    K_values <- values(K_map)
  } else {
    K_values <- values(subset(K_map, 1))
  }

  return(K_values)
}


#' Check For Precalculating Target Cells
#'
#' [calc_dist] checks if target cells should be precalculated
#' and if so calls [dist_list].
#'
#' @param id [`SpatRaster`][terra::SpatRaster-class]; contains all cells ids
#' @param data_table matrix; contains information about all cells in current
#' time points
#' @param resolution integer vector of length 1; dimension of one cell of `id`
#' @param id_within numeric vector; ids of cells inside the study area
#' @inheritParams initialise
#'
#' @return List of target cells ids for each target cells in any distance
#' within `max_dist`.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
calc_dist <- function(
    calculate_dist, id, data_table, resolution, id_within, max_dist,
    progress_bar, quiet, cl) {

  if (calculate_dist) {
    if (!quiet) {
      cat("Calculating distances...", "\n")
    }
    dlist <- dist_list(
      id, data_table, resolution, id_within, max_dist,
      progress_bar, cl
    )
  } else {
    dlist <- NULL
  }


  return(dlist)
}


#' Precalculating Target Cells For Dispersal
#'
#' `dist_list` checks if precalculation of target cells ids should be done
#' in a linear or parallel way and then uses [target_ids] for calculation
#'
#' @inheritParams calc_dist
#'
#' @return List of available target cells from each cell within the study area,
#' divided by distance
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
dist_list <- function(
    id, data_table, resolution, id_within, max_dist, progress_bar, cl) {

  # within_list <- !is.na(data_table[, "K"])
  data <- cbind(data_table[, c("id", "x", "y")], dist = NA)


  # specify function and arguments (for clarity)
  if(!is.null(cl)) id <- wrap(id)
  tfun <- function(x) target_ids(x, id, data, resolution, max_dist, id_within)

  # calculate targets id with or without parallelization
  if (is.null(cl)) {
    if (progress_bar) {
      out <- pblapply(id_within, tfun)
    } else {
      out <- lapply(id_within, tfun)
    }
  } else {

    # clusterExport(cl, c(
    #   "target_ids", "id", "data", "resolution",
    #   "max_dist", "id_within"
    # ),
    # envir = environment())

    if (progress_bar) {
      out <- pblapply(id_within, tfun, cl = cl)
    } else {
      out <- parLapplyLB(cl = cl, id_within, tfun)
    }
#
#     id <- unwrap(id)
  }


  return(out)
}



#' Get Indexes Of Target Cells
#'
#' [target_ids] finds all target cells available from given focal cell,
#' that lie within the maximum distance threshold (`max_dist`).
#'
#' @param idx integer vector of length 1; id of cell
#' @param data integer matrix; necessary data (defined in [dist_list])
#' @param id_within integer vector; indexes of cells inside study area
#' (defined in [dist_list])
#' @inheritParams dist_list
#'
#' @return List of target cells for each distance or `NULL` if there isn't any
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
target_ids <- function(idx, id, data, resolution, max_dist, id_within) {

  # get coordinates of current cell
  id_i <- data[data[, "id"] == idx, ]
  xy_i <- vect(cbind(id_i["x"], id_i["y"]))
  id <- unwrap(id)
  crs(xy_i) <- crs(id)

  # calculate distances
  d <- distance(id, xy_i, progress = 0)
  d <- round(c(as.matrix(d, wide = TRUE)) / resolution)
  # data[, "dist"] <- d

  # check if cells are within study area and specified range
  d_within <- d[id_within]
  in_range <- d_within >= 1 & d_within <= (max_dist / resolution)

  # check if such a cell exists
  if (!any(in_range)) {
    return(NULL)
  }


  # extract cells ids and distance at which they are
  ids <- id_within[in_range]
  ds <- d_within[in_range]

  # all needed distances
  dists <- seq_len(max_dist / resolution)

  # make list of target cells for each distance
  lapply(dists, function(x) {
    out <- ids[ds == x]

    if (length(out) == 0) { # if there isn't any target cell return null
      out <- NULL
    }

    return(out)
  })
}



#' Count Cells On Every Distance
#'
#' This internal function counts how many cells are reachable on each distance
#' from any cells of template `r`. It takes raster's resolution into account.
#'
#' @param template template [`SpatRaster`][terra::SpatRaster-class] object
#'
#' @return numeric vector; numbers of target cells on every possible distance
#' range
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
ncell_in_circle <- function(template) {

  # get resolution
  resolution <- res(template)[1]

  # get extents in both dimensions
  a <- xmax(template) - xmin(template) - resolution
  b <- ymax(template) - ymin(template) - resolution

  # calculate maximum possible distance
  max_dist <- round(sqrt(a**2 + b**2) / resolution) * resolution

  # raster based on max_dist
  e <- ext(
    0, 2 * max_dist + resolution, 0,
    2 * max_dist + resolution
  )
  d <- rast(e, resolution = resolution, crs = crs(template))
  center_point <- max_dist + resolution / 2
  center_vect <- vect(cbind(center_point, center_point))
  crs(center_vect) <- crs(template)

  # calculate all distances
  d <- distance(d, center_vect, progress = 0)

  # adjust to resolution
  d_cell <- as.matrix(round(d / resolution))

  # return number of cells on each distance
  return(tabulate(d_cell)[1:(max_dist / resolution)])
}

get_initialise_call <- function(call) {

  if ("dlist" %in% names(call)) {
    call$dlist <- TRUE
  }

  return(call)
}
