#' Prepare data required to perform a simulation
#'
#' This function generates a `sim_data` object which contains all the necessary
#' information needed to run a simulation by the [`sim`] function.
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
#' [`K_get_interpolation`] function can be used to prepare `K_map` that changes
#' in time. This may be useful, when simulating environmental change or
#' exploring the effects of ecological disturbances.
#'
#'
#' @param n1_map [`SpatRaster`][terra::SpatRaster-class] object; population
#' numbers in every square at the first time step
#' @param K_map [`SpatRaster`][terra::SpatRaster-class] object; carrying
#' capacity map (if K is constant across time) or maps (if K is time-varying)
#' @param K_sd a real number >= 0 (default 0); this parameter can be used if
#' additional environmental stochasticity is required; if `K_sd > 0`,
#' a random numbers are generated from a log-normal distribution with
#' the mean `K_map` and standard deviation `K_sd`
#' @param r a real number; intrinsic population growth rate
#' @param r_sd a real number `>= 0` (default `0`); if additional demographic
#' stochasticity is required, `r_sd > 0` is the standard deviation for a normal
#' distribution around `r` (defined for each time step)
#' @param growth string; the name of a population growth function,
#' either defined in [`growth`] or provided by the user
#' @param A a real number; strength of the Allee effect
#' (see the [`growth`] function)
#' @param dens_dep a string specifying if the probability of settling in
#' a target square is:
#' \itemize{
#'   \item{"none" - fully random,}
#'   \item{"K" - proportional to the carrying capacity of a target square,}
#'   \item{"K2N" - density-dependent, i.e. proportional to the ratio of
#'   carrying capacity of a target square to the number of individuals
#'   already present in a target square}
#' }
#' @param border a string defining how to deal with borders:
#' \itemize{
#'   \item "absorbing" - individuals that disperse outside the study area
#'   are removed from the population
#'   \item "reprising" - cells outside the study area are not allowed
#'   as targets for dispersal
#' }
#' @param kernel_fun string; name of a random number generation function
#' defining a dispersal kernel
#' @param ... any parameters required by `kernel_fun`
#' @param max_dist integer; maximum distance of dispersal to pre-calculate
#' target cells
#' @param calculate_dist logical; determines if target cells will
#' be precalculated
#' @param dlist list; target cells at a specified distance calculated
#' for every cell within the study area
#' @param progress_bar logical; determines if progress bar for calculating
#' distances should be displayed (used only if dlist is `NULL`)
#' @param quiet logical; determines if messages for calculating distances
#' should be displayed (used only if dlist is `NULL`)
#' @param cl cluster object created by [`makeCluster`][parallel::makeCluster()]
#'
#' @return Object of class `sim_data` which inherits from `list`. This object
#' contains all necessary information to perform a simulation using
#' [`sim`] function.
#'
#' @export
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
initialise <- function(
    n1_map, K_map, K_sd = 0, r, r_sd = 0, growth = "gompertz", A = NA,
    dens_dep = c("K2N", "K", "none"), border = c("absorbing", "reprising"),
    kernel_fun = "rexp", ..., max_dist = NA, calculate_dist = TRUE,
    dlist = NULL, progress_bar = FALSE, quiet = TRUE, cl = NULL) {

  # validation of arguments
  dens_dep <- match.arg(dens_dep)
  border <- match.arg(border)

  changing_env <- nlyr(K_map) != 1
  K_n1_map_check(K_map, n1_map, changing_env)

  # classify NaN to NA for input maps
  n1_map <- classify(n1_map, cbind(NaN, NA))
  K_map <- classify(K_map, cbind(NaN, NA))

  # define other data
  ncells <- ncell(n1_map)
  id <- n1_map
  values(id) <- matrix(1:ncells, nrow(n1_map), ncol(n1_map))

  resolution <- res(id)
  if(resolution[1] != resolution[2]) {
    stop("Currently, rangr only supports rasters with square cells.\n",
         "You may want to change the resolution of your input maps ",
         "before proceeding")
  } else {
    resolution <- resolution[1]
  }

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
  within_mask <- as.matrix(!is.na(n1_map), wide = TRUE) # bool matrix -  the study area

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


#' Validating K_map and n1_map
#'
#' This internal function checks if `K_map` and `n1_map` are correct (contain
#' only non-negative values or NAs) and corresponds to each other. If `K_map`
#' has more than one layer, object comparison is performed based on the first
#' layer.
#' In case of any mistake in given data, suitable error message is printed.
#'
#' @inheritParams initialise
#' @param changing_env logical; determines if carrying capacity map is changing
#' during the [`sim`]ulation
#'
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
    stop("n1_map can contain only non-negative values or NAs")
  }

  if (!all(values(K_map) >= 0, na.rm = TRUE)) {
    stop("K_map can contain only non-negative values or NAs")
  }
}


#' Get carrying capacity for the first time step
#'
#' [K_get_init_values] returns all values from map of carrying capacity
#' in the first time step.
#'
#' @inheritParams K_n1_map_check
#'
#' @return Numeric vector with values of all cells carrying capacity
#' in the first time step.
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


#' Check for precalculating target cells
#'
#' [calc_dist] checks if target cells should be precalculated
#' and if so calls [dist_list].
#'
#' @param id [`SpatRaster`][terra::SpatRaster-class]; contains all cells ids
#' @param data_table matrix; contains information about all cells in current
#' time points
#' @param resolution integer; dimension of one cell of `id`
#' @param id_within numeric vector; ids of cells inside the study area
#' @inheritParams initialise
#'
#' @return List of target cells ids for each target cells in any distance
#' within `max_dist`.
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


#' Precalculating target cells for dispersal
#'
#' `dist_list` checks if precalculation of target cells ids should be done
#' in a linear or parallel way and then uses [target_ids] for calculation
#'
#' @inheritParams calc_dist
#'
#' @return List of available target cells from each cell within the study area,
#' divided by distance
#'
#' @noRd
#'
dist_list <- function(
    id, data_table, resolution, id_within, max_dist, progress_bar, cl) {

  # within_list <- !is.na(data_table[, "K"])
  data <- cbind(data_table[, c("id", "x", "y")], dist = NA)


  # specify function and arguments (for clarity)

  id <- raster(id)
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

    id <- unwrap(id)
  }


  return(out)
}



#' Get indexes of target cells
#'
#' [target_ids] finds all target cells available from given focal cell,
#' that lie within the maximum distance threshold (`max_dist`).
#'
#' @param idx integer; id of cell
#' @param data matrix; necessary data (defined in [dist_list])
#' @param id_within vector; indexes of cells inside study area
#' (defined in [dist_list])
#' @inheritParams dist_list
#'
#' @return List of target cells for each distance or `NULL` if there isn't any
#'
#' @noRd
#'
target_ids <- function(idx, id, data, resolution, max_dist, id_within) {

  # get coordinates of current cell
  id_i <- data[data[, "id"] == idx, ]
  xy_i <- cbind(id_i["x"], id_i["y"])

  # calculate distances
  d <- distanceFromPoints(id, xy_i, progress = 0)
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



#' Count cells on every distance
#'
#' This internal function counts how many cells are reachable on each distance
#' from any cells of template `r`. It takes raster's resolution into account.
#'
#' @param template template [`SpatRaster`][terra::SpatRaster-class] object
#'
#' @return numeric vector; numbers of target cells on every possible distance
#' range
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
