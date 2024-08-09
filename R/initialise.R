#' Prepare Data Required To Perform A Simulation
#'
#' This function generates a `sim_data` object containing all the necessary
#' information required to run a simulation by the [`sim`] function. The
#' input maps (`n1_map` and `K_map`) can be in the Cartesian or longitude/latitude
#' coordinate system.
#'
#'
#' The most time-consuming part of computations performed by the [`sim`]
#' function is the simulation of dispersal. To speed it up, a list containing
#' indexes of target cells at a specified distance from a focal cell
#' is calculated in advance and stored in a `dlist` slot.
#' The `max_dist` parameter sets
#' the maximum distance at which this pre-calculation is performed. If `max_dist`
#' is `NULL`, it is set to 0.99 quantile from the `kernel_fun`.
#' All distance calculations are always based on metres if the input maps are
#' latitude/longitude. For planar input maps, distances are calculated in map
#' units, which are typically metres, but check the [`crs()`][terra::crs]
#' if in doubt.
#'
#' If the input maps are in the Cartesian coordinate system and the grid cells
#' are squares, then
#' the distances between cells are calculated using the [`distance`][terra::distance]
#' function from the `terra` package. These distances are later divided by the
#' resolution of the input maps.
#'
#' For input maps with grid cells in shapes other than squares (e.g. with
#' rectangular cells or longitude/latitude coordinate system), the distance
#' resolution is calculated by finding the shortest distance between each
#' "queen" type neighbor. All distances calculated by the [`distance`][terra::distance]
#' function are further divided by this distance resolution.
#' To avoid discontinuities in the distances at which the target cells are located,
#' an additional parameter `dist_bin` is calculated as half of the maximum
#' distance between each "queen" type neighbour. It is used to expand the
#' distances at which target cells are located from a single number to a range.
#'
#' NA in the input maps represents cells outside the study area.
#'
#' The [`K_get_interpolation`] function can be used to prepare `K_map` that changes
#' over time. This may be useful, when simulating environmental change or
#' exploring the effects of ecological disturbances.
#'
#'
#'
#' @param n1_map [`SpatRaster`][terra::SpatRaster-class] object with one layer;
#' population numbers in every grid cell at the first time step
#' @param K_map [`SpatRaster`][terra::SpatRaster-class] object with one layer;
#' carrying capacity map (if K is constant across time) or maps (if K is
#' time-varying)
#' @param K_sd numeric vector of length 1 with value `>= 0` (default 0);
#' this parameter can be used if additional environmental stochasticity
#' is required; if `K_sd > 0`, random numbers are generated from a log-normal
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
#' of settling in a target grid cell is (case-sensitive, default `"K2N"`):
#' \itemize{
#'   \item "none" - fully random,
#'   \item "K" - proportional to the carrying capacity of a target cell,
#'   \item "K2N" - density-dependent, i.e. proportional to the ratio of
#'   carrying capacity of a target cell to the number of individuals
#'   already present in a target cell
#' }
#' @param border character vector of length 1 defining how to deal
#' with borders (case-sensitive, default `"absorbing"`):
#' \itemize{
#'   \item "reprising" - cells outside the study area are not allowed
#'   as targets for dispersal
#'   \item "absorbing" - individuals that disperse outside the study area
#'   are removed from the population
#' }
#' @param kernel_fun character vector of length 1; name of a random number
#' generation function defining a dispersal kernel (case-sensitive,
#' default `"rexp"`)
#' @param ... any parameters required by `kernel_fun`
#' @param max_dist numeric vector of length 1; maximum distance of dispersal
#' to pre-calculate target cells
#' @param calculate_dist logical vector of length 1; determines if target cells
#' will be precalculated
#' @param dlist list; target cells at a specified distance calculated
#' for every cell within the study area
#' @param progress_bar logical vector of length 1; determines if progress bar
#' for calculating distances should be displayed
#' @param quiet logical vector of length 1; determines if messages should be displayed
#'
#' @return Object of class `sim_data` which inherits from `list`. This object
#' contains all necessary information to perform a simulation using
#' [`sim`] function.
#'
#' @export
#' @seealso [update][rangr::update.sim_data()]
#'
#' @references Hijmans R (2024). terra: Spatial Data Analysis. R package version
#' 1.7-81, \url{https://rspatial.github.io/terra/}, \url{https://rspatial.org/}
#'
#' Solymos P, Zawadzki Z (2023). pbapply: Adding Progress Bar to '*apply' Functions. R
#' package version 1.7-2, \url{https://CRAN.R-project.org/package=pbapply}.
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
#' n1_small_lon_lat <- rast(system.file("input_maps/n1_small_lon_lat.tif", package = "rangr"))
#' K_small_lon_lat <- rast(system.file("input_maps/K_small_lon_lat.tif", package = "rangr"))
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
#' # example with lon/lat rasters
#' sim_data_3 <- initialise(
#'   n1_map = n1_small_lon_lat,
#'   K_map = K_small_lon_lat,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#'
#' # example without progress bar and messages
#' sim_data_4 <- initialise(
#'   n1_map = n1_small, K_map = K_small, K_sd = 0.1, r = log(5),
#'   r_sd = 4, growth = "ricker", rate = 1 / 200,
#'   max_dist = 5000, dens_dep = "K2N", progress_bar = FALSE, quiet = TRUE
#' )
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
    dens_dep = c("K2N", "K", "none"), border = c("reprising", "absorbing"),
    kernel_fun = "rexp", ..., max_dist = NA, calculate_dist = TRUE,
    dlist = NULL, progress_bar = TRUE, quiet = FALSE) {


  # check if session is interactive
  if(!interactive()) {
    call_names <- names(match.call())

    if(!("progress_bar" %in% call_names)) progress_bar <-  FALSE
    if(!("quiet" %in% call_names)) quiet <-  TRUE
  }

  #' @srrstats {G2.0, G2.2, G2.13} assert input length
  #' @srrstats {G2.1, G2.3, G2.3a, G2.6, SP2.7} assert input type
  # Validation of arguments
  ## input maps
  assert_that(inherits(K_map, "SpatRaster"))
  assert_that(inherits(n1_map, "SpatRaster"))

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
  if ((any(is.nan(values(n1_map))) || any(is.nan(values(K_map)))) && !quiet) {

    # message("NaN values were found in input maps and replaced with NA (cells outside the study area)") #nolint
    n1_map <- classify(n1_map, cbind(NaN, NA))
    K_map <- classify(K_map, cbind(NaN, NA))

  }

  # define ncells and id raster
  ncells <- ncell(n1_map)
  id <- n1_map
  values(id) <- matrix(1:ncells, nrow(n1_map), ncol(n1_map))


  # define population dynamic function and dispersal kernel
  dynamics <- function(x, r, K, A) match.fun(growth)(x, r, K, A)
  kernel <- function(n) match.fun(kernel_fun)(n, ...)


  # apply environmental stochasticity if specified (space specific)
  if (K_sd > 0) {
    K_map <- app(K_map, function(x) {
      # multiply input K_map values by generated errors
      x * suppressWarnings(rlnorm(length(x), 0, K_sd))
    })
  }

  # define matrix witch data about each cell grid
  data_table <- as.matrix(data.frame(
    values(id),
    xyFromCell(id, 1:ncells),
    K_get_init_values(K_map, changing_env),
    values(n1_map)
  ))
  colnames(data_table) <- c("id", "x", "y", "K", "N")
  data_table <- data_table[order(data_table[, "id"]), ]

  # ids of cells within the study are
  id_within <- data_table[!is.na(data_table[, "K"]), "id"]

  # bool matrix -  the study area
  within_mask <- as.matrix(!is.na(n1_map), wide = TRUE)

  # check if raster is planar
  planar <- !is.lonlat(id)

  # define dist_params: dist_bin, dist_resolution, max_avl_dist
  if(!planar) {
    # lon/lat input maps

    # calculate and extract dist_params
    dist_params <- calculate_dist_params(id, id_within, data_table,  progress_bar, quiet)
    dist_bin <- dist_params["dist_bin"]
    dist_resolution <- dist_params["dist_resolution"]
    max_avl_dist <- dist_params["max_avl_dist"]

  } else {
    # planar input maps

    # get input map resolution
    dist_resolution <- res(n1_map)
    if(dist_resolution[1] != dist_resolution[2]) {
      # not square grid cells

      # calculate and extract dist_params
      dist_params <- calculate_dist_params(id, ncells, data_table, progress_bar, quiet)
      dist_bin <- dist_params["dist_bin"]
      dist_resolution <- dist_params["dist_resolution"]
      max_avl_dist <- dist_params["max_avl_dist"]

    } else {
      # square grid cells

      # set default dist_params
      dist_resolution <- dist_resolution[1]
      dist_bin <- 0
      max_avl_dist <- NULL
    }
  }

  # define max_dist
  max_dist <- ifelse(
    is.na(max_dist),
    round(quantile(kernel(1e4), 0.99, names = FALSE) /
            dist_resolution) * dist_resolution,
    max_dist
  )


  # define dlist
  if (is.null(dlist)) {
    dlist <- calc_dist(
      calculate_dist, id, data_table, id_within, max_dist, dist_resolution,
      dist_bin, progress_bar, quiet
    )
  }

  # define number of cells at each distance for "absorbing" border
  if(border == "reprising") {
    ncells_in_circle <- NULL

  } else if (border == "absorbing") {
    if(planar) {
      # planar input maps
      ncells_in_circle <- ncell_in_circle_planar(id, dist_resolution)
    }
    else {
      # lon/lat input maps
      ncells_in_circle <- ncell_in_circle_lonlat(
        id, dist_resolution, dist_bin, id_within, max_avl_dist, progress_bar, quiet)
    }
  }


  # output list ---------------------------------------------
  out <- list(
    n1_map = as.matrix(n1_map, wide = TRUE),
    id = wrap(id),
    dist_bin = dist_bin,
    dist_resolution = dist_resolution,
    r = r,
    r_sd = r_sd,
    K_map = wrap(K_map),
    K_sd = K_sd,
    growth = growth,
    A = A,
    dynamics = dynamics,
    dens_dep = dens_dep,
    border = border,
    planar = planar,
    max_dist = max_dist,
    max_avl_dist = max_avl_dist,
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

  # set class
  class(out) <- c("sim_data", class(out))

  return(out)
}


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
  compareGeom(n1_map, K_map)

  # check NAs placement
  assert_that(
    ifelse(!changing_env,
           all(is.na(values(n1_map)) == is.na(values(K_map))),
           all(is.na(values(n1_map)) == is.na(values(subset(K_map, 1))))),
    msg = "n1_map and K_map have NA values in different grid cells")


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
    # get values from K_map
    K_values <- values(K_map)
  } else {
    # get values from first K_map if changing environment
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
#' @param dist_resolution integer vector of length 1; dimension of one side of
#' one cell of `id`; in case of an irregular grid or lon/lat raster it is calculated during [`initialisation`][`initialise`]
#' calculated by [`calculate_dist_params`]
#' @param dist_bin numeric vector of length 1 with value `>= 0`; in case of an irregular grid or lon/lat raster it is
#' calculated by [`calculate_dist_params`]
#' is equal to 0 if  input maps are planar raster; if input maps are lon/lat it is calculated by
#' rasters, `dist_bin` is calculated by [`calculate_dist_params`]
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
#' @name calc_dist
#'
#' @noRd
#'
calc_dist <- function(
    calculate_dist, id, data_table, id_within, max_dist, dist_resolution,
    dist_bin, progress_bar, quiet) {

  if (calculate_dist) {

    # message for user
    if (!quiet) cat("Calculating distances...", "\n")

    # calculate dlist
    dlist <- dist_list(
      id, data_table, id_within, max_dist, dist_resolution,
      dist_bin, progress_bar
    )
  } else {
    # set dlist to NULL
    dlist <- NULL
  }


  return(dlist)
}


#' Precalculating Target Cells For Dispersal
#'
#' `dist_list` prepares data for precalculation of target cells ids
#' and then uses [`target_ids`] for calculation
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
    id, data_table, id_within, max_dist, dist_resolution,
    dist_bin, progress_bar) {

  # prepare data table - add dist column anf fill it with NAs
  data <- cbind(data_table[, c("id", "x", "y")], dist = NA)


  # specify function and arguments (for clarity)
  tfun <- function(x)
    target_ids(x, id, data, min_dist_scaled = 1,
               max_dist_scaled = max_dist / dist_resolution,
               dist_resolution, dist_bin, id_within)

  # calculate targets id with or without progress bar
  if (!progress_bar) {

    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo))
  }

  out <- pblapply(id_within, tfun)



  return(out)
}



#' Calculate distance parameters
#'
#' Calculates `dist_bin`, `dist_resolution` and `max_avl_dist`. These parameters
#' are necessary to dispersal process if input maps have cells in differen shape
#' than squares.
#'
#' @inheritParams calc_dist
#'
#' @return List of available target cells from each cell within the study area,
#'
#' @noRd
#'
calculate_dist_params <- function(id, id_within, data_table, progress_bar, quiet) {

  # calculate distance to the closest neighbour from each grid cell
  params_fun <- function(x) {

    # get neighbours ids
    neighbours <- adjacent(id, cells = x, directions = "queen")
    neighbours <- neighbours[!is.na(neighbours)]

    # get raster with distances from current id
    xy <- vect(xyFromCell(id, x))
    crs(xy) <- crs(id)
    d <- values(distance(id, xy, progress = 0))

    # select distances to the neighbours
    neighbour_d <-  round(d[neighbours])

    # return minimum and maximum distance to neighbour and maximum available distance
    return(c(min_neighbour = min(neighbour_d), max_neighbour = max(neighbour_d), max_avl_dist = max(d)))
  }

  # message to the user
  if (!quiet) cat("Calculating distance parameters...", "\n")

  # calculate dist params with or without progress bar
  if (!progress_bar) {

    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo))
  }

  dist_params <- pbvapply(id_within, params_fun, numeric(3))


  # calculate dist_resolution - min distance between each neighbours
  dist_resolution <- round(min(dist_params["min_neighbour",]))

  # calculate dist_bin - half of the maximum distance between each neighbours
  dist_bin <- round(max(dist_params["max_neighbour",] / dist_resolution / 2))

  # check if dist_bin isn't too small
  if (dist_bin < 1) {
    stop("Your input maps have too high resolution. Consider using terra::aggregate() to change it.")

  }

  # calculate max_avl_dist - max distance between any grid cells divided by dist_resolution plus dist_bin
  max_avl_dist <- round(max(dist_params["max_avl_dist",]) / dist_resolution) + dist_bin

  return(c(dist_bin = dist_bin, dist_resolution = dist_resolution, max_avl_dist = max_avl_dist))
}


#' Count Cells On Every Distance - planar raster
#'
#' This internal function counts how many cells are reachable on each distance
#' from any cells of template `r`. It takes raster's dist_resolution into account.
#'
#' @param template template [`SpatRaster`][terra::SpatRaster-class] object
#' @param dist_resolution parameter calculated by [`calculate_dist_params`] function
#'
#' @return numeric vector; numbers of target cells on every possible distance
#' range
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
ncell_in_circle_planar <- function(template, dist_resolution) {

  # get the resolution
  res <- res(template)

  # get extents in both dimensions
  a <- xmax(template) - xmin(template)
  b <- ymax(template) - ymin(template)

  # calculate maximum possible distance
  max_dist <- round(sqrt(a**2 + b**2))

  # prep new extent
  e <- ext(
    xmin(template) - max_dist, xmax(template) + max_dist,
    ymin(template) - max_dist, ymax(template) + max_dist
  )

  # prep raster to calculate distances
  d <- extend(template, e)

  # prep the center (as vector)
  center_point <- cbind((e[2] - e[1]) / 2 + e[1], (e[4] - e[3]) / 2 + e[3])
  center_vect <- vect(center_point)
  crs(center_vect) <- crs(template)

  # calculate all distances
  d <- distance(d, center_vect, progress = 0)

  # adjust to dist_resolution
  d_cell <- as.matrix(round(d / dist_resolution))

  # number of cells on each distance
  ncells_in_circle <-  tabulate(d_cell)[1:(max_dist / dist_resolution)]

  return(ncells_in_circle)
}



#' Count Cells On Every Distance - lon/lat raster
#'
#' This internal function counts how many cells are reachable on each distance
#' from any cells of template `r`. It takes raster's dist_resolution adn dist_bin
#' into account.
#'
#' @param template template [`SpatRaster`][terra::SpatRaster-class] object
#' @param max_avl_dist numeric vector of length 1; max distance between any grid
#' cells divided by dist_resolution plus dist_bin; describes max available
#' distance achievable in given input maps
#' @inheritParams calc_dist
#' @inheritParams initialise
#'
#' @return numeric matrix with number of columns corresponding to id_within
#' and number of rows equal to max_avl_dist; numbers of target cells on
#' every possible distance from each cell;
#'
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
ncell_in_circle_lonlat <- function(template, dist_resolution, dist_bin, id_within, max_avl_dist, progress_bar, quiet) {

  # get extents in both dimensions
  xdiff <- xmax(template) - xmin(template)
  ydiff <- ymax(template) - ymin(template)

  # prep new extent
  multiplier <- 2
  e <- ext(
    xmin(template) - xdiff * multiplier, xmax(template) + xdiff * multiplier,
    ymin(template) - ydiff * multiplier, ymax(template) + ydiff * multiplier
  )
  # prep raster to calculate distances
  extended <- extend(template, e)


  # calculates how many cells are reachable on each distance from each grid cell
  circles_fun <- function(x) {

    # get current grid cell location
    xy <- vect(xyFromCell(template, x))
    crs(xy) <- crs(template)

    # calculate distance to each cell in extended raster from current grid cell
    d <- distance(extended, xy, progress = 0)
    d <- round(values(d / dist_resolution))

    # get only distances that are reachable in the input maps
    d <- d[d <= max_avl_dist]

    # number of cells on each distance
    d_table <-  tabulate(d)

    # available distances
    d_avl <- (1:length(d_table))[d_table != 0]

    # expand distances by dist_bin
    bin_start <- ifelse(d_avl - dist_bin + 1 < 0, 0, d_avl - dist_bin + 1)
    bin_stop <- d_avl + dist_bin

    ds <- unlist(lapply(seq_len(length(d_avl)), function(x) {
      rep(seq(bin_start[x], bin_stop[x]), times = d_table[x])
    }))

    # number of cells on each expanded distance
    circle <- tabulate(ds)[1:max_avl_dist]

    return(circle)
  }

  # message for user
  if (!quiet) cat("Calculating number of cells on each distance...\nThis step may take some time. Consider using border = \"reprising\" with lon/lat rasters if possible.", "\n")

  # calculate "circles" with or without progress bar
  if (!progress_bar) {

    pbo <- pboptions(type = "none")
    on.exit(pboptions(pbo))
  }

  circles <- pbvapply(id_within, circles_fun, numeric(max_avl_dist))

  # error if NA
  if (any(is.na(circles))) {
    stop("Your study area is too big for the \"absorbing\" border. Change the border parameter to \"reprising\".")
  }

  return(circles)
}


# helper function to get initialise call with info about dlist
get_initialise_call <- function(call) {

  if ("dlist" %in% names(call)) {
    call$dlist <- TRUE
  }

  return(call)
}
