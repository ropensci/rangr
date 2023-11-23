#' Simulating dispersal
#'
#' The function simulates dispersal, for every square calculating the no. of
#' individuals that disperse out of this square and the no. of individuals that
#'  disperse into this square.
#'
#' This is the function used by [`sim`] internally and is not intended to be
#' called by the user. The parameters for this function are passed from
#' a `sim_data` object created by [`initialise`].
#'
#' Dispersal distance is expressed in original spatial units of the
#' [`SpatRaster`][terra::SpatRaster-class] provided to [`sim`] function
#' (`n1_map` and `K_map`), however, it is internally converted to units
#' of the simulation (i.e. to the size of a single cell) by calculating
#' `round(distance/resolution)`. Thus, if the selected dispersal distance is
#' smaller than `resolution/2`, it effectively means that an individual
#' does not disperse, i.e. it lands in the same cell as it was before.
#' The dispersal rate (proportion of dispersing individuals) can be estimated
#' from the dispersal kernel probability function by calculating the probability
#' that the dispersal distance is greater than `resolution/2`.
#'
#' @param N_t integer matrix of population numbers at a single time step;
#' NA stands for cells that are outside the study area
#' @param id [`SpatRaster`][terra::SpatRaster-class] object
#' (of the same size as `N_t`) with cell identifiers
#' @param data_table matrix that contains information about all cells
#' in current time points
#' @param kernel a function defining dispersal kernel
#' @param dlist a list with identifiers of target cells at a specified
#' distance from a focal cell
#' @param id_within integer vector with identifiers of cells inside the
#' study area
#' @param within_mask logical matrix that specifies boundaries of the study area
#' @param max_dist a distance (in the same units as used in the raster `id`)
#' specifying the maximum range at which identifiers of target dispersal cells
#' are determined in advance (see [`initialise`])
#' @param resolution integer vector of length 1; spatial resolution of
#' `id` raster
#' @param ncells_in_circle numeric vector; number of cells on each distance
#' @param cl if simulation is done in parallel, the name of a cluster object
#' created by [`makeCluster`][parallel::makeCluster()]
#' @inheritParams initialise
#' @return The function returns a list that contains two matrices:
#'
#' `em` - emigration matrix with the number of individuals that dispersed
#' from each cell
#' `im` - immigration matrix with the number of individuals that dispersed
#' to each cell
#'
#'
#' @export
#'
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
#' # disp
#' disp_output <- disp(
#'   N_t = sim_data$n1_map,
#'   id = sim_data$id,
#'   data_table = sim_data$data_table,
#'   kernel = sim_data$kernel,
#'   dens_dep = sim_data$dens_dep,
#'   dlist = sim_data$dlist,
#'   id_within = sim_data$id_within,
#'   within_mask = sim_data$within_mask,
#'   border = sim_data$border,
#'   max_dist = sim_data$max_dist,
#'   resolution = sim_data$resolution,
#'   ncells_in_circle = sim_data$ncells_in_circle
#' )
#'
#' # immigration and emigration matrices
#' names(disp_output)
#'
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, SP2.6} documented types expectation
#' @srrstats {SP2.3} load data in spatial formats
#'
#'
disp <- function(
    N_t, id, data_table, kernel, dens_dep, dlist, id_within, within_mask,
    border, max_dist, resolution, ncells_in_circle, cl = NULL) {

  # empty immigration/emigration matrices
  im <- em <- matrix(0L, nrow = nrow(N_t), ncol = ncol(N_t))

  # necessary variables
  id_ok_mask <- N_t > 0 & within_mask # square ids where the species is present
  id_ok <- as.matrix(id, wide = TRUE)[id_ok_mask]
  N_pos <- N_t[id_ok_mask]
  disp_dist <- dists_tab(N_pos, kernel, resolution)
  if (is.null(dlist)) dlist <- vector("list", length(id_within))

  # version of dispersal (linear vs. parallel calculations)
  if (is.null(cl)) {

    # cycle over non-empty squares
    disp_res <- lapply(
      seq_len(length(N_pos)), sq_disp,
      disp_dist = disp_dist,
      id_within = id_within,
      id_ok = id_ok,
      dlist = dlist,
      data_table = data_table,
      id = id,
      resolution = resolution,
      dens_dep = dens_dep,
      ncells_in_circle = ncells_in_circle,
      border = border
    )

  } else {

    # cycle over non-empty squares
    disp_res <- parLapply(
      cl, seq_len(length(N_pos)), sq_disp,
      disp_dist = disp_dist,
      id_within = id_within,
      id_ok = id_ok,
      dlist = dlist,
      data_table = data_table,
      id = wrap(id),
      resolution = resolution,
      dens_dep = dens_dep,
      ncells_in_circle = ncells_in_circle,
      border = border
    )
  }

  # fill immigration/emigration matrices
  for (i in seq_len(length(id_ok))) {
    targets <- disp_res[[i]]

    n_disp <- length(targets)
    em[id_ok[i]] <- em[id_ok[i]] + n_disp
    im[targets] <- im[targets] + 1L
  }

  # return immigration/emigration matrices
  return(list(em = em, im = im))
}



# Internal functions for disp function -----------------------------------------


#' Distance for every individual
#'
#' This function computes dispersal distance for every individual available
#' in the simulation. It uses `kernel` to do so. `dists_tab` returns a list
#' of vectors where list's indexes correspond to source cell, vector indexes
#' specify emigration distance and values - number of individuals.
#'
#' @param N_pos integer vector of length 1 or more; number of individuals in
#' every cell
#' (cases with zero individuals  aren't listed)
#' @inheritParams disp
#'
#' @return List of numeric vectors; number of emigrating individuals
#' from each cell to each distance.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
dists_tab <- function(N_pos, kernel, resolution) {

  tab <- function(x) {
    dd <- kernel(x)
    dd <- round(dd / resolution)
    dd <- dd[dd > 0]
    tabd <- tabulate(dd)
  }

  lapply(N_pos, tab)
}



#' Dispersal from non-empty square
#'
#' This function calculates more possible target squares available
#' from source cell `i` (if needed). Then, it uses [one_dist_sq_disp] function
#' to find cells that individuals will emigrate to.
#'
#' @param i integer vector of length 1; number of current source cell
#' in relation to `disp_dist`
#' @inheritParams disp_linear
#'
#' @return Indexes of cells that individuals emigrate to. One occurrence of
#' the particular cell equals to one specimen that emigrates to it.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#'
#' @noRd
#'
sq_disp <- function(
    i, disp_dist, id_within, id_ok, dlist, data_table, id, resolution, dens_dep,
    ncells_in_circle, border) {

  # max dispersal distance out from the square id_ok[i] (in raster units):
  nd <- length(disp_dist[[i]])

  # position of a target id-s stored in a list "dlist"
  # for the focal square id_ok[i]
  pos <- which(id_within == id_ok[i])

  # no info in dlist about that particular square OR beyond max_dist
  if ((no_info <- (pos > length(dlist))) || nd > length(dlist[[pos]])) {

    # calculate missing targets
    more_targets <- target_ids_in_disp(
      id_x_y = data_table[id_ok[i], 1:3],
      id = id,
      id_within = id_within,
      resolution = resolution,
      min = ifelse(no_info, 1, length(dlist[[pos]]) + 1),
      max = nd
    )

    dlist[[pos]][(length(dlist[[pos]]) + 1):nd] <- more_targets
  }

  # cycle over j distances within the square id_ok[i]
  to <- lapply(
    seq_len(nd), one_dist_sq_disp, id_ok[i], dlist[[pos]], disp_dist[[i]],
    data_table, dens_dep, ncells_in_circle, border
  )


  to <- unlist(to, use.names = FALSE)
  return(to)
}

#' Calculate missing indexes of target cells
#'
#' This internal function is used during dispersal process
#' in [sq_disp] function. It returns indexes of target cells that were not
#'  precalculated during initialisation.
#'
#' There are two possible reason for usage of [target_ids_in_disp].
#' First is that during simulation some individuals are dispersing beyond
#' specified distance threshold (`max_dist` parameter in [`initialise`]).
#' Second reason is that the user has chosen to not pre-calculate target cells
#' (by setting `calculate_dist` to`FALSE` in [`initialise`]).
#'
#' @param id_x_y integer vector; index and coordinates of source cell
#' @param id [`SpatRaster`][terra::SpatRaster-class] object; cells indexes
#' from `sim_data` object created by [`sim`]
#' @param id_within integer vector; indexes of cells inside study area
#' @param resolution integer vector of length 1; resolution
#' @param min integer vector of length 1; the closest distance for which
#' target cells will be returned
#' @param max integer vector of length 1; the farthest distance for which
#' target cells will be returned
#'
#' @return List of target cells for each distance or `NULL` if there isn't any
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {G2.1a, SP2.6} documented types expectation
#'
#' @noRd
#'
target_ids_in_disp <- function(id_x_y, id, id_within, resolution, min, max) {

  # get coordinates of given cell
  xy_i <- vect(cbind(id_x_y["x"], id_x_y["y"]))
  id <- unwrap(id)
  crs(xy_i) <- crs(id)

  # calculate distances
  d <- distance(id, xy_i, progress = 0)
  d <- round(c(as.matrix(d, wide = TRUE)) / resolution)

  # check if cells are within study area and specified range
  d_within <- d[id_within]
  in_range <- d_within >= min & d_within <= max

  # check if such a cell exists
  if (!any(in_range)) {
    return(NULL)
  }

  # extract cells ids and distance at which they are
  ids <- id_within[in_range]
  ds <- d_within[in_range]

  # all needed distances
  dists <- min:max

  # make list of target cells for each distance
  lapply(dists, function(x) {
    out <- ids[ds == x] # get ids on distance x

    if (length(out) == 0) { # if there isn't any target cell return null
      out <- NULL
    }

    return(out)
  })
}



#' Dispersal simulation in one square
#'
#' `one_dist_sq_disp` simulates dispersal in one cell to given distance
#'
#' @param j integer; distance
#' @param id_int integer; index of the cell from which the emigration takes
#' place
#' @param dlist_pos list of numeric vectors; target cells available
#' from current cell
#' @param disp_dist_i integer vector; number of individuals that disperse
#' on distance `j`
#' @inheritParams disp
#'
#' @return Indexes of cells at given distance `j` that individuals
#' emigrate to. One occurrence of the particular cell equals to
#' one specimen that emigrates to it.
#'
#'
#' @srrstats {G1.4a} uses roxygen documentation (internal function)
#'
#' @noRd
#'
one_dist_sq_disp <- function(j, id_int, dlist_pos, disp_dist_i, data_table,
                             dens_dep, ncells_in_circle, border) {

  if (j > length(dlist_pos) || is.null(dlist_pos[[j]])) {
    # no cell in study area available -> absorbed or stayed

    if (border == "absorbing") {
      out <- NULL
    } else {
      out <- rep(id_int, times = disp_dist_i[j])
    }
  } else {
    # tij target squares at the distance j from the focal square id_ok[i]
    tij <- dlist_pos[[j]]

    Ks <- data_table[tij, "K"]
    Ns <- data_table[tij, "N"]

    # dij - no. of individuals dispersing out from the square id_ok[i]
    # at the distance j:
    if ((dij <- disp_dist_i[j]) == 0) {
      # if no individuals

      out <- NULL
    } else if (sum(Ks) == 0 && dens_dep != "none") {
      # if K in all target squares is 0 and dispersal is ~ to K -
      # all individuals stay in current sq

      out <- rep(id_int, disp_dist_i[j])
    } else {
      if (border == "absorbing") {
        # if absorbing borders and dispersal out available

        if ((ncells_in_circle[j] - length(Ns)) > 0) {
          # pick
          dij <- sum(sample(
            c(TRUE, FALSE),
            dij,
            TRUE,
            c(length(Ns), ncells_in_circle[j] - length(Ns))
          ))
        }
      }

      p_weights <- switch(dens_dep,
                          none = NULL, # evenly
                          K = Ks / sum(Ks), # proportionally to K
                          K2N = { # proportionally to K/N
                            w <- Ks / (Ns + 1)
                            w / sum(w)
                          }
      )

      if (length(tij) == 1) {
        out <- rep(tij, dij)
      } else {
        out <- c(sample(tij, dij, replace = TRUE, prob = p_weights))
      }
    }
  }

  return(out)
}
