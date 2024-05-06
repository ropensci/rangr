#' Simulating Dispersal
#'
#' The function simulates dispersal, for every grid cell calculating the number
#' of individuals that disperse out of this cell and the number of individuals
#' that disperse into this cell.
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
    border, planar, dist_resolution, max_dist, dist_bin, ncells_in_circle,
    cl = NULL) {

  # empty immigration/emigration matrices
  im <- em <- matrix(0L, nrow = nrow(N_t), ncol = ncol(N_t))

  # necessary variables
  id_ok_mask <- N_t > 0 & within_mask # cell ids where the species is present
  id_ok <- as.matrix(id, wide = TRUE)[id_ok_mask]
  N_pos <- N_t[id_ok_mask]
  disp_dist <- dists_tab(N_pos, kernel, dist_resolution)
  if (is.null(dlist)) dlist <- vector("list", length(id_within))

  # version of dispersal (linear vs. parallel calculations)
  if (is.null(cl)) {

    # cycle over non-empty grid cells
    disp_res <- lapply(
      seq_len(length(N_pos)), sq_disp,
      disp_dist = disp_dist,
      id_within = id_within,
      id_ok = id_ok,
      dlist = dlist,
      data_table = data_table,
      id = id,
      dist_resolution = dist_resolution,
      dist_bin = dist_bin,
      dens_dep = dens_dep,
      ncells_in_circle = ncells_in_circle,
      border = border,
      planar = planar
    )

  } else {

    # cycle over non-empty grid cells
    disp_res <- parLapply(
      cl, seq_len(length(N_pos)), sq_disp,
      disp_dist = disp_dist,
      id_within = id_within,
      id_ok = id_ok,
      dlist = dlist,
      data_table = data_table,
      id = wrap(id),
      dist_resolution = dist_resolution,
      dist_bin = dist_bin,
      dens_dep = dens_dep,
      ncells_in_circle = ncells_in_circle,
      border = border,
      planar = planar
    )
  }

  # fill immigration/emigration matrices
  for (i in seq_len(length(id_ok))) {
    targets <- disp_res[[i]]
    for (j in targets) {
      im[j] <- im[j] + 1L
    }

    em[id_ok[i]] <- em[id_ok[i]] + length(targets)

  }

  # return immigration/emigration matrices
  return(list(em = em, im = im))
}



# Internal functions for disp function -----------------------------------------


#' Distance For Every Individual
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
dists_tab <- function(N_pos, kernel, dist_resolution) {

  tab <- function(x) {
    dd <- round(kernel(x) / dist_resolution)
    dd_pos <- dd[dd > 0]
    tabd <- tabulate(dd_pos)
    tabd
  }

  lapply(N_pos, tab)
}



#' Dispersal From Non-Empty Grid Cells
#'
#' This function calculates more possible target cells available
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
    i, disp_dist, id_within, id_ok, dlist, data_table, id, dist_resolution,
    dist_bin, dens_dep, ncells_in_circle, border, planar) {

  # max dispersal distance out from the cell id_ok[i] (in raster units):
  nd <- length(disp_dist[[i]])

  # position of a target id-s stored in a list "dlist"
  # for the focal cell id_ok[i]
  pos <- which(id_within == id_ok[i])

  # no info in dlist about that particular cell OR beyond max_dist
  if ((no_info <- (pos > length(dlist))) || nd > length(dlist[[pos]])) {

    # calculate missing targets
    more_targets <- target_ids(
      idx = NULL,
      id = id,
      data = data_table[id_ok[i], 1:3],
      min_dist_scaled = ifelse(no_info, 1, length(dlist[[pos]]) + 1),
      max_dist_scaled = nd,
      dist_resolution = dist_resolution,
      dist_bin = dist_bin,
      id_within = id_within
    )

    dlist[[pos]][(length(dlist[[pos]]) + 1):nd] <- more_targets
  }

  if(!is.null(dim(ncells_in_circle))) {
    ncells_in_circle <- ncells_in_circle[, pos]
  }
  # cycle over j distances within the cell id_ok[i]
  to <- lapply(
    seq_len(nd), one_dist_sq_disp, id_ok[i], dlist[[pos]], disp_dist[[i]],
    data_table, dens_dep, ncells_in_circle, border
  )


  to <- unlist(to, use.names = FALSE)
  return(to)
}



#' Dispersal Simulation In One Grid Cell
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
    # tij target cells at the distance j from the focal cell id_ok[i]
    tij <- dlist_pos[[j]]

    Ks <- data_table[tij, "K"]
    Ns <- data_table[tij, "N"]

    # dij - number of individuals dispersing out from the cell id_ok[i]
    # at the distance j:
    if ((dij <- disp_dist_i[j]) == 0) {
      # if no individuals

      out <- NULL
    } else if (sum(Ks) == 0 && dens_dep != "none") {
      # if K in all target cells is 0 and dispersal is ~ to K -
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
