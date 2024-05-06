
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
target_ids <- function(
    idx, id, data, min_dist_scaled, max_dist_scaled,
    dist_resolution, dist_bin, id_within) {


  # unwrap id raster
  id <- unwrap(id)

  # get coordinates of current cell
  xy_vect <- get_vect_from_xy(id, data, idx)

  # get available ids and their corresponding distances
  ids_ds <- get_ds(
    template = id,
    xy_vect,
    id_within,
    dist_resolution,
    ifelse(min_dist_scaled - dist_bin < 1, 1, min_dist_scaled - dist_bin),
    max_dist_scaled + dist_bin
  )

  if(is.null(ids_ds)) return(NULL)

  # use distance bins to expand distances for target cells
  # if distances between cells are not regular and if cells aren't squares
  if(dist_bin != 0)
    ids_ds <- get_bins(ids_ds[, 1], ids_ds[, 2], idx, dist_bin)


  # make list of target cells for each distance
  targets_list <- get_targets_list(
    dists = min_dist_scaled:max_dist_scaled,
    ids = ids_ds[, 1],
    ds = ids_ds[, 2])


  return(targets_list)
}





#' Title
#'
#' @param template
#' @param xy_cell
#' @param idx
#'
#' @return
#'
#' @noRd
#'
get_vect_from_xy <- function(template, xy_cell, idx = NULL) {

  if(!is.null(idx))
    xy_cell <- xy_cell[xy_cell[, "id"] == idx, ]

  xy_vect <- vect(cbind(xy_cell["x"], xy_cell["y"]))
  crs(xy_vect) <- crs(template)


  return(xy_vect)
}




#' Title
#'
#' @param template
#' @param xy_vect
#' @param id_within
#' @param dist_resolution
#' @param min_dist_scaled
#' @param max_dist_scaled
#'
#' @return
#'
#' @noRd
#'
get_ds <- function(template, xy_vect, id_within, dist_resolution,
                   min_dist_scaled, max_dist_scaled) {


  # calculate distances
  d <- distance(template, xy_vect, progress = 0)
  d <- round(c(as.matrix(d, wide = TRUE)) / dist_resolution)

  # check if cells are within study area and specified range
  d_within <- d[id_within]
  in_range <-
    d_within >= min_dist_scaled &
    d_within <= max_dist_scaled

  # check if such a cell exists
  if (!any(in_range)) {
    return(NULL)
  }

  # extract cells ids and distance at which they are
  ids <- id_within[in_range]
  ds <- d_within[in_range]

  return(matrix(c(ids, ds), ncol = 2))
}




#' Title
#'
#' @param ids
#' @param ds
#' @param idx
#' @param dist_bin
#'
#' @return
#'
#' @noRd
#'
get_bins <- function(ids, ds, idx, dist_bin) {

  if(!is.null(idx)) {
    ds <- c(0, ds)
    ids <- c(idx, ids)
  }


  bin_start <- ifelse(ds - dist_bin + 1 < 0, 0, ds - dist_bin + 1)
  bin_stop <- ds + dist_bin

  # update cells ids and distance at which they are
  ids <- rep(ids, times = bin_stop - bin_start + 1)
  # ids <- rep(ids, each = dist_bin + 1)

  ds <- unlist(lapply(seq_len(length(ds)), function(x) {
    seq(bin_start[x], bin_stop[x])
  }))

  return(matrix(c(ids, ds), ncol = 2))
}




#' Title
#'
#' @param dists
#' @param ids
#' @param ds
#'
#' @return
#'
#' @noRd
#'
get_targets_list <- function(dists, ids, ds) {

  targets_list <- lapply(dists, function(x) {
    out <- ids[ds == x]

    if (length(out) == 0) { # if there isn't any target cell return null
      out <- NULL
    }

    return(out)
  })

  return(targets_list)
}
