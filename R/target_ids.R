
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





#' Create SpatVector
#'
#' Creates [`SpatVector`][terra::SpatVector-class] from given data.
#'
#' @param template template [`SpatRaster`][terra::SpatRaster-class] object
#' @param xy_cell numeric matrix with 4 columns: "id", "x", "y", "dist" or named numeric vector with 4 elements: "id", "x", "y", "dist"
#'
#' @inheritParams target_ids
#'
#' @return [`SpatVector`][terra::SpatVector-class] object
#'
#' @noRd
#'
get_vect_from_xy <- function(template, xy_cell, idx = NULL) {

  if(!is.null(idx)) {
    # extract row with cell specified by idx
    xy_cell <- xy_cell[xy_cell[, "id"] == idx, ]
  }

  # transform coordinates to vector
  xy_vect <- vect(cbind(xy_cell["x"], xy_cell["y"]))
  # assign templates crs to vector
  crs(xy_vect) <- crs(template)


  return(xy_vect)
}




#' Get Available Ids And Their Corresponding Distances
#'
#' @param xy_vect [`SpatVector`][terra::SpatVector-class] object; represents current cell
#' @param dist_resolution integer vector of length 1; dimension of one side of
#' one cell of `id`; in case of an irregular grid or lon/lat raster it is
#' calculated by [`calculate_dist_params`]
#' @param min_dist_scaled integer vector of length 1; minimum distance to calculate target cell scaled by `dist_resolution`
#' @param max_dist_scaled integer vector of length 1; maximum distance to calculate target cell scaled by `dist_resolution`
#'
#' @inheritParams get_vect_from_xy
#'
#' @return numeric matrix with 2 columns representing: cells ids and distances
#' at which they are
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




#' Add Distance Bins To Distances
#'
#' Used for planar rasters with irregular resolution or lon/lat rasters.
#'
#' @param ids numeric vector; cells ids
#' @param ds numeric vector; cells distances
#' @param idx numeric vector of length 1; current source cell
#' @param dist_bin numeric vector of length 1; distance bin size
#'
#' @return numeric matrix with 2 columns representing: cells ids and distances
#' at which they are
#'
#' @noRd
#'
get_bins <- function(ids, ds, idx, dist_bin) {

  # if current id id provided - add it to ds and ids
  if(!is.null(idx)) {
    ds <- c(0, ds)
    ids <- c(idx, ids)
  }

  # calculated starts and stops of the expanded distances
  bin_start <- ifelse(ds - dist_bin + 1 < 0, 0, ds - dist_bin + 1)
  bin_stop <- ds + dist_bin

  # update cells ids and distance at which they are
  ids <- rep(ids, times = bin_stop - bin_start + 1)

  ds <- unlist(lapply(seq_len(length(ds)), function(x) {
    seq(bin_start[x], bin_stop[x])
  }))

  return(matrix(c(ids, ds), ncol = 2))
}




#' Get Cells Ids At Each Distance
#'
#' @param dists numeric vector; distances for which cells ids are supposed
#' to be returned
#' @param ids numeric vector; cells ids
#' @param ds numeric vector; cells distances
#'
#' @return list of the same length as `dists`, each element of this list
#' contains numeric vector with cells ids available at the corresponding
#' distance
#'
#' @noRd
#'
get_targets_list <- function(dists, ids, ds) {

  # loop through distances
  targets_list <- lapply(dists, function(x) {
    # select indexes at distance x
    out <- ids[ds == x]

    if (length(out) == 0) {
      # if there isn't any target cell - return null
      out <- NULL
    }

    return(out)
  })

  return(targets_list)
}
