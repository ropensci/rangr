#' @name K_small.tif
#' @title Example of carrying capacity map (small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that can be used as carrying
#' capacity map to [`initialise`] data necessary to perform a [`sim`]ulation.
#' This map is compatible with [`n1_small.tif`]
#' - [`SpatRaster`][terra::SpatRaster-class] object that can be used
#' as simulation starting point.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 15 rows
#' and 10 columns containing integer values 0-100 and NA's
#' (that indicates unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_small.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL


#' @name K_small_changing.tif
#' @title Example of changing carrying capacity maps (small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that can be used as carrying
#' capacity maps to [`initialise`] data necessary to perform a [`sim`]ulation.
#' To use those maps in [`initialise`], the user first have to
#' use [`K_get_interpolation`] to generate a map for every time step
#' of the simulation.
#' These maps are compatible with [`n1_small.tif`]
#' - [`SpatRaster`][terra::SpatRaster-class] object that can be used
#' as simulation starting point.
#' Each subsequent map contains a virtual environment with greater
#' carrying capacity than the previous one.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 3 layers,
#' each has 15 rows and 10 columns containing integer values 0-170 and
#' NA's (that indicates unsuitable areas).
#'
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_small_changing.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL


#' @name K_big.tif
#' @title Example of carrying capacity map (big)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that can be used as
#' carrying capacity map to [`initialise`] data necessary
#' to perform a [`sim`]ulation. This map is compatible with [`n1_big.tif`]
#' - [`SpatRaster`][terra::SpatRaster-class] object that can be used
#'   as simulation starting point.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 100 rows
#' and 100 columns containing integer values 0-25 and NA's
#' (that indicates unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_big.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
NULL



#' @name n1_small.tif
#' @title Example of abundance map at first time step of the simulation (small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that can be used
#' as simulation starting point to [`initialise`] data necessary
#' to perform a [`sim`]ulation. This map is compatible with [`K_small.tif`]
#' and [`K_small_changing.tif`] maps.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 15 rows
#' and 10 columns containing integer values 0-10 and NA's
#' (that indicates unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#'
#' @examples
#' system.file("input_maps/n1_small.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
NULL


#' @name n1_big.tif
#' @title Example of abundance map at first time step of the simulation (big)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that can be used
#' as simulation starting point to [`initialise`] data necessary
#' to perform a [`sim`]ulation. This map is compatible with [`K_big.tif`] map.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 100 rows
#' and 100 columns containing integer values 0-50 and NA's
#' (that indicates unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#'
#' @examples
#' system.file("input_maps/n1_big.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL



#' @title Example of observation points list
#'
#' @description
#' A `data.frame` containing a sample input data to the function
#' [`get_observations`]  (while `type` argument is set to "from_file").
#' This data is compatible with [`n1_small.tif`]
#' and [`K_small.tif`]/[`K_small_changing.tif`] maps.
#'
#' @format A data frame with 1500 rows and 3 variables:
#' \describe{
#'   \item{x}{x coordinate}
#'   \item{y}{y coordinate}
#'   \item{time_step}{time_step in which the abundances should be observed}
#' }
#'
#' @source Data generated in-house to serve as an example
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
"observations_points"
