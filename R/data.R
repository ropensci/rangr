#' @name K_small.tif
#' @title Example Of Carrying Capacity Map (Small)
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
#' @title Example Of Changing Carrying Capacity Maps (Small)
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
#' @title Example Of Carrying Capacity Map (Big)
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
#' @title Example Of Abundance Map At First Time Step Of The Simulation (Small)
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
#' @title Example Of Abundance Map At First Time Step Of The Simulation (Big)
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

#' @name K_small_lon_lat.tif
#' @title Example Of Carrying Capacity Map (Small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object that represents a carrying
#' capacity map projected to WGS 84 (CRS84) from the original raster `K_small`.
#' This map can be used as a carrying capacity map to initialise data necessary
#' to perform a simulation. It is compatible with the `n1_small_lon_lat.tif` raster.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 12 rows
#' and 14 columns containing integer values 0-100 and NA's
#' (indicating unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_small_lon_lat.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL


#' @name K_small_changing_lon_lat.tif
#' @title Example Of Changing Carrying Capacity Maps (Small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object representing changing carrying
#' capacity maps projected to WGS 84 (CRS84) from the original raster
#' `K_small_changing`. These maps can be used as carrying capacity maps to initialise
#' data necessary to perform a simulation. To use these maps in the initialization
#' process, the user first has to use [`K_get_interpolation`] to generate a map for
#' every time step of the simulation. These maps are compatible with the
#' `n1_small_lon_lat.tif` raster.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 3 layers,
#' each having 12 rows and 14 columns containing integer values 0-170 and NA's
#' (indicating unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_small_changing_lon_lat.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL


#' @name K_big_lon_lat.tif
#' @title Example Of Carrying Capacity Map (Big)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object representing a carrying
#' capacity map projected to WGS 84 (CRS84) from the original raster `K_big`.
#' This map can be used as a carrying capacity map to initialise data necessary
#' to perform a simulation. It is compatible with the `n1_big_lon_lat.tif` raster.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 74 rows
#' and 125 columns containing integer values 0-25 and NA's
#' (indicating unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#' (using spatial autocorrelation)
#'
#' @examples
#' system.file("input_maps/K_big_lon_lat.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
NULL


#' @name n1_small_lon_lat.tif
#' @title Example Of Abundance Map At First Time Step Of The Simulation (Small)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object representing an abundance map
#' at the first time step of the simulation projected to WGS 84 (CRS84) from the
#' original raster `n1_small`. This map can be used as a simulation starting point
#' to initialise data necessary to perform a simulation. It is compatible with the
#' `K_small_lon_lat.tif` and `K_small_changing_lon_lat.tif` maps.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 12 rows
#' and 14 columns containing integer values 0-10 and NA's
#' (indicating unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#'
#' @examples
#' system.file("input_maps/n1_small_lon_lat.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
NULL


#' @name n1_big_lon_lat.tif
#' @title Example Of Abundance Map At First Time Step Of The Simulation (Big)
#' @docType data
#'
#' @description
#' [`SpatRaster`][terra::SpatRaster-class] object representing an abundance map
#' at the first time step of the simulation projected to WGS 84 (CRS84) from the
#' original raster `n1_big`. This map can be used as a simulation starting point
#' to initialise data necessary to perform a simulation. It is compatible with the
#' `K_big_lon_lat.tif` map.
#'
#' @format [`SpatRaster`][terra::SpatRaster-class] object with 74 rows
#' and 125 columns containing integer values 0-50 and NA's
#' (indicating unsuitable areas).
#'
#' @source Data generated in-house to serve as an example
#'
#' @examples
#' system.file("input_maps/n1_big_lon_lat.tif", package = "rangr")
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G5.1} dataset used in examples is exported and documented
NULL


#' @title Example Of Observation Points List
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
