# nolint start
#' srr_stats
#'
#' @srrstatsVerbose FALSE
#'
#' @srrstats {G1.1} Novel algorithm - mentioned in README
#' @srrstats {G1.2} Life Cycle Statement included in CONTRIBUTING.md
#' @srrstats {G1.3} Any statistical terminology is clarified in the documentation
#' @srrstats {G3.0} All numeric equality comparisons are made between methods should be documented (typically in examples or vignettes).
#' @srrstats {SP2.1} This package uses the terra package
#' @srrstats {SP2.2, SP2.2a, SP2.2b} Since this package is based on the terra package, it is compatible with it and other packages with which terra is compatible
#' @srrstats {SP2.4, SP2.4a} The terra package takes care of that
#' @srrstats {SP2.5} SpatRaster class from the terra package contain meta data on associated coordinate reference systems
#'
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.5} No performance claims were made in associated publications
#' @srrstatsNA {G1.6} No performance claims were made with alternative implementations in other R packages.
#' @srrstatsNA {G2.4} All exported function have assertion rules implemented so there is no need for any conversion
#' @srrstatsNA {G2.4a} There is no need for explicit conversion to `integer` via `as.integer()`
#' @srrstatsNA {G2.4b} There is no need for explicit conversion to continuous via `as.numeric()`
#' @srrstatsNA {G2.4c} There is no need for explicit conversion to character via `as.character()`
#' @srrstatsNA {G2.4d} There is no need for explicit conversion to factor via `as.factor()`
#' @srrstatsNA {G2.4e} There is no need for explicit conversion from factor via `as...()` functions
#' @srrstatsNA {G2.5} No inputs of `factor` type are expected
#' @srrstatsNA {G2.10} No single-columned tabular inputs are allowed
#' @srrstatsNA {G2.11} Only data.frames and matrices are allowed so as far as I understand this shouldn't be an issue
#' @srrstatsNA {G2.12} Only data.frames and matrices are allowed as far as I understand this shouldn't be an issue
#' @srrstatsNA {G2.14, G2.14b, G2.14c, G2.15} Data structures that this package operates on are mostly of SpatRaster class, where `NA` stands for cells that are outside the study area. Other input data are mostly univariate
#' @srrstatsNA {G3.1, G3.1a}  No covariance calculations are made
#' @srrstatsNA {G4.0} No outputs are written to local files
#' @srrstatsNA {G5.0} It would not be practical for tests to use standard data sets, as it would unnecessarily complicate them
#' @srrstatsNA {G5.4b} No existing methods
#' @srrstatsNA {G5.4c} Not applicable
#' @srrstatsNA {G5.6, G5.6a, G5.6b} There aren't any parameter recoveries in rangr
#' @srrstatsNA {G5.7} Not implemented yet
#' @srrstatsNA {G5.9, G5.9a, G5.9b} In my opinion such test doesn't make much sense in case of rangr
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} There are no extended tests in rangr
#' @srrstatsNA {SP2.5a} No new spatial classes are implemented
#' @srrstatsNA {SP3.0, SP3.0a, SP3.0b, SP3.1} No spatial neighbours considered
#' @srrstatsNA {SP3.2} No spatial sampling
#' @srrstatsNA {SP3.3} No spatial regression is implemented
#' @srrstatsNA {SP3.4} No spatial clustering is implemented
#' @srrstatsNA {SP3.5, SP3.6} No spatial machine learning is implemented
#' @srrstatsNA {SP5.1, SP5.2, SP5.3} No new spatial classes are implemented so the terra package takes care of that
#' @srrstatsNA {SP6.0} No transformation of coordinates implemented
#' @srrstatsNA {SP6.1, SP6.1a, SP6.1b} Only Cartesian data are allowed
#' @srrstatsNA {SP6.2} Such test are not necessary
#' @srrstatsNA {SP6.3, SP6.4} No spatial neighbours considered proximity.
#' @srrstatsNA {SP6.5} No clustering algorithms implemented
#' @srrstatsNA {SP6.6} No ML algorithm implemented
#'
#' @noRd
NULL
# nolint end
