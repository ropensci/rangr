#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the  within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.1} Novel algorithm - mentioned in README
#' @srrstats {G1.2} Life Cycle Statement included in CONTRIBUTING.md
#' @srrstats {G1.3} Any statistical terminology is clarified in the documentation
#' @srrstats {G3.0} All numeric equality comparisons are made between integersmethods should be documented (typically in examples or vignettes).*
#'
#'
#' Spatial Software
#' Spatial domain
#' @srrstatsTODO {SP1.0} *Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/rectilinear/geometric domains, curvilinear/geographic domains, or both.*
#' @srrstatsTODO {SP1.1} *Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality.*
#'
#' Input data structures and validation
#' @srrstatsTODO {SP2.0} *Spatial software should only accept input data of one or more classes explicitly developed to represent such data.*
#' @srrstatsTODO {SP2.0a} *Where new classes are implemented, conversion to other common classes for spatial data in R should be documented.*
#' @srrstatsTODO {SP2.0b} *Class systems should ensure that functions error appropriately, rather than merely warning, in response to data from inappropriate spatial domains.*
#' @srrstatsTODO {SP2.1} *Spatial Software should not use the [`sp` package](https://cran.r-project.org/package=sp), rather should use [`sf`](https://cran.r-project.org/package=sf).*
#' @srrstatsTODO {SP2.2} *Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through:*
#' @srrstatsTODO {SP2.2a} *Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on these established packages; and*
#' @srrstatsTODO {SP2.2b} *Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established packages.*
#' @srrstatsTODO {SP2.3} *Software which accepts spatial input data in any standard format established in other R packages (such as any of the formats able to be read by [`GDAL`](https://gdal.org), and therefore by the [`sf` package](https://cran.r-project.org/package=sf)) should include example and test code which load those data in spatial formats, rather than R-specific binary formats such as `.Rds`.*
#' @srrstatsTODO {SP2.4} *Geographical Spatial Software should be compliant with version 6 or larger of* [`PROJ`](https://proj.org/), *and with* `WKT2` *representations. The primary implication, described in detail in the articles linked to above, is that:*
#' @srrstatsTODO {SP2.4a} *Software should not permit coordinate reference systems to be represented merely by so-called "PROJ4-character vector of length 1s", but should use at least WKT2.*
#' @srrstatsTODO {SP2.5} *Class systems for input data must contain meta data on associated coordinate reference systems.*
#' @srrstatsTODO {SP2.5a} *Software which implements new classes to input spatial data (or the spatial components of more general data) should provide an ability to convert such input objects into alternative spatial classes such as those listed above.*
#' @srrstatsTODO {SP2.6} *Spatial Software should explicitly document the types and classes of input data able to be passed to each function.*
#' @srrstatsTODO {SP2.7} *Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstatsTODO {SP2.8} *Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.*
#' @srrstatsTODO {SP2.9} *The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or return values.*
#'
#'
#' Algorithms
#' @srrstatsTODO {SP3.0} *Spatial software which considers spatial neighbours should enable user control over neighbourhood forms and sizes. In particular:*
#' @srrstatsTODO {SP3.0a} *Neighbours (able to be expressed) on regular grids should be able to be considered in both rectangular only, or rectangular and diagonal (respectively "rook" and "queen" by analogy to chess).*
#' @srrstatsTODO {SP3.0b} *Neighbourhoods in irregular spaces should be minimally able to be controlled via an integer number of neighbours, an area (or equivalent distance defining an area) in which to include neighbours, or otherwise equivalent user-controlled value.*
#' @srrstatsTODO {SP3.1} *Spatial software which considers spatial neighbours should wherever possible enable neighbour contributions to be weighted by distance (or other continuous weighting variable), and not rely exclusively on a uniform-weight rectangular cut-off.*
#' @srrstatsTODO {SP3.2} *Spatial software which relies on sampling from input data (even if only of spatial coordinates) should enable sampling procedures to be based on local spatial densities of those input data.*
#' @srrstatsTODO {SP3.3} *Spatial regression software should explicitly quantify and distinguish autocovariant or autoregressive processes from those covariant or regressive processes not directly related to spatial structure alone.*
#'
#' Return Results
#' @srrstatsTODO {SP3.4} *Where possible, spatial clustering software should avoid using standard non-spatial clustering algorithms in which spatial proximity is merely represented by an additional weighting factor in favour of explicitly spatial algorithms.*
#' @srrstatsTODO {SP3.5} *Spatial machine learning software should ensure that broadcasting procedures for reconciling inputs of different dimensions are **not** applied*.
#' @srrstatsTODO {SP3.6} *Spatial machine learning software should document (and, where possible, test) the potential effects of different sampling procedures*
#' @srrstatsTODO {SP4.0} *Return values should either:*
#' @srrstatsTODO {SP4.0a} *Be in same class as input data, or*
#' @srrstatsTODO {SP4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstatsTODO {SP4.1} *Any aspects of input data which are included in output data (either directly, or in some transformed form) and which contain units should ensure those same units are maintained in return values.*
#' @srrstatsTODO {SP4.2} *The type and class of all return values should be explicitly documented.*
#'
#' Visualization
#' @srrstatsTODO {SP5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstatsTODO {SP5.1} *Implement appropriate placement of variables along x- and y-axes.*
#' @srrstatsTODO {SP5.2} *Ensure that axis labels include appropriate units.*
#' @srrstatsTODO {SP5.3} *Offer an ability to generate interactive (generally `html`-based) visualisations of results.*
#'
#'
#'
#' Testing
#' @srrstatsTODO {SP6.0} *Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.*
#' @srrstatsTODO {SP6.1} *All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both.*
#' @srrstatsTODO {SP6.1a} *Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.*
#' @srrstatsTODO {SP6.1b} *Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data.*
#' @srrstatsTODO {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.*
#' @srrstatsTODO {SP6.3} *Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative effects of different ways of defining neighbours.*
#' @srrstatsTODO {SP6.4} *Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity.*
#' @srrstatsTODO {SP6.5} *Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtained with a non-spatial clustering algorithm.*
#' @srrstatsTODO {SP6.6} *Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from the same spatial region, rather than from spatially distinct regions.
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
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
#' @srrstatsNA {G2.14, G2.14a, G2.14b, G2.15} Data structures that this package operates on are mostly of SpatRaster class, where `NA` stands for cells that are outside the study area. Other input data are mostly univariate
#' @srrstatsNA {G3.1, G3.1a}  No covariance calculations are made
#' @srrstatsNA {G4.0} No outputs are written to local files
#' @srrstatsNA {G5.0} It would not be practical for tests to use standard data sets, as it would unnecessarily complicate them
#' @srrstatsNA {G5.4b} No existing methods
#' @srrstatsNA {G5.4c} Not applicable
#' @srrstatsNA {G5.5} In case of rangr the correctness tests doesn't check the exact values
#' @srrstatsNA {G5.6, G5.6a, G5.6b} There aren't any parameter recoveries in rangr
#' @srrstatsNA {G5.7} Not implemented yet
#' @srrstatsNA {G5.9, G5.9a, G5.9b} In my opinion such test doesn't make much sense in case of rangr
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} There are no extended tests in rangr
#'
#'
#' @noRd
NULL
