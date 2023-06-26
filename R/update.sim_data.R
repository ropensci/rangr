#' Update `sim_data` object
#'
#' @param object `sim_data` object; returned by [`initialise`] function
#' @param ... further arguments passed to or from other methods;
#' currently none specified
#' @param evaluate logical vector of length 1 if `TRUE` evaluate the new call
#' else return the call
#'
#' @return If `evaluate = TRUE` the updated `sim_data` object,
#' otherwise the updated call.
#'
#' @export
#' @method update sim_data
#'
#' @examples
#' \dontrun{
#'
#' # data preparation
#' library(terra)
#' n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
#' K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))
#'
#' sim_data_1 <- initialise(
#'   n1_map = n1_small,
#'   K_map = K_small,
#'   r = log(2),
#'   rate = 1 / 1e3
#' )
#' summary(sim_data_1)
#'
#' sim_data_2 <- update(sim_data_1, max_dist = 3000)
#' summary(sim_data_2)
#'
#'}
#'
#' @srrstats {G1.4} uses roxygen documentation
#' @srrstats {G2.0a} documented lengths expectation
#' @srrstats {SP2.3} load data in spatial formats
#'
update.sim_data <- function(object, ..., evaluate = TRUE) {

  # get call
  if (is.null(call <- getCall(object))) {
    stop("Need an object with call component")
  }

  # get parameters to update
  extras <- match.call(expand.dots = FALSE)$...


  if (length(extras)) { # if anything to update

    # get parameters of current kernel function
    kernel_args <- formalArgs(object$kernel_fun)[-1]

    # if kernel to update - remove old kernel parameters from call
    if ("kernel_fun" %in% names(extras)) {
      call[kernel_args] <- NULL
    }

    # check if parameters to update are present in call
    existing <- !is.na(match(names(extras), names(call)))

    # for present ones - update them
    for (a in names(extras)[existing]) {
      call[[a]] <- extras[[a]]
    }

    # transform call to a list
    call <- c(as.list(call))

    # for absent parameters - add them
    if (any(!existing)) {
      call <- c(call, extras[!existing])
    }


    # dlist: given, inherited or to calculate
    if (!"dlist" %in% names(extras)) {
      rm_old_dlist <- any(!is.na(match(
        names(extras),
        c(
          "max_dist", "kernel_fun", kernel_args
        )))) |
        sum(!is.na(match(
          names(extras),
          c(
            "n1_map", "K_map"
          )
        )), na.rm = TRUE) == 2

      if (rm_old_dlist) {
        call$dlist <- NULL
      } else {
        call$dlist <- object$dlist
      }
    }

    # transform call to call object
    call <- as.call(call)



    # evaluate or return the call
    if (evaluate) {
      eval(call, parent.frame())
    } else {
      return(call)
    }
  } else { # if nothing to update

    warning("Nothing to update")
    object
  }
}
