#' RPD (landscape level)
#'
#' @description Relative patch richness (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param classes_max Potential maximum number of present classes
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{RPR = \frac{m} {m_{max}} * 100}
#' where \eqn{m} is the number of classes and \eqn{m_{max}} is the (theoretical)
#' maximum number of classes.
#'
#' RPR is an 'Diversity metric'. The metric calculates the percentage of present classes
#' in the landscape in relation to a (theoretical) number of maximum classes. The user has to
#' specify the maximum number of classes. Note, that if \code{classes_max} is not provided,
#' the functions returns \code{NA}.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Ranges}{0 < RPR <= 100}
#' \subsection{Behaviour}{Approaches RPR > 0 when only one class type is present,
#' but the maximum number of classes is large. Equals RPR = 100 when m = m_max}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_rpr(landscape, classes_max = 5)
#'
#' @aliases lsm_l_rpr
#' @rdname lsm_l_rpr
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Romme, W. H. 1982. Fire and landscapediversity in subalpine forests of
#' Yellowstone National Park.Ecol.Monogr. 52:199-221
#'
#' @export
lsm_l_rpr <- function(landscape, classes_max, verbose) UseMethod("lsm_l_rpr")


#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterLayer <- function(landscape, classes_max = NULL, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_rpr_calc,
                     classes_max = classes_max,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterStack <- function(landscape, classes_max = NULL, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_rpr_calc,
                     classes_max = classes_max,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterBrick <- function(landscape, classes_max = NULL, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_rpr_calc,
                     classes_max = classes_max,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.stars <- function(landscape, classes_max = NULL, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_rpr_calc,
                     classes_max = classes_max,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.list <- function(landscape, classes_max = NULL, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_rpr_calc,
                     classes_max = classes_max,
                     verbose = verbose)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_rpr_calc <- function(landscape, classes_max, verbose) {

    if(is.null(classes_max)) {

        if(isTRUE(verbose)) {
            warning("No maximum number of classes provided: RPR = NA", call. = FALSE)
        }
        rpr <- NA
    }

    else {

        pr <- lsm_l_pr_calc(landscape)

        rpr <- pr$value / classes_max * 100
    }

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "rpr",
        value = as.double(rpr)
    )
}
