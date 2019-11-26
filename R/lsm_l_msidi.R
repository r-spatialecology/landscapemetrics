#' MSIDI (landscape level)
#'
#' @description Modified Simpson's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MSIDI = -\ln \sum \limits_{i = 1}^{m} P_{i}^{2}}
#' where \eqn{P_{i}} is the landscape area proportion of class i.
#'
#' MSIDI is a 'Diversity metric'.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{MSIDI >= 0}
#' \subsection{Behaviour}{MSIDI = 0 if only one patch is present and increases, without
#' limit, as the amount of patches with equally distributed landscape proportions increases}
#'
#' @seealso
#' \code{\link{lsm_l_sidi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_msidi(landscape)
#'
#' @aliases lsm_l_msidi
#' @rdname lsm_l_msidi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' Pielou, E. C. 1975. Ecological Diversity. Wiley-Interscience, New York.
#'
#' Romme, W. H. 1982. Fire and landscapediversity in subalpine forests of
#' Yellowstone National Park.Ecol.Monogr. 52:199-221
#'
#' @export
lsm_l_msidi <- function(landscape, directions) UseMethod("lsm_l_msidi")

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_msidi_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (class(landscape) != "matrix") {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "msidi",
                              value = as.double(NA)))
    }

    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    msidi <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2], FUN = sum)

    msidi <- -log(sum((msidi$value / sum(msidi$value)) ^ 2))

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "msidi",
                          value = as.double(msidi)))
}
