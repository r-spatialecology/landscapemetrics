#' LSI (landscape level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{LSI = \frac{E} {\min E}}
#' where \eqn{E} is the total edge length in cell surfaces and \eqn{\min E}
#' is the minimum total edge length in cell surfaces
#'
#' LSI is an 'Aggregation metric'. It is the ratio between the actual landscape edge length
#' and the hypothetical minimum edge length. The minimum edge length equals
#' the edge length if only one patch would be present.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one squared patch is present.
#' Increases, without limit, as the length of the actual edges increases, i.e.
#' the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_c_lsi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_lsi(landscape)
#'
#' @aliases lsm_l_lsi
#' @rdname lsm_l_lsi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_l_lsi <- function(landscape, directions) UseMethod("lsm_l_lsi")

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_l_lsi
#' @export
lsm_l_lsi.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_lsi_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if(class(landscape) != "matrix") {
        resolution <- raster::res(landscape)
        landscape <- raster::as.matrix(landscape)
    }

    # get total edge
    edge_landscape <- lsm_l_te_calc(landscape,
                                    count_boundary = TRUE,
                                    resolution = resolution)

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to total area in sqm
    total_area <- dplyr::summarise(patch_area, value = sum(value) * 10000)

    lsi <- dplyr::mutate(total_area,
                         n = trunc(sqrt(value)),
                         m = value - n^ 2,
                         minp = dplyr::case_when(
                             m == 0 ~ n * 4,
                             n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                             value > n * (1 + n) ~ 4 * n + 4),
                         value = edge_landscape$value / minp)

    tibble::tibble(
        level = "landscape",
        class = as.integer(edge_landscape$class),
        id = as.integer(edge_landscape$id),
        metric = "lsi",
        value = as.double(lsi$value)
    )

}
