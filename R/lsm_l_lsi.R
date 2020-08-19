#' LSI (landscape level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
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
lsm_l_lsi <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_lsi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_lsi_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "lsi",
                              value = as.double(NA)))
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
    total_area <- sum(patch_area$value) * 10000

    n <- trunc(sqrt(total_area))
    m <- total_area - n^2

    min_p <- ifelse(test = m == 0,
                    yes = n * 4,
                    no = ifelse(test =  n ^ 2 < total_area & total_area <= n * (1 + n),
                                yes = 4 * n + 2,
                                no = ifelse(test = total_area > n * (1 + n),
                                            yes = 4 * n + 4,
                                            no = NA)))

    # warning if NA is introduced
    if (anyNA(min_p)) {
        warning("NA introduced by lsm_l_lsi", call. = FALSE)
    }

    lsi <- edge_landscape$value / min_p

    return(tibble::tibble(level = "landscape",
                          class = as.integer(edge_landscape$class),
                          id = as.integer(edge_landscape$id),
                          metric = "lsi",
                          value = as.double(lsi)))
}
