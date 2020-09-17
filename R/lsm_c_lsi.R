#' LSI (class level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{LSI = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces
#'
#' LSI is an 'Aggregation metric'. It is the ratio between the actual edge length of
#' class i and the hypothetical minimum edge length of class i. The minimum edge length equals
#' the edge length if class i would be maximally aggregated.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one squared patch is present or all
#' patches are maximally aggregated. Increases, without limit, as the length of the
#' actual edges increases, i.e. the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_l_lsi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lsi(landscape)
#'
#' @aliases lsm_c_lsi
#' @rdname lsm_c_lsi
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
lsm_c_lsi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_lsi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_lsi_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "lsi",
                              value = as.double(NA)))
    }

    # get class edge
    class_edge <- lsm_c_te_calc(landscape,
                                directions = directions,
                                count_boundary = TRUE,
                                resolution = resolution)

    # get patch area
    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise to class area in sqm
    class_area <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2],
                                   FUN = function(x) sum(x) * 10000)

    # calculate lsi index
    class_area$n <- trunc(sqrt(class_area$value))
    class_area$m <- class_area$value - class_area$n ^ 2
    class_area$minp <- ifelse(test = class_area$m == 0,
                              yes = class_area$n * 4,
                              no = ifelse(test = class_area$n ^ 2 < class_area$value & class_area$value <= class_area$n * (1 + class_area$n),
                                          yes = 4 * class_area$n + 2,
                                          no = ifelse(test = class_area$value > class_area$n * (1 + class_area$n),
                                                      yes = 4 * class_area$n + 4,
                                                      no = NA)))

    # test if any NAs introduced
    if (anyNA(class_area$minp)) {
        warning("NAs introduced by lsm_c_lsi", call. = FALSE)
    }

    lsi <- class_edge$value / class_area$minp

    tibble::tibble(
        level = "class",
        class = as.integer(class_edge$class),
        id = as.integer(class_edge$id),
        metric = "lsi",
        value = as.double(lsi)
    )
}
