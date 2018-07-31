#' ENN_SD (class level)
#'
#' @description Standard deviation of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{ENN_{SD} = sd(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_CV is an 'Aggregation metric'. It summarises each class as the standard
#' deviation of each patch belonging to class i. ENN measures the distance to the  nearest
#' neighbouring patch of the same class i. The distance is measured from edge-to-edge.
#' The range is limited by the cell resolution on the lower limit and the landscape extent
#' on the upper limit. The metric is a simple way to describe patch isolation. Because it is
#' scaled to the mean, it is easily comparable among different landscapes.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN_SD >= 0}
#' \subsection{Behaviour}{Equals ENN_SD = 0 if the euclidean nearest-neighbor distance is
#' identical for all patches. Increases, without limit, as the variation of ENN increases.}
#'
#' @seealso
#' \code{\link{lsm_p_enn}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_sd(landscape)
#'
#' @aliases lsm_c_enn_sd
#' @rdname lsm_c_enn_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_enn_sd <- function(landscape, directions) UseMethod("lsm_c_enn_sd")

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_enn_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_enn_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_enn_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_enn_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_sd_calc <- function(landscape, directions) {

    enn_sd  <- landscape %>%
        lsm_p_enn_calc(., directions = directions) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(enn_sd$class),
        id = as.integer(NA),
        metric = "enn_sd",
        value = as.double(enn_sd$value)
    )

}
