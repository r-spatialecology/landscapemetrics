#' SHAPE_SD (class level)
#'
#' @description Standard deviation shape index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE_{SD} = sd(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_SD is a 'Shape metric'. Each class is summarised as the standard deviation
#' of each patch belonging to class i. SHAPE describes the ratio between the actual perimeter
#' of the patch and the hypothetical minimum perimeter of the patch. The minimum perimeter
#' equals the perimeter if the patch would be maximally compact.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE_SD >= 0}
#' \subsection{Behaviour}{Equals SHAPE_SD = 0 if all patches have an identical shape index.
#' Increases, without limit, as the variation of the shape index increases.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_cv}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_shape_sd(landscape)
#'
#' @aliases lsm_c_shape_sd
#' @rdname lsm_c_shape_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_shape_sd <- function(landscape, directions) UseMethod("lsm_c_shape_sd")

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_shape_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_shape_sd
#' @export
lsm_c_shape_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_shape_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_shape_sd_calc <- function(landscape, directions){

    shape_sd <- landscape %>%
        lsm_p_shape_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(shape_sd$class),
        id = as.integer(NA),
        metric = "shape_sd",
        value = as.double(shape_sd$value)
    )
}
