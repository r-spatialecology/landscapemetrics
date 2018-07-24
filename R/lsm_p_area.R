#' AREA (patch level)
#'
#' @description Patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA = a_{ij} * (\frac{1} {10000})}
#' where \eqn{a_{ij}} is the area in square meters.
#'
#' AREA is an 'Area and edge metric' and equals the area of each patch in hectares.
#' The lower limit of AREA is limited by the resolution of the input raster,
#' i.e. AREA can't be smaller than the resolution squared (in hectares). It is one of
#' the most basic, but also most important metrics, to characterise a landscape. The
#' metric is the simplest measure of composition.
#'
#' \subsection{Units}{hectares}
#' \subsection{Range}{AREA > 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch size increases.}
#'
#' @seealso
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}},
#' \code{\link{lsm_c_ca}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}},
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_area(landscape)
#'
#' @aliases lsm_p_area
#' @rdname lsm_p_area
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_area <- function(landscape, directions) UseMethod("lsm_p_area")

#' @name lsm_p_area
#' @export
lsm_p_area.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_area_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_area
#' @export
lsm_p_area.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_area_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_area
#' @export
lsm_p_area.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_area_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_area
#' @export
lsm_p_area.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_p_area_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_area_calc <- function(landscape, directions){

    landscape_labelled <- cclabel(landscape, directions = directions)

    area_patch <- purrr::map_dfr(landscape_labelled, function(patches_class){

        area_patch_ij <- rcpp_get_composition_vector(
            x = raster::as.matrix(patches_class)) *
            prod(raster::res(patches_class)) / 10000

        class <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        tibble::tibble(
            class = as.integer(class),
            value = area_patch_ij
        )
    })

    tibble::tibble(
        level = "patch",
        class = as.integer(area_patch$class),
        id = as.integer(seq_len(nrow(area_patch))),
        metric = "area",
        value = as.double(area_patch$value)
    )
}


