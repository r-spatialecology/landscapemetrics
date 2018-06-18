#' Euclidean Nearest Neighbor Distance Distribution (landscape level)
#'
#' @description Coefficent of variation Euclidean Nearest Neighbor Distance (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the coefficent of variation of the eclidean nearest neighbor distance of all patches
#' in the landscape. ENN equals the distance the the nearest neigbouring patch of the same
#' patch type (shortest edge-to-edge distance)
#' \deqn{ENN_CV = cv(ENN[patch])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_enn_cv(landscape)
#'
#' @aliases lsm_l_enn_cv
#' @rdname lsm_l_enn_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_enn_cv <- function(landscape) UseMethod("lsm_l_enn_cv")

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_enn_cv
#' @export
lsm_l_enn_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_enn_cv_calc <- function(landscape) {

    enn_cv  <- lsm_p_enn(landscape) %>%
        dplyr::summarize(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (cv)",
        value = enn_cv$value
    )

}
