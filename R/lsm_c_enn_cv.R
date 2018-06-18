#' Euclidean Nearest Neighbor Distance Distribution (class level)
#'
#' @description Coeffiecent of variation Euclidean Nearest Neighbor Distance (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the coeffiecent of variation of the eclidean nearest neighbor distance of class i.
#' ENN equals the distance the the nearest neigbouring patch of the same patch type
#' (shortest edge-to-edge distance)
#' \deqn{ENN_CV = cv(ENN[patch_i])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_cv(landscape)
#'
#' @aliases lsm_c_enn_cv
#' @rdname lsm_c_enn_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_enn_cv <- function(landscape) UseMethod("lsm_c_enn_cv")

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_cv
#' @export
lsm_c_enn_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_enn_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_cv_calc <- function(landscape) {

    enn_cv  <- lsm_p_enn_calc(landscape) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(enn_cv)),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (cv)",
        value = enn_cv$value
    )

}
