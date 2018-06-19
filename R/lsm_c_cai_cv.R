#' CAI_CV
#'
#' @description Coefficient of variation of the core area index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CAI_CV = cv(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch
#'
#' CAI_CV is a 'Core area metric'. The metric summarises each class
#' as the coeffiecent of variation of the core area index of all patches
#' belonging to class i. The core area index is the percentag of core area
#' in relation to patch area
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_CV >= 0}
#' \subsection{Behaviour}{Increases as the variation of the core area indices increases}
#'
#' @seealso \code{\link{lsm_p_cai}} and \code{\link{cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_cv(landscape)
#'
#' @aliases lsm_c_cai_cv
#' @rdname lsm_c_cai_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cai_cv <- function(landscape) UseMethod("lsm_c_cai_cv")

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cai_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cai_cv_calc <- function(landscape){
    cai_cv <- landscape %>%
        lsm_p_cai_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = cai_cv$class,
        id = as.integer(NA),
        metric = "core area index (cv)",
        value = cai_cv$value
    )
}
