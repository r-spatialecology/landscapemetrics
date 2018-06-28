#' PARA_CV (class level)
#'
#' @description Coeffiecent of variation perimeter-area ratio (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PARA_{CV} = cv(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_CV is a 'Shape metric'. It summarises each class as the coeffiecent of variation of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_CV >= 0}
#' \subsection{Behaviour}{Equals PARA_CV = 0 if the perimeter-area ratio is identical for
#' all patches. Increases, without limit, as the variation of the perimeter-area ratio
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_cv(landscape)
#'
#' @aliases lsm_c_para_cv
#' @rdname lsm_c_para_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_para_cv <- function(landscape) UseMethod("lsm_c_para_cv")

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_cv
#' @export
lsm_c_para_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_para_cv_calc <- function(landscape){

    para_cv <- landscape %>%
        lsm_p_para_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(para_cv$class),
        id = as.integer(NA),
        metric = "perimeter-area-ratio (cv)",
        value = as.double(para_cv$value)
    )
}
