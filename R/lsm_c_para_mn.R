#' PARA_MN (class level)
#'
#' @description Mean perimeter-area ratio (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PARA_{MN} = mean(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_MN is a 'Shape metric'. It summarises each class as the mean of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_MN > 0}
#' \subsection{Behaviour}{Approaches PARA_MN > 0 if PARA for each patch approaches PARA > 0,
#' i.e. the form approaches a rather small square. Increases, without limit, as PARA increases,
#' i.e. patches become more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_mn(landscape)
#'
#' @aliases lsm_c_para_mn
#' @rdname lsm_c_para_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_para_mn <- function(landscape) UseMethod("lsm_c_para_mn")

#' @name lsm_c_para_mn
#' @export
lsm_c_para_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_para_mn
#' @export
lsm_c_para_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_mn
#' @export
lsm_c_para_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_mn
#' @export
lsm_c_para_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_para_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_para_mn_calc <- function(landscape){

    para_mn <- landscape %>%
        lsm_p_para_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(para_mn$class),
        id = as.integer(NA),
        metric = "perimeter-area-ratio (cv)",
        value = as.double(para_mn$value)
    )
}
