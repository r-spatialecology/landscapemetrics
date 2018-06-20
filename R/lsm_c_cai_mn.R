#' CAI_MN (class level)
#'
#' @description Mean of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CAI_{MN} = mean(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_MN is a 'Core area metric'. The metric summarises each class
#' as the mean of the core area index of all patches belonging to class i.
#' The core area index is the percentag of core area in relation to patch area.
#' A cell is defined as core area if the cell has no neighbour with a different
#' value than itself (rook's case).
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_MN >= 0}
#' \subsection{Behaviour}{Equals CAI_MN = 0 if CAI = 0 for all patches. Increases,
#' without limit, as the core area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_mn(landscape)
#'
#' @aliases lsm_c_cai_mn
#' @rdname lsm_c_cai_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cai_mn <- function(landscape) UseMethod("lsm_c_cai_mn")

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_cai_mn_calc <- function(landscape){
    cai_mean <- landscape %>%
        lsm_p_cai_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(cai_mean$class),
        id = as.integer(NA),
        metric = "core area index (mean)",
        value = as.double(cai_mean$value)
    )
}
