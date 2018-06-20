#' AREA_SD (class level)
#'
#' @description Standard deviation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#'
#' @details
#' \deqn{AREA_{SD} = sd(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_SD is an 'Area and Edge metric'. The metric summarises each class
#' as the standard deviation of all patch areas belonging to class i. The metric describes the differences among patches of the same class i in
#' the landscape.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_SD >= 0}
#' \subsection{Behaviour}{Equals AREA_SD = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_cv}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_sd(landscape)
#'
#' @aliases lsm_c_area_sd
#' @rdname lsm_c_area_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_area_sd <- function(landscape) UseMethod("lsm_c_area_sd")

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_sd_calc <- function(landscape){
    area_sd <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(area_sd$class),
        id = as.integer(NA),
        metric = "patch area (sd)",
        value = as.double(area_sd$value)
    )
}
