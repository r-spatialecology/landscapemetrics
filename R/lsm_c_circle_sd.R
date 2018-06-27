#' CIRCLE_SD (Class level)
#'
#' @description Standard deviation of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CIRCLE_{SD} = sd(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_SD is a 'Shape metric' and summarises each class as the standard deviation of
#' the related circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. The metric describes the differences
#' among patches of the same class i in the landscape.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_SD >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_SD if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_sd(landscape)
#'
#' @aliases lsm_c_circle_sd
#' @rdname lsm_c_circle_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_circle_sd <- function(landscape) UseMethod("lsm_c_circle_sd")

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_sd
#' @export
lsm_c_circle_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_circle_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_circle_sd_calc <- function(landscape) {

    circle_sd  <- landscape %>%
        lsm_p_circle_calc() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(circle_sd$class),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = as.double(circle_sd$value)
    )

}

