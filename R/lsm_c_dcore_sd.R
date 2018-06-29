#' DCORE_SD (class level)
#'
#' @description Standard deviation number of disjunct core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{DCORE_{SD} = sd(NCORE[patch_{ij}])}
#' where \eqn{NCORE[patch_{ij}]} is the number of core areas.
#'
#' DCORE_SD is an 'Core area metric'. It summarises each class as the standard deviation
#' of all patch areas belonging to class i. A cell is defined as core if the cell
#' has no neighbour with a different value than itself (rook's case). NCORE counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#' The metric describes the differences among patches of the same class i in
#' the landscape.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{DCORE_SD >= 0}
#' \subsection{Behaviour}{Equals DCORE_SD = 0 if all patches have the same number of disjunct
#' core areas. Increases, without limit, as the variation of number of disjunct corea areas
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_ncore}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_cv}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}}
#'
#' @return tibble
#'
#' @importFrom stats sd
#'
#' @examples
#' lsm_c_dcore_sd(landscape)
#'
#' @aliases lsm_c_dcore_sd
#' @rdname lsm_c_dcore_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_dcore_sd <- function(landscape) UseMethod("lsm_c_dcore_sd")

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_dcore_sd_calc <- function(landscape){
    dcore_sd <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(dcore_sd$class),
        id = as.integer(NA),
        metric = "number of core areas (sd)",
        value = as.double(dcore_sd$value)
    )
}
