#' TE (landscape level)
#'
#' @description Total edge (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Include landscape boundary in edge length
#'
#' @details
#' \deqn{TE = \sum \limits_{k = 1}^{m} e_{ik}}
#' where \eqn{e_{ik}} is the edge lengths in meters.

#' TE is an 'Area and edge metric'. Total edge includes all edges. It measures the
#' configuration of the landscape because a highly fragmentated landscape will have many
#' edges. However, total edge is an absolute measure, making comparisons among landscapes
#' with different total areas difficult. If \code{cound_boundary = TRUE} also edges to the
#' landscape boundary are included.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{TE >= 0}
#' \subsection{Behaviour}{Equals TE = 0 if all cells are edge cells. Increases, without limit,
#' as landscape becomes more fragmentated}
#'
#' @seealso
#' \code{\link{lsm_p_perim}}
#' \code{\link{lsm_l_te}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_te(landscape)
#'
#' @aliases lsm_l_te
#' @rdname lsm_l_te
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_te <- function(landscape, count_boundary) UseMethod("lsm_l_te")

#' @name lsm_l_te
#' @export
lsm_l_te.RasterLayer <- function(landscape,  count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterStack <- function(landscape,  count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterBrick <- function(landscape,  count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_te_calc <- function(landscape, count_boundary = FALSE){

    if(isTRUE(count_boundary)){
        landscape <- pad_raster(landscape = landscape,
                             pad_raster_value = max(raster::values(landscape)) +
                                 1,
                             pad_raster_cells = 1)
    }

    tb <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                      directions = 4)
    te <- sum(tb[lower.tri(tb)]) * raster::res(landscape)[[1]]

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "te",
        value = as.double(te)
    )

}
