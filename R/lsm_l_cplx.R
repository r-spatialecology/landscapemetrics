#' Complexity (landscape level)
#'
#' @description Complexity
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Details.
#'
#' @seealso
#' \code{\link{lsm_l_comp}},
#' \code{\link{lsm_l_conf}},
#' \code{\link{lsm_l_aggr}}
#'
#' @return tibble
#'
#' @examples
#' library(raster)
#' test1 = raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
#' test2 = as.matrix(test1)
#' test3 = raster(nrows = 3, ncols = 3, vals = c(3, rep(1, 7), NA))
#'
#' lsm_l_cplx(test1)
#' lsm_l_cplx(test1, directions = 4)
#' lsm_l_cplx(test2)
#' lsm_l_cplx(test3)
#'
#' @aliases lsm_l_cplx
#' @rdname lsm_l_cplx
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. DOI:
#'
#' @export
lsm_l_cplx <- function(landscape, directions = 4, ordered = TRUE, base = "log2") UseMethod("lsm_l_cplx")

#' @name lsm_l_cplx
#' @export
lsm_l_cplx.RasterLayer <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cplx_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cplx
#' @export
lsm_l_cplx.RasterStack <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cplx_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cplx
#' @export
lsm_l_cplx.RasterBrick <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cplx_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cplx
#' @export
lsm_l_cplx.list <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(landscape, lsm_l_cplx_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))


}

lsm_l_cplx_calc <- function(landscape, directions = 4, ordered = TRUE, base = "log2"){
    landscape_matrix <- raster::as.matrix(landscape)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix, directions, ordered)
    rcpp_get_entropy(coh, base)
}
