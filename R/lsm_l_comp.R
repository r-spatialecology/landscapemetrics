#' Composition (landscape level)
#'
#' @description Composition
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Details.
#'
#' @seealso
#' \code{\link{lsm_l_conf}},
#' \code{\link{lsm_l_aggr}},
#' \code{\link{lsm_l_cplx}},
#'
#' @return tibble
#'
#' @examples
#' lsm_l_comp(landscape)
#'
#' @aliases lsm_l_comp
#' @rdname lsm_l_comp
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. DOI:
#'
#' @export
lsm_l_comp <- function(landscape, base) UseMethod("lsm_l_comp")

#' @name lsm_l_comp
#' @export
lsm_l_comp.RasterLayer <- function(landscape, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_comp_calc,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_comp
#' @export
lsm_l_comp.RasterStack <- function(landscape, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_comp_calc,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_comp
#' @export
lsm_l_comp.RasterBrick <- function(landscape, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_comp_calc,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_comp
#' @export
lsm_l_comp.list <- function(landscape, base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_comp_calc,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_comp_calc <- function(landscape, base){
    landscape_matrix <- raster::as.matrix(landscape)
    cmh  <- rcpp_get_composition_vector(landscape_matrix)
    comp <- rcpp_get_entropy(cmh, base)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "composition",
        value = as.double(comp)
    )
}
