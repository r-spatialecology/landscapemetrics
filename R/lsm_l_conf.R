#' Configuration (landscape level)
#'
#' @description Configuration
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Details.
#'
#' @seealso
#' \code{\link{lsm_l_comp}},
#' \code{\link{lsm_l_aggr}},
#' \code{\link{lsm_l_cplx}},
#'
#' @return tibble
#'
#' @examples
#' lsm_l_conf(landscape)
#'
#' @aliases lsm_l_conf
#' @rdname lsm_l_conf
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. DOI:
#'
#' @export
lsm_l_conf <- function(landscape, directions, ordered, base) UseMethod("lsm_l_conf")

#' @name lsm_l_conf
#' @export
lsm_l_conf.RasterLayer <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_conf_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_conf
#' @export
lsm_l_conf.RasterStack <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_conf_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_conf
#' @export
lsm_l_conf.RasterBrick <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_conf_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_conf
#' @export
lsm_l_conf.list <- function(landscape, directions = 4, ordered = TRUE, base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_conf_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_conf_calc <- function(landscape, directions, ordered, base){
    landscape_matrix <- raster::as.matrix(landscape)
    cmh  <- rcpp_get_composition_vector(landscape_matrix)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix,
                                       directions = directions,
                                       ordered = ordered)
    comp <- rcpp_get_entropy(cmh, base)
    cplx <- rcpp_get_entropy(coh, base)
    conf <- cplx - comp

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "configuration",
        value = as.double(conf)
    )
}
