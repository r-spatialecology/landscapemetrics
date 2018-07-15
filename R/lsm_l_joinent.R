#' Joint entropy (landscape level)
#'
#' @description Complexity of a landscape pattern. An overall spatio-thematic complexity metric.
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be connected:
#' 4 (rook's case) or 8 (queen's case).
#' The default is 4.
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured.
#' The default is "log2", which compute entropy in "bits".
#' "log" and "log10" can be also used.
#'
#' @seealso
#' \code{\link{lsm_l_ent}},
#' \code{\link{lsm_l_condent}},
#' \code{\link{lsm_l_mutinf}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_joinent(landscape)
#'
#' @aliases lsm_l_joinent
#' @rdname lsm_l_joinent
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. DOI:
#'
#' @export
lsm_l_joinent <- function(landscape,
                          directions = 4,
                          ordered = TRUE,
                          base = "log2") UseMethod("lsm_l_joinent")

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterLayer <- function(landscape,
                                      directions = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterStack <- function(landscape,
                                      directions = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterBrick <- function(landscape,
                                      directions = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.list <- function(landscape,
                               directions = 4,
                               ordered = TRUE,
                               base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_joinent_calc,
                   directions = directions,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_joinent_calc <- function(landscape, directions, ordered, base){

    landscape_matrix <- raster::as.matrix(landscape)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix,
                                       directions = as.matrix(directions),
                                       ordered = ordered)
    cplx <- rcpp_get_entropy(coh, base)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "joinent",
        value = as.double(cplx)
    )
}
