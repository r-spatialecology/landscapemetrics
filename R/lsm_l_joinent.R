#' JOINENT (landscape level)
#'
#' @description Joint entropy
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered.
#' Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured.
#' The default is "log2", which compute entropy in "bits".
#' "log" and "log10" can be also used.
#'
#' @details
#' Complexity of a landscape pattern. An overall spatio-thematic complexity metric.
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
#' landscape complexity. https://doi.org/10.1101/383281
#'
#' @export
lsm_l_joinent <- function(landscape,
                          neighbourhood,
                          ordered,
                          base) UseMethod("lsm_l_joinent")

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterLayer <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterStack <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.RasterBrick <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_joinent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_joinent
#' @export
lsm_l_joinent.list <- function(landscape,
                               neighbourhood = 4,
                               ordered = TRUE,
                               base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_joinent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_joinent_calc <- function(landscape, neighbourhood, ordered, base){

    landscape_matrix <- raster::as.matrix(landscape)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix,
                                       directions = as.matrix(neighbourhood),
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
