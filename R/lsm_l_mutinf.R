#' MUTINF (landscape level)
#'
#' @description Mutual information
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
#' It disambiguates landscape pattern types characterize
#' by the same value of an overall complexity (\code{\link{lsm_l_joinent}}).
#'
#' @seealso
#' \code{\link{lsm_l_ent}},
#' \code{\link{lsm_l_condent}},
#' \code{\link{lsm_l_joinent}},
#'
#' @return tibble
#'
#' @examples
#' lsm_l_mutinf(landscape)
#'
#' @aliases lsm_l_mutinf
#' @rdname lsm_l_mutinf
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. https://doi.org/10.1101/383281
#'
#' @export
lsm_l_mutinf <- function(landscape,
                         neighbourhood,
                         ordered,
                         base) UseMethod("lsm_l_mutinf")

#' @name lsm_l_mutinf
#' @export
lsm_l_mutinf.RasterLayer <- function(landscape,
                                     neighbourhood = 4,
                                     ordered = TRUE,
                                     base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mutinf_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mutinf
#' @export
lsm_l_mutinf.RasterStack <- function(landscape,
                                     neighbourhood = 4,
                                     ordered = TRUE,
                                     base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mutinf_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_mutinf
#' @export
lsm_l_mutinf.RasterBrick <- function(landscape,
                                     neighbourhood = 4,
                                     ordered = TRUE,
                                     base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mutinf_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_mutinf
#' @export
lsm_l_mutinf.list <- function(landscape,
                              neighbourhood = 4,
                              ordered = TRUE,
                              base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_mutinf_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_mutinf_calc <- function(landscape, neighbourhood, ordered, base){

    landscape_matrix <- raster::as.matrix(landscape)
    cmh  <- rcpp_get_composition_vector(landscape_matrix)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)
    comp <- rcpp_get_entropy(cmh, base)
    cplx <- rcpp_get_entropy(coh, base)
    conf <- cplx - comp
    aggr <- comp - conf

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "mutinf",
        value = as.double(aggr)
    )
}
