#' Conditional entropy (landscape level)
#'
#' @description Complexity of a landscape pattern configuration.
#' It measures a only a geometric intricacy (configurational complexity)
#' of a landscape pattern.
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param neighbourhood The number of neighbourhood in which cells should be connected:
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
#' \code{\link{lsm_l_mutinf}},
#' \code{\link{lsm_l_joinent}},
#'
#' @return tibble
#'
#' @examples
#' lsm_l_condent(landscape)
#'
#' @aliases lsm_l_condent
#' @rdname lsm_l_condent
#'
#' @references
#' Nowosad J., TF Stepinski. 2018. Information-theoretical approach to measure
#' landscape complexity. DOI:
#'
#' @export
lsm_l_condent <- function(landscape,
                          neighbourhood = 4,
                          ordered = TRUE,
                          base = "log2") UseMethod("lsm_l_condent")

#' @name lsm_l_condent
#' @export
lsm_l_condent.RasterLayer <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_condent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_condent
#' @export
lsm_l_condent.RasterStack <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_condent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_condent
#' @export
lsm_l_condent.RasterBrick <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_condent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_condent
#' @export
lsm_l_condent.list <- function(landscape,
                               neighbourhood = 4,
                               ordered = TRUE,
                               base = "log2") {
    purrr::map_dfr(landscape,
                   lsm_l_condent_calc,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_condent_calc <- function(landscape, neighbourhood, ordered, base){

    landscape_matrix <- raster::as.matrix(landscape)
    cmh  <- rcpp_get_composition_vector(landscape_matrix)
    coh <- rcpp_get_coocurrence_vector(landscape_matrix,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)
    comp <- rcpp_get_entropy(cmh, base)
    cplx <- rcpp_get_entropy(coh, base)
    conf <- cplx - comp

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "condent",
        value = as.double(conf)
    )
}
