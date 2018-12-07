#' Conditional entropy (landscape level)
#'
#' @description Conditional entropy
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
#' Complexity of a landscape pattern configuration.
#' It measures a only a geometric intricacy (configurational complexity)
#' of a landscape pattern.
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
#' landscape complexity. https://doi.org/10.1101/383281
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

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_condent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_condent
#' @export
lsm_l_condent.RasterStack <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_condent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_condent
#' @export
lsm_l_condent.RasterBrick <- function(landscape,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2") {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_condent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_condent
#' @export
lsm_l_condent.stars <- function(landscape,
                                neighbourhood = 4,
                                ordered = TRUE,
                                base = "log2") {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_condent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_condent
#' @export
lsm_l_condent.list <- function(landscape,
                               neighbourhood = 4,
                               ordered = TRUE,
                               base = "log2") {

    result <- lapply(X = landscape,
                     FUN = lsm_l_condent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_condent_calc <- function(landscape, neighbourhood, ordered, base){

    # convert to raster to matrix
    if(class(landscape) != "matrix") {
        landscape <- raster::as.matrix(landscape)
    }

    cmh  <- rcpp_get_composition_vector(landscape)

    coh <- rcpp_get_coocurrence_vector(landscape,
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
