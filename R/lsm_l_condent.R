#' Conditional entropy (landscape level)
#'
#' @description Conditional entropy \\[H(y|x)\\]
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
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework
#' for quantification and classification of landscape patterns. https://doi.org/10.1007/s10980-019-00830-x
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

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
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

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
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

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
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

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
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

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_condent_calc <- function(landscape, neighbourhood, ordered, base){

    # convert to raster to matrix
    if (class(landscape) != "matrix") {
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "condent",
                              value = as.double(NA)))
    }

    com <- rcpp_get_coocurrence_matrix(landscape,
                                       directions = as.matrix(neighbourhood))
    com_c <- colSums(com)

    coh <- rcpp_get_coocurrence_vector(landscape,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)

    comp <- rcpp_get_entropy(com_c, base)
    cplx <- rcpp_get_entropy(coh, base)

    conf <- cplx - comp

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "condent",
                          value = as.double(conf)))
}
