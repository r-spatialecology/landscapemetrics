#' ENT (landscape level)
#'
#' @description Marginal entropy \\[H(x)\\]
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param base The unit in which entropy is measured.
#' The default is "log2", which compute entropy in "bits".
#' "log" and "log10" can be also used.
#'
#' @details
#' It measures a diversity (thematic complexity) of landscape classes.
#'
#' @seealso
#' \code{\link{lsm_l_condent}},
#' \code{\link{lsm_l_mutinf}},
#' \code{\link{lsm_l_joinent}},
#' \code{\link{lsm_l_relmutinf}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_ent(landscape)
#'
#' @aliases lsm_l_ent
#' @rdname lsm_l_ent
#'
#' @references
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework
#' for quantification and classification of landscape patterns. https://doi.org/10.1007/s10980-019-00830-x
#'
#' @export
lsm_l_ent <- function(landscape,
                           neighbourhood = 4,
                           base = "log2") {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_ent_calc,
                     neighbourhood = neighbourhood,
                     base = base)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ent_calc <- function(landscape, neighbourhood, base, extras = NULL){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ent",
                              value = as.double(NA)))
    }

    if (!is.null(extras)){
        comp <- extras$comp
    } else {
        com <- rcpp_get_coocurrence_matrix(landscape,
                                        directions = as.matrix(neighbourhood))
        comp <- rcpp_get_entropy(colSums(com), base)
    }

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "ent",
                          value = as.double(comp)))
}
