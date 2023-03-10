#' JOINENT (landscape level)
#'
#' @description Joint entropy \\[H(x, y)\\]
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' \code{\link{lsm_l_mutinf}},
#' \code{\link{lsm_l_relmutinf}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_joinent(terra::unwrap(landscape))
#'
#' @aliases lsm_l_joinent
#' @rdname lsm_l_joinent
#'
#' @references
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework
#' for quantification and classification of landscape patterns. https://doi.org/10.1007/s10980-019-00830-x
#'
#' @export
lsm_l_joinent <- function(landscape,
                               neighbourhood = 4,
                               ordered = TRUE,
                               base = "log2") {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_joinent_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_joinent_calc <- function(landscape, neighbourhood, ordered, base){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "joinent",
                              value = as.double(NA)))
    }

    coh <- rcpp_get_coocurrence_vector(landscape,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)

    cplx <- rcpp_get_entropy(coh, base)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "joinent",
                          value = as.double(cplx)))
}
