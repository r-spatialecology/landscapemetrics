#' MUTINF (landscape level)
#'
#' @description Mutual information \\[I(y,x)\\]
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' It disambiguates landscape pattern types characterize by the same value of an
#' overall complexity (\code{\link{lsm_l_joinent}}).
#'
#' @seealso
#' \code{\link{lsm_l_ent}},
#' \code{\link{lsm_l_condent}},
#' \code{\link{lsm_l_joinent}},
#' \code{\link{lsm_l_relmutinf}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_mutinf(landscape)
#'
#' @references
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework
#' for quantification and classification of landscape patterns. https://doi.org/10.1007/s10980-019-00830-x
#'
#' @export
lsm_l_mutinf <- function(landscape,
                              neighbourhood = 4,
                              ordered = TRUE,
                              base = "log2") {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_mutinf_calc,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_mutinf_calc <- function(landscape, neighbourhood, ordered, base, extras = NULL){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "mutinf",
                              value = as.double(NA))))
    }

    if (!is.null(extras)){
        comp <- extras$comp
        cplx <- extras$cplx
    } else {
        com <- rcpp_get_coocurrence_matrix(landscape, directions = as.matrix(neighbourhood))
        comp <- rcpp_get_entropy(colSums(com), base)
        cplx <- get_complexity(landscape, neighbourhood, ordered, base)
    }

    conf <- cplx - comp
    aggr <- comp - conf

    return(tibble::new_tibble(list(level = rep("landscape", length(aggr)),
                          class = rep(as.integer(NA), length(aggr)),
                          id = rep(as.integer(NA), length(aggr)),
                          metric = rep("mutinf", length(aggr)),
                          value = as.double(aggr))))
}
