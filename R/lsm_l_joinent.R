#' JOINENT (landscape level)
#'
#' @description Joint entropy \\[H(x, y)\\]
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_joinent(landscape)
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
                     FUN = function(x) {
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         value <- lsm_l_joinent_calc(
                             landscape_mat = landscape_mat,
                             neighbourhood = neighbourhood,
                             ordered = ordered,
                             base = base
                         )

                         lsm_landscape_output(metric = "joinent", value = value)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_joinent_calc <- function(landscape_mat, neighbourhood = 4, ordered = TRUE, base = "log2", cplx = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(cplx)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            required = c("cplx"),
            neighbourhood = neighbourhood,
            ordered = ordered,
            base = base
        )
        cplx <- deps$cplx
    }

    return(as.double(cplx))
}
