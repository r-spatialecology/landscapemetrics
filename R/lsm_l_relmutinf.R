#' RELMUTINF (landscape level)
#'
#' @description Relative mutual information
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
#' Due to the spatial autocorrelation, the value of mutual information tends to grow
#' with a diversity of the landscape (marginal entropy). To adjust this tendency,
#' it is possible to calculate relative mutual information by dividing the mutual
#' information by the marginal entropy. Relative mutual information always has a
#' range between 0 and 1 and can be used to compare spatial data with different
#' number and distribution of categories. When the value of mutual information equals
#' to 0, then relative mutual information is 1.
#'
#' @seealso
#' \code{\link{lsm_l_ent}},
#' \code{\link{lsm_l_condent}},
#' \code{\link{lsm_l_joinent}},
#' \code{\link{lsm_l_mutinf}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_relmutinf(landscape)
#'
#' @references
#' Nowosad J., TF Stepinski. 2019. Information theory as a consistent framework
#' for quantification and classification of landscape patterns. https://doi.org/10.1007/s10980-019-00830-x
#'
#' @export
lsm_l_relmutinf <- function(landscape,
                              neighbourhood = 4,
                              ordered = TRUE,
                              base = "log2") {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         value <- lsm_l_relmutinf_calc(
                             landscape_mat = landscape_mat,
                             neighbourhood = neighbourhood,
                             ordered = ordered,
                             base = base
                         )

                         lsm_landscape_output(metric = "relmutinf", value = value)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_relmutinf_calc <- function(landscape_mat, neighbourhood = 4, ordered = TRUE, base = "log2", comp = NULL, cplx = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(comp) || is.null(cplx)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            required = c("comp", "cplx"),
            neighbourhood = neighbourhood,
            ordered = ordered,
            base = base
        )
        comp <- deps$comp
        cplx <- deps$cplx
    }

    conf <- cplx - comp

    mutinf <- comp - conf

    relmutinf <- ifelse(mutinf == 0, 1, mutinf / comp)

    return(as.double(relmutinf))
}
