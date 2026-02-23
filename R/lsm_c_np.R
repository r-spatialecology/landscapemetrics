#' NP (class level)
#'
#' @description Number of patches (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{NP = n_{i}}
#' where \eqn{n_{i}} is the number of patches.
#'
#' NP is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{NP >= 1}
#' \subsection{Behaviour}{Equals NP = 1 when only one patch is present and
#' increases, without limit, as the number of patches increases}
#'
#' @seealso
#' \code{\link{lsm_l_np}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_np(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_np <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         np <- lsm_c_np_calc(
                             landscape_mat = landscape_mat,
                             directions = directions
                         )

                         lsm_class_output(metric = "np",
                                          class = as.integer(names(np)),
                                          value = unname(np))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_np_calc <- function(landscape_mat, directions = NULL, classes = NULL, class_patches = NULL) {

    # all cells are NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches")
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
    }

    # get number of patches for each class
    np_class <- vapply(classes, function(patches_class) {
        landscape_labeled <- class_patches[[as.character(patches_class)]]
        as.integer(max(landscape_labeled, na.rm = TRUE))
    }, integer(1))

    # return named vector
    stats::setNames(as.double(np_class), as.character(classes))
}
