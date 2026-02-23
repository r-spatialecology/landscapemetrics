#' PARA (patch level)
#'
#' @description Perimeter-Area ratio (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA = \frac{p_{ij}} {a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters.
#'
#' PARA is a 'Shape metric'. It describes the patch complexity in a
#' straightforward way. However, because it is not standarised to a certain
#' shape (e.g. a square), it is not scale independent, meaning that increasing
#' the patch size while not changing the patch form will change the ratio.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA > 0}
#' \subsection{Behaviour}{Increases, without limit, as the shape complexity
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_para(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_para <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         para <- lsm_p_para_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_patch_output(metric = "para",
                                          class = as.integer(names(para)),
                                          value = unname(para),
                                          id = seq_along(para))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_para_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                             classes = NULL, class_patches = NULL, perimeter_patch = NULL, area_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(perimeter_patch) || is.null(area_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "perimeter_patch", "area_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        perimeter_patch <- deps$perimeter_patch
        area_patches <- deps$area_patches
    }

    # calculate ratio between area and perim
    para_patch <- perimeter_patch / (area_patches * 10000)

    # return named vector (preserve names)
    structure(as.double(para_patch), names = names(para_patch))
}
