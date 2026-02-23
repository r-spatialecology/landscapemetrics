#' PARA_MN (class level)
#'
#' @description Mean perimeter-area ratio (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA_{MN} = mean(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_MN is a 'Shape metric'. It summarises each class as the mean of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_MN > 0}
#' \subsection{Behaviour}{Approaches PARA_MN > 0 if PARA for each patch approaches PARA > 0,
#' i.e. the form approaches a rather small square. Increases, without limit, as PARA increases,
#' i.e. patches become more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link[base]{mean}}, \cr
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
#' lsm_c_para_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_para_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         para_mn <- lsm_c_para_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "para_mn",
                                          class = as.integer(names(para_mn)),
                                          value = unname(para_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_para_mn_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                                classes = NULL, class_patches = NULL, perimeter_patch = NULL, area_patches = NULL) {

    # reuse lsm_p_para_calc to get para values (handles lazy deps)
    para_patch <- lsm_p_para_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        perimeter_patch = perimeter_patch,
        area_patches = area_patches
    )

    # all cells are NA
    if (all(is.na(unname(para_patch)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    para_mn <- tapply(para_patch, names(para_patch), mean)

    # return named vector
    stats::setNames(as.double(para_mn), names(para_mn))
}
