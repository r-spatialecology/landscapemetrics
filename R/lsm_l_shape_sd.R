#' SHAPE_SD (landscape level)
#'
#' @description Standard deviation shape index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE_{SD} = sd(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_SD is a 'Shape metric'. The landscape summarised as the standard deviation
#' of all patches in the landscape. SHAPE describes the ratio between the actual perimeter of
#' the patch and the square root of patch area.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE_SD >= 0}
#' \subsection{Behaviour}{Equals SHAPE_SD = 0 if all patches have an identical shape index.
#' Increases, without limit, as the variation of the shape index increases.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}},
#' \code{\link[stats]{sd}} \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_sd}},
#' \code{\link{lsm_c_shape_cv}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_shape_sd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_l_shape_sd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_shape_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_shape_sd_calc <- function(landscape, directions, resolution, extras = NULL){

    # shape index for each patch
    shape <- lsm_p_shape_calc(landscape,
                              directions = directions,
                              resolution = resolution,
                              extras = extras)

    # all values NA
    if (all(is.na(shape$value))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "shape_sd",
                              value = as.double(NA))))
    }

    # calculate sd
    shape_sd <- stats::sd(shape$value, na.rm = TRUE)

    return(tibble::new_tibble(list(level = rep("landscape", length(shape_sd)),
                          class = rep(as.integer(NA), length(shape_sd)),
                          id = rep(as.integer(NA), length(shape_sd)),
                          metric = rep("shape_sd", length(shape_sd)),
                          value = as.double(shape_sd))))
}
