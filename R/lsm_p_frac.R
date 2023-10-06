#' FRAC (patch level)
#'
#' @description Fractal dimension index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC = \frac{2 * \ln * (0.25 * p_{ij})} {\ln a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters
#'
#' FRAC is a 'Shape metric'. The index is based on the patch perimeter and
#' the patch area and describes the patch complexity. Because it is standardized,
#' it is scale independent, meaning that increasing the patch size while not changing the
#' patch form will not change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= FRAC <= 2 }
#' \subsection{Behaviour}{Approaches FRAC = 1 for a squared patch shape form and FRAC = 2
#' for a irregular patch shape.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_frac(landscape)
#'
#' @aliases lsm_p_frac
#' @rdname lsm_p_frac
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Mandelbrot, B. B. 1977. Fractals: Form, Chance, and Dimension.
#' San Francisco. W. H. Freeman and Company.
#'
#' @export
lsm_p_frac <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_frac_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_frac_calc <- function(landscape, directions, resolution, extras = NULL){

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_p_frac"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "frac",
                              value = as.double(NA))))
    }

    # get patch perimeter
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution,
                                        extras = extras)

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # calculate frac
    frac_patch <- 2 * log(0.25 * perimeter_patch$value) / log(area_patch$value * 10000)

    # NaN for patches with only one cell (mathematical reasons) -> should be 1
    frac_patch[is.na(frac_patch)] <- 1

    tibble::new_tibble(list(
        level = rep("patch", nrow(perimeter_patch)),
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = rep("frac", nrow(perimeter_patch)),
        value = as.double(frac_patch)
    ))
}
