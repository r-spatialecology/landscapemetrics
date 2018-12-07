#' FRAC_SD (class level)
#'
#' @description Standard deviation fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{FRAC_{SD} = sd(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_SD is a 'Shape metric'. The metric summarises each class
#' as the standard deviation of the fractal dimension index of all patches
#' belonging to class i. The fractal dimension index is based on the patch perimeter and
#' the patch area and describes the patch complexity.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_SD>= 0 }
#' \subsection{Behaviour}{Equals FRAC_SD = 0 if the fractal dimension index is identical
#' for all patches. Increases, without limit, as the variation of the fractal dimension
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_sd(landscape)
#'
#' @aliases lsm_c_frac_sd
#' @rdname lsm_c_frac_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Mandelbrot, B. B. 1977. Fractals: Form, Chance, and Dimension.
#' San Francisco. W. H. Freeman and Company.
#'
#' @export
lsm_c_frac_sd <- function(landscape, directions) UseMethod("lsm_c_frac_sd")

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_frac_sd_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_frac_sd_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_frac_sd_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_frac_sd_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_frac_sd_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_frac_sd_calc <- function(landscape, directions, resolution = NULL){

    frac <- lsm_p_frac_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    frac_sd <- dplyr::summarise(dplyr::group_by(frac, class),
                                value = stats::sd(value))

    tibble::tibble(
        level = "class",
        class = as.integer(frac_sd$class),
        id = as.integer(NA),
        metric = "frac_sd",
        value = as.double(frac_sd$value)
    )
}
