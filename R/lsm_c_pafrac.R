#' PAFRAC  (class level)
#'
#' @description Perimeter-Area Fractal Dimension (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{PAFRAC = \frac{2}{\beta}}
#' where \eqn{\beta} is the slope of the regression of the area against the perimeter
#' (logarithm) \eqn{n_{i}\sum \limits_{j = 1}^{n} \ln a_{ij} = a + \beta n_{i}\sum \limits_{j = 1}^{n} \ln p_{ij}}
#'
#' PAFRAC is a 'Shape metric'. It describes the patch complexity of class i while being
#' scale independent. This means that increasing the patch size while not changing the
#' patch form will not change the metric. However, it is only meaningful if the relationship
#' between the area and perimeter is linear on a logarithmic scale. Furthermore, if there
#' are less than 10 patches in class i, the metric returns NA because of the small-sample
#' issue.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2}
#' \subsection{Behaviour}{Approaches PAFRAC = 1 for patches with simple shapes and
#' approaches PAFRAC = 2 for irregular shapes}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_l_pafrac}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pafrac(landscape)
#'
#' @aliases lsm_c_pafrac
#' @rdname lsm_c_pafrac
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Burrough, P. A. 1986. Principles of Geographical Information Systems for
#' Land Resources Assessment. Monographs on Soil and Resources Survey No. 12.
#' Clarendon Press, Oxford
#'
#' @export
lsm_c_pafrac <- function(landscape, directions, verbose) UseMethod("lsm_c_pafrac")

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterLayer <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterStack <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.RasterBrick <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.stars <- function(landscape, directions = 8, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_pafrac
#' @export
lsm_c_pafrac.list <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_pafrac_calc <- function(landscape, directions, verbose){

    area_patch <- dplyr::mutate(lsm_p_area_calc(landscape, directions = directions),
                                value = value * 10000)

    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions)

    np_class <- lsm_c_np_calc(landscape,
                              directions = directions)

    pafrac_class <- lapply(X = seq_len(nrow(np_class)), FUN = function(class_current) {

        class_name <- as.integer(np_class[class_current, "class"])

        if(np_class$value[np_class$class == class_name] < 10){

            pafrac <- NA

            if(isTRUE(verbose)) {
                warning(paste0("Class ", class_name, ": PAFRAC = NA for class with < 10 patches"),
                        call. = FALSE)
            }
        } else {

            area_class <- dplyr::filter(area_patch, class == class_name)

            perimeter_class <- dplyr::filter(perimeter_patch, class == class_name)

            regression_model_class <- stats::lm(log(area_class$value) ~ log(perimeter_class$value))

            pafrac <- 2 / regression_model_class$coefficients[[2]]
        }

        tibble::tibble(
            level = "class",
            class = as.integer(class_name),
            id = as.integer(NA),
            metric = "pafrac",
            value = as.double(pafrac))
    })

    pafrac_class <- dplyr::bind_rows(pafrac_class)
}
