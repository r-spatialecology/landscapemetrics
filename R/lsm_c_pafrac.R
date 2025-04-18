#' PAFRAC  (class level)
#'
#' @description Perimeter-Area Fractal Dimension (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_pafrac(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Burrough, P. A. 1986. Principles of Geographical Information Systems for
#' Land Resources Assessment. Monographs on Soil and Resources Survey No. 12.
#' Clarendon Press, Oxford
#'
#' @export
lsm_c_pafrac <- function(landscape, directions = 8, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pafrac_calc <- function(landscape, directions, verbose, resolution, extras = NULL){

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_c_pafrac"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pafrac",
                              value = as.double(NA))))
    }

    # get patch area in sqm
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    area_patch$value <- area_patch$value * 10000

    # get patch perimeter
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution,
                                        extras = extras)

    # get number of patches
    np_class <- lsm_c_np_calc(landscape,
                              directions = directions,
                              extras = extras)

    pafrac_class <- lapply(X = seq_len(nrow(np_class)), FUN = function(class_current) {

        class_name <- as.integer(np_class[class_current, "class"])

        if (np_class$value[np_class$class == class_name] < 10) {

            pafrac <- NA

            if (verbose) {
                warning(paste0("Class ", class_name, ": PAFRAC = NA for class with < 10 patches"),
                        call. = FALSE)
            }

        } else {

            area_patch <- area_patch[area_patch$class == class_name, ]

            perimeter_patch <- perimeter_patch[perimeter_patch$class == class_name, ]

            regression_model_class <- stats::lm(log(area_patch$value) ~ log(perimeter_patch$value))

            pafrac <- 2 / regression_model_class$coefficients[[2]]
        }

        tibble::new_tibble(list(
            level = rep("class", length(pafrac)),
            class = rep(as.integer(class_name), length(pafrac)),
            id = rep(as.integer(NA), length(pafrac)),
            metric = rep("pafrac", length(pafrac)),
            value = as.double(pafrac)))
        })

    do.call("rbind", pafrac_class)
}
