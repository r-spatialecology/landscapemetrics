#' nLSI (class level)
#'
#' @description Normalized landscape shape index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#'
#' @details
#' \deqn{nLSI = \frac{e_{i} - \min e_{i}} {\max e_{i} - \min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' \eqn{\max e_{i}} are the minimum and maximum total edge length in cell surfaces, respectively.
#'
#' nLSI is an 'Aggregation metric'. It is closely related to the \code{\link{lsm_c_lsi}}
#' and describes the ratio of the actual edge length of class i in relation to the
#' hypothetical range of possible edge lengths of class i (min/max).
#'
#' Currently, nLSI ignores all background cells when calculating the minimum and maximum
#' total edge length. Also, a correct calculation of the minimum and maximum
#' total edge length is currently only possible for rectangular landscapes.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{0 <= nlsi <= 1}
#' \subsection{Behaviour}{Equals nLSI = 0 when only one squared patch is present. nLSI
#' increases the more disaggregated patches are and equals nLSI = 1 for a maximal disaggregated
#' (i.e. a "checkerboard pattern").}
#'
#' @seealso
#' \code{\link{lsm_c_lsi}}
#' \code{\link{lsm_l_lsi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_nlsi(landscape)
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
lsm_c_nlsi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_nlsi_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_nlsi_calc <- function(landscape, extras = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "nlsi",
                              value = as.double(NA))))
    }

    # cells at the boundary of the landscape need neighbours to calculate perim
    landscape_pad <- pad_raster_internal(landscape, pad_raster_value = NA,
                                     pad_raster_cells = 1, global = FALSE)

    # which cells are NA (i.e. background)
    target_na <- which(is.na(landscape_pad))

    # set all NA to -999 to get adjacencies between patches and all background
    landscape_pad[target_na] <- -999

    # get class edge in terms of cell surfaces
    class_perim <- rcpp_get_coocurrence_matrix(landscape_pad, as.matrix(4))
    class_area <- rcpp_get_composition_vector(landscape_pad)[-1]

    # set diagonal to NA because no edge
    diag(class_perim) <- NA

    # calculate total edge
    class_perim <- apply(X = class_perim, MARGIN = 1, FUN = sum, na.rm = TRUE)[-1]

    # n is the side of the largest integer square
    class_n <- trunc(sqrt(class_area))

    # calculate m
    class_m <- class_area - class_n ^ 2

    # calculate min_edge
    class_perim_min <- ifelse(test = class_m == 0,
                              yes = class_n * 4,
                              no = ifelse(test = class_n ^ 2 < class_area & class_area <= class_n * (1 + class_n),
                                          yes = 4 * class_n + 2,
                                          no = ifelse(test = class_area > class_n * (1 + class_n),
                                                      yes = 4 * class_n + 4,
                                                      no = NA)))

    # calculate numerator
    numerator <- class_perim - class_perim_min

    # calculate total area in terms of cells
    total_area <- sum(class_area)

    # get all cells on the boundary; need to remove padded cells
    cells_boundary <- ((nrow(landscape_pad) - 2) * 2) + ((ncol(landscape_pad) - 2) * 2)

    # calculate proportion of classes
    class_pi <- prop.table(class_area)

    class_perim_max <- ifelse(test = class_pi <= 0.5,
                              yes = 4 * class_area,
                              no = ifelse(test = total_area %% 2 == 0 & class_pi > 0.5 & class_pi <= (0.5 * total_area + 0.5 * cells_boundary) / total_area,
                                          yes = 3 * total_area - 2 * class_area,
                                          no = ifelse(test = total_area %% 2 != 0 & class_pi > 0.5 & class_pi <= (0.5 * total_area + 0.5 * cells_boundary) / total_area,
                                                      yes = 3 * total_area - 2 * class_area + 3,
                                                      no = ifelse(test = class_pi >= (0.5 * total_area + 0.5 * cells_boundary) / total_area,
                                                                  yes = class_perim + 4 * (total_area - class_area),
                                                                  no = NA))))

    # test if any NAs introduced
    if (anyNA(class_perim_max)) {
        stop("NAs introduced by lsm_c_nlsi", call. = FALSE)
    }

    # calculate denominator
    denominator <- class_perim_max - class_perim_min

    # calculate total nlsi
    nlsi <- numerator / denominator

    # test if any NAs introduced
    if (!all(is.finite(nlsi))) {
        warning("NAs introduced by lsm_c_nlsi.", call. = FALSE)
        nlsi[!is.finite(nlsi)] <- NA
    }

    return(tibble::new_tibble(list(level = rep("class", length(nlsi)),
                              class = as.integer(names(nlsi)),
                              id = rep(as.integer(NA), length(nlsi)),
                              metric = rep("nlsi", length(nlsi)),
                              value = as.double(nlsi))))
}
