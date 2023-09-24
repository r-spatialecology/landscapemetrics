#' LSI (landscape level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#'
#' @details
#' \deqn{LSI = \frac{E} {\min E}}
#' where \eqn{E} is the total edge length in cell surfaces and \eqn{\min E}
#' is the minimum total edge length in cell surfaces.
#'
#' LSI is an 'Aggregation metric'. It is the ratio between the actual landscape edge length
#' and the hypothetical minimum edge length. The minimum edge length equals
#' the edge length if only one patch would be present.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one squared patch is present.
#' Increases, without limit, as the length of the actual edges increases, i.e.
#' the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_c_lsi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_lsi(landscape)
#'
#' @aliases lsm_l_lsi
#' @rdname lsm_l_lsi
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
lsm_l_lsi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_lsi_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_lsi_calc <- function(landscape, extras = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "lsi",
                              value = as.double(NA)))
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

    # calculate total edge
    total_perim <- sum(class_perim[lower.tri(class_perim)])

    # calculate total area
    total_area <- sum(class_area)

    # calculate N and M
    total_n <- trunc(sqrt(total_area))

    total_m <- total_area - total_n ^ 2

    # calculate min_edge
    total_perim_min <- ifelse(test = total_m == 0,
                              yes = total_n * 4,
                              no = ifelse(test = total_n ^ 2 < total_area & total_area <= total_n * (1 + total_n),
                                          yes = 4 * total_n + 2,
                                          no = ifelse(test = total_area > total_n * (1 + total_n),
                                                      yes = 4 * total_n + 4,
                                                      no = NA)))

    lsi <- total_perim / total_perim_min

    # test if any NAs introduced
    if (!is.finite(lsi)) {

        warning("NAs introduced by lsm_l_lsi.", call. = FALSE)

        lsi <- NA
    }

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "lsi",
                          value = as.double(lsi)))
}
