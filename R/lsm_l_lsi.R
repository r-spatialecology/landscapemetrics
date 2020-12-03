#' LSI (landscape level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' lsm_l_lsi(landscape)
#'
#' @aliases lsm_l_lsi
#' @rdname lsm_l_lsi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
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

lsm_l_lsi_calc <- function(landscape) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
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
    landscape <- pad_raster(landscape,
                            pad_raster_value = NA,
                            pad_raster_cells = 1,
                            return_raster = FALSE)[[1]]

    # which cells are NA (i.e. background)
    target_na <- which(is.na(landscape))

    # set all NA to -999 to get adjacencies between patches and all background
    landscape[target_na] <- -999

    # get class edge in terms of cell surfaces
    class_perim <- rcpp_get_coocurrence_matrix(landscape,
                                               as.matrix(4))

    # calculate total edge
    total_perim <- sum(class_perim[lower.tri(class_perim)])

    # calculate total area
    total_area <- sum(rcpp_get_composition_vector(landscape)[-1])

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
