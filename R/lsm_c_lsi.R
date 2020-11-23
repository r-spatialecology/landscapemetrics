#' LSI (class level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#'
#' @details
#' \deqn{LSI = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces.
#'
#' LSI is an 'Aggregation metric'. It is the ratio between the actual edge length of
#' class i and the hypothetical minimum edge length of class i. The minimum edge length equals
#' the edge length if class i would be maximally aggregated.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one squared patch is present or all
#' patches are maximally aggregated. Increases, without limit, as the length of the
#' actual edges increases, i.e. the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_l_lsi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lsi(landscape)
#'
#' @aliases lsm_c_lsi
#' @rdname lsm_c_lsi
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
lsm_c_lsi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_lsi_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_lsi_calc <- function(landscape) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "nlsi",
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

    # set diagonal to NA because no edge
    diag(class_perim) <- NA

    # calculate total edge
    class_perim <- apply(X = class_perim, MARGIN = 1, FUN = sum, na.rm = TRUE)[-1]

    # number of cells class
    class_area <- rcpp_get_composition_vector(landscape)[-1]

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

    # calculate LSI
    lsi <- class_perim / class_perim_min

    # warning if NA is introduced
    if (anyNA(lsi)) {
        warning("NA introduced by lsm_c_lsi", call. = FALSE)
    }

    return(tibble::tibble(level = "class",
                          class = as.integer(names(lsi)),
                          id = as.integer(NA),
                          metric = "lsi",
                          value = as.double(lsi)))
}
