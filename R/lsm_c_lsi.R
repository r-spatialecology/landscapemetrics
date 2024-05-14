#' LSI (class level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' actual edges increases, i.e. the patches become less compact.} Because the metric
#' is based on distances or areas please make sure your data is valid using
#' \code{\link{check_landscape}}.
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_l_lsi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_lsi(landscape)
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
lsm_c_lsi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_lsi_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_lsi_calc <- function(landscape, extras = NULL) {

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
    class_perim_min <- ifelse(test = class_m == 0, yes = class_n * 4,
                              no = ifelse(test = class_n ^ 2 < class_area & class_area <= class_n * (1 + class_n), yes = 4 * class_n + 2,
                                          no = ifelse(test = class_area > class_n * (1 + class_n), yes = 4 * class_n + 4,
                                                      no = NA)))

    # test if any NAs introduced
    if (anyNA(class_perim_min)) {
        stop("NAs introduced by lsm_c_lsi.", call. = FALSE)
    }

    # calculate LSI
    lsi <- class_perim / class_perim_min

    return(tibble::new_tibble(list(level = rep("class", length(lsi)),
                              class = as.integer(names(lsi)),
                              id = rep(as.integer(NA), length(lsi)),
                              metric = rep("lsi", length(lsi)),
                              value = as.double(lsi))))
}
