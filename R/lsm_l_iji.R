#'Interspersion and Juxtaposition index (landscape level)
#
#' @description Interspersion and Juxtaposition index (Aggregation metric)
#
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param verbose Print warning message if not sufficient patches are present
#
#' @details
#' \deqn{IJI = \frac{- \sum \limits_{i = 1}^{m} \sum \limits_{k = i + 1}^{m} \Bigg[ \Bigg( \frac{e_{ik}}{E} \Bigg) ln \Bigg( \frac{e_{ik}}{E} \Bigg) \Bigg]}{ln(0.5[m(m - 1)])}  * 100}
#'
#' where \eqn{e_{ik}} are the unique adjacencies of all classes (lower/upper triangle of
#' the adjacency table - without the diagonal), \eqn{E} is the total length of edges in the landscape
#' and \eqn{m} is the number of classes.
#'
#' IJI is an 'Aggregation metric'. It is a so called "salt and pepper" metric and
#' describes the intermixing of classes (i.e. without considering like adjacencies - the
#' diagonal of the adjacency table). The number of classes to calculate IJI must be >= than 3.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 < IJI <= 100}
#' \subsection{Behaviour}{Approaches 0 if a class is only adjacent to a single other class
#' and equals 100 when a class is equally adjacent to all other classes.}
#
#' @seealso
#' \code{\link{lsm_c_iji}}
#'
#' @return tibble
#
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_iji(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#'McGarigal, K., & Marks, B. J. 1995. FRAGSTATS: spatial pattern analysis
#'program for quantifying landscape structure. Gen. Tech. Rep. PNW-GTR-351.
#'Portland, OR: US Department of Agriculture, Forest Service, Pacific Northwest
#'Research Station. 122 p, 351.
#
#' @export
lsm_l_iji <- function(landscape, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_iji_calc,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_iji_calc <- function(landscape, verbose, extras = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "iji",
                              value = as.double(NA))))
    }

    if (!is.null(extras)){
        adjacencies <- extras$neighbor_matrix
    } else {
        adjacencies <- rcpp_get_coocurrence_matrix(landscape, as.matrix(4))
    }

    if (ncol(adjacencies) < 3) {

        if (verbose) {
            warning("Number of classes must be >= 3, IJI = NA.", call. = FALSE)
        }

        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "iji",
                              value = as.double(NA))))
    } else {

        diag(adjacencies) <- 0

        e_total <- sum(adjacencies[lower.tri(adjacencies)])

        edge_ratio <- (adjacencies / e_total) * log(adjacencies / e_total)

        edge_ratio <- edge_ratio[lower.tri(edge_ratio)]

        landscape_sum <- -sum(edge_ratio, na.rm = TRUE)

        iji <- (landscape_sum / log(0.5  * (ncol(adjacencies) * (ncol(adjacencies)  - 1)))) * 100

        return(tibble::new_tibble(list(level = rep("landscape", length(iji)),
                 class = rep(as.integer(NA), length(iji)),
                 id = rep(as.integer(NA), length(iji)),
                 metric = rep("iji", length(iji)),
                 value = as.double(iji))))
    }
}
