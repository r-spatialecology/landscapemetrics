#' Interspersion and Juxtaposition index (class level)
#
#' @description Interspersion and Juxtaposition index (Aggregation metric)
#
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param verbose Print warning message if not sufficient patches are present
#
#' @details
#' \deqn{IJI = \frac{- \sum \limits_{k = 1}^{m} \Bigg[ \Bigg( \frac{e_{ik}}{\sum \limits_{k = 1}^{m} e_{ik}} \Bigg) ln \Bigg( \frac{e_{ik}}{\sum \limits_{k = 1}^{m} e_{ik}} \Bigg) \Bigg]}{ln(m - 1)}  * 100}
#'
#' where \eqn{e_{ik}} are the unique adjacencies of all classes (lower/upper triangle of
#' the adjacency table - without the diagonal) and \eqn{m} is the number of classes.
#'
#' IJI is an 'Aggregation metric'. It is a so called "salt and pepper" metric and
#' describes the intermixing of classes (i.e. without considering like adjacencies - the
#' diagonal of the adjacency table). The number of classes to calculate IJI must be >= than 3.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 < IJI <= 100}
#' \subsection{Behaviour}{Approaches 0 if a class is only adjacent to a single other class
#' and equals 100 when a class is equally adjacent to all other classes.}
#'
#' @seealso
#' \code{\link{lsm_l_iji}}
#'
#' @return tibble
#
#' @examples
#'lsm_c_iji(landscape)
#
#' @aliases lsm_c_iji
#' @rdname lsm_c_iji
#
#' @references
#'McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#'Program for Categorical and Continuous Maps. Computer software program produced by
#'the authors at the University of Massachusetts, Amherst. Available at the following
#'web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#'McGarigal, K., & Marks, B. J. 1995. FRAGSTATS: spatial pattern analysis
#'program for quantifying landscape structure. Gen. Tech. Rep. PNW-GTR-351.
#'Portland, OR: US Department of Agriculture, Forest Service, Pacific Northwest
#'Research Station. 122 p, 351.
#
#' @export
lsm_c_iji <- function(landscape, verbose = TRUE) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_iji_calc,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_iji_calc <- function(landscape, verbose) {

    # conver to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "iji",
                              value = as.double(NA)))
    }

    adjacencies <- rcpp_get_coocurrence_matrix(landscape,
                                               as.matrix(4))

    classes <- rownames(adjacencies)

    if (ncol(adjacencies) < 3) {

        if (verbose) {
            warning("Number of classes must be >= 3, IJI = NA.", call. = FALSE)
        }

        return(tibble::tibble(level = "class",
                              class = as.integer(classes),
                              id = as.integer(NA),
                              metric = "iji",
                              value = as.double(NA)))
    }

    else {

        diag(adjacencies) <- 0

        edge_ratio <- adjacencies / rowSums(adjacencies) *
            log(adjacencies / rowSums(adjacencies))

        class_sums <- -rowSums(edge_ratio, na.rm = TRUE)

        iji <- (class_sums / log(ncol(adjacencies) - 1)) * 100

        return(tibble::tibble(level = "class",
                              class = as.integer(classes),
                              id = as.integer(NA),
                              metric = "iji",
                              value = as.double(iji)))
    }
}
