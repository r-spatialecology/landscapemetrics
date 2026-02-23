#' Interspersion and Juxtaposition index (class level)
#
#' @description Interspersion and Juxtaposition index (Aggregation metric)
#
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_iji(landscape)
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
lsm_c_iji <- function(landscape, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         iji <- lsm_c_iji_calc(
                             landscape_mat = landscape_mat,
                             verbose = verbose
                         )

                         lsm_class_output(metric = "iji",
                                          class = as.integer(names(iji)),
                                          value = unname(iji))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_iji_calc <- function(landscape_mat, verbose = TRUE, neighbor_matrix = NULL) {

    # all cells are NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(neighbor_matrix)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            required = c("neighbor_matrix"),
            neighbourhood = 4
        )
        neighbor_matrix <- deps$neighbor_matrix
    }

    adjacencies <- neighbor_matrix

    classes <- rownames(adjacencies)

    if (ncol(adjacencies) < 3) {

        if (verbose) {
            warning("Number of classes must be >= 3, IJI = NA.", call. = FALSE)
        }

        return(stats::setNames(rep(as.double(NA), length(classes)), classes))
    }

    else {

        diag(adjacencies) <- 0

        edge_ratio <- adjacencies / rowSums(adjacencies) *
            log(adjacencies / rowSums(adjacencies))

        class_sums <- -rowSums(edge_ratio, na.rm = TRUE)

        iji <- (class_sums / log(ncol(adjacencies) - 1)) * 100

        # return named vector
        stats::setNames(as.double(iji), classes)
    }
}
