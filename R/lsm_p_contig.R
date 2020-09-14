#' CONTIG (patch level)
#'
#' @description Contiguity index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CONTIG =  \frac{\Bigg[\frac{\sum\limits_{r=1}^z  c_{ijr}}{a_{ij}}\Bigg] - 1 }{ v - 1} }
#'
#' where \eqn{c_{ijr}} is the contiguity value for pixel r in patch ij,
#' \eqn{a_{ij}} the area of the respective patch (number of cells) and \eqn{v} is
#' the size of the filter matrix (13 in this case).
#'
#' CONTIG is a 'Shape metric'. It asses the spatial connectedness (contiguity) of
#' cells in patches. CONTIG coerces patch values to a value of 1 and the background
#' to NA. A nine cell focal filter matrix:
#'
#' ```
#' filter_matrix <- matrix(c(1, 2, 1,
#'                           2, 1, 2,
#'                           1, 2, 1), 3, 3, byrow = T)
#' ```
#' ... is then used to weight orthogonally contiguous pixels more heavily than
#' diagonally contiguous pixels. Therefore, larger and more connections between
#' patch cells in the rookie case result in larger contiguity index values.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 >= CONTIG <= 1}
#' \subsection{Behaviour}{Equals 0 for one-pixel patches and increases to a limit
#' of 1 (fully connected patch).}
#'
#' @seealso
#' \code{\link{lsm_c_contig_mn}},
#' \code{\link{lsm_c_contig_sd}},
#' \code{\link{lsm_c_contig_cv}}, \cr
#' \code{\link{lsm_l_contig_mn}},
#' \code{\link{lsm_l_contig_sd}},
#' \code{\link{lsm_l_contig_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_contig(landscape)
#'
#' @aliases lsm_p_contig
#' @rdname lsm_p_contig
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' LaGro, J. 1991. Assessing patch shape in landscape mosaics.
#' Photogrammetric Engineering and Remote Sensing, 57(3), 285-293
#'
#' @export
lsm_p_contig <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_contig_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_contig_calc <- function(landscape, directions) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "contig",
                              value = as.double(NA)))
    }

    # get unique values
    classes <- get_unique_values(landscape)[[1]]

    # diagonal neighbours
    diagonal_matrix <- matrix(c(1, NA, 1,
                                NA, 0, NA,
                                1, NA, 1), 3, 3, byrow = TRUE)

    # x-y neighbours
    straigth_matrix <- matrix(c(NA, 1, NA,
                                1, 0, 1,
                                NA, 1, NA), 3, 3, byrow = TRUE)

    contig_patch <- do.call(rbind,
                            lapply(classes, function(patches_class) {

        # get connected patches
        patch_mat <- get_patches(landscape,
                                 directions = directions,
                                 class = patches_class,
                                 return_raster = FALSE)[[1]]

        # get number of cells for each patch
        n_cells <- rcpp_get_composition_vector(patch_mat)

        # get number of patches
        n_patches <- length(n_cells)


        # get diagonal neighbours of same patch
        diagonal_neighbours <- rcpp_get_coocurrence_matrix_diag(patch_mat,
                                                                directions = as.matrix(diagonal_matrix))

        # get straight neighbours of same patch weighted twice
        straigth_neighbours <- rcpp_get_coocurrence_matrix_diag(patch_mat,
                                                                directions = as.matrix(straigth_matrix)) * 2

        # calculated contiguity
        contiguity <- (((diagonal_neighbours +
                             straigth_neighbours +
                             n_cells) /
                            n_cells) - 1) / 12

        class <- patches_class

        rm(patch_mat)
        gc(verbose = FALSE)

        tibble::tibble(class = class,
                       value = contiguity)

        })
    )

    tibble::tibble(
        level = "patch",
        class = as.integer(contig_patch$class),
        id = as.integer(seq_len(nrow(contig_patch))),
        metric = "contig",
        value = as.double(contig_patch$value)
    )
}
