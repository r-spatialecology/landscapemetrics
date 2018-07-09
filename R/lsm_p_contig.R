#' CONTIG (patch level)
#'
#' @description Contiguity index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' @export
lsm_p_contig <- function(landscape) UseMethod("lsm_p_contig")

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc,.id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}


lsm_p_contig_calc <- function(landscape) {

    landscape_labelled <- cclabel(landscape)

    contig_patch <- purrr::map_dfr(landscape_labelled, function(patches_class) {

        cells <- raster::Which(!is.na(patches_class), cells = TRUE)
        n_cells <- table(raster::values(patches_class))
        n_patches <- length(n_cells)

        diagonal_matrix <- matrix(c(1, NA, 1,
                                    NA, 0, NA,
                                    1, NA, 1), 3, 3, byrow = T)

        straigth_matrix <- matrix(c(NA, 1, NA,
                                     1, 0, 1,
                                    NA, 1, NA), 3, 3, byrow = T)

        diagonal_neighbours <- raster::adjacent(patches_class,
                                                cells = cells,
                                                directions = diagonal_matrix)

        straigth_neighbours <- raster::adjacent(patches_class,
                                                cells = cells,
                                                directions = straigth_matrix)

        tb_diagonal <- table(factor(patches_class[diagonal_neighbours[,1]],
                                    levels = 1:n_patches),
                             factor(patches_class[diagonal_neighbours[,2]],
                                    levels = 1:n_patches))

        tb_straight <- table(factor(patches_class[straigth_neighbours[,1]],
                                    levels = 1:n_patches),
                             factor(patches_class[straigth_neighbours[,2]],
                                    levels = 1:n_patches)) * 2

        contiguity <- (((diag(tb_diagonal) + diag(tb_straight) + n_cells) /
                            n_cells) - 1) / 12

        class_name <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        tibble::tibble(class = class_name,
                       value = contiguity)
    })

    tibble::tibble(
        level = "patch",
        class = as.integer(contig_patch$class),
        id = as.integer(seq_len(nrow(contig_patch))),
        metric = "contiguity",
        value = as.double(contig_patch$value)
    )
}
