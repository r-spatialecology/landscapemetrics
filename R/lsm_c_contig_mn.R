#' CONTIG_MN (class level)
#'
#' @description Mean of Contiguity index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CONTIG_{MN} =  mean(CONTIG[patch_{ij}])}
#'
#' where \eqn{CONTIG[patch_{ij}]} is the contiguity of each patch.
#'
#' CONTIG_MN is a 'Shape metric'. It summarises each class as the mean of each patch
#' belonging to class i. CONTIG_MN asses the spatial connectedness (contiguity) of
#' cells in patches. The metric coerces patch values to a value of 1 and the background
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
#' \subsection{Range}{0 >= CONTIG_MN <= 1}
#' \subsection{Behaviour}{CONTIG equals the mean of the contiguity index on class level for all
#'  patches.}
#'
#' @seealso
#' \code{\link{lsm_p_contig}},
#' \code{\link{lsm_c_contig_sd}},
#' \code{\link{lsm_c_contig_cv}}, \cr
#' \code{\link{lsm_l_contig_mn}},
#' \code{\link{lsm_l_contig_sd}},
#' \code{\link{lsm_l_contig_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_contig_mn(landscape)
#'
#' @aliases lsm_c_contig_mn
#' @rdname lsm_c_contig_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_contig_mn <- function(landscape, directions) UseMethod("lsm_c_contig_mn")

#' @name lsm_c_contig_mn
#' @export
lsm_c_contig_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_contig_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_contig_mn
#' @export
lsm_c_contig_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_contig_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_contig_mn
#' @export
lsm_c_contig_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_contig_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_contig_mn
#' @export
lsm_c_contig_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_contig_mn_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_contig_mn_calc <- function(landscape, directions) {

    contig_mn  <- landscape %>%
        lsm_p_contig_calc(directions = directions) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(contig_mn$class),
        id = as.integer(NA),
        metric = "contig_mn",
        value = as.double(contig_mn$value)
    )

}
