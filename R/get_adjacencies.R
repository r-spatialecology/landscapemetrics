#' get_adjacencies
#'
#' @description Fast calculation of adjacencies between classes in a raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param neighbourhood The number of directions in which cell adjacencies are
#' considered as neighbours: 4 (rook's case), 8 (queen's case) or a binary matrix
#'  where the ones define the neighbourhood. The default is 4.
#' @param what Which adjacencies to calculate: "full" for a full adjacency matrix,
#' "like" for the diagonal, "unlike" for the off diagonal part of the matrix and
#'  "triangle" for a triangular matrix counting adjacencies only once.
#' @param upper Logical value indicating whether the upper triangle of the adjacency matrix
#' should be returned (default FALSE).
#'
#' @details
#' A fast implementation with Rcpp to calculate the adjacency matrix for raster.
#' The adjacency matrix is most often used in landscape metrics to describe
#' the configuration of landscapes, is it is a cellwise count of edges between classes.
#'
#' The "full" adjacency matrix is double-count method, as it contains the pairwise
#' counts of cells between all classes. The diagonal of this matrix contains the
#' like adjacencies, a count for how many edges a shared in each class with the same class.
#' The "unlike" adjacencies are counting the cellwise edges between different classes.
#'
#' @return matrix
#'
#' @examples
#'
#' # calculate full adjacency matrix
#' get_adjacencies(landscape, 4)
#'
#' # count diagonal neighbour adjacencies
#' diagonal_matrix <- matrix(c(1,  NA,  1,
#'                             NA,  0, NA,
#'                             1,  NA,  1), 3, 3, byrow = TRUE)
#' get_adjacencies(landscape, diagonal_matrix)
#'
#' # equivalent with the raster package:
#' adjacencies <- raster::adjacent(landscape, 1:raster::ncell(landscape), 4, pairs=TRUE)
#' table(landscape[adjacencies[,1]], landscape[adjacencies[,2]])
#'
#' @aliases get_adjacencies
#' @rdname get_adjacencies
#'
#' @export
get_adjacencies <- function(landscape,
                            neighbourhood = 4,
                            what = "full",
                            upper = FALSE){

    adjacencies <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                as.matrix(neighbourhood))

    if (isFALSE(upper)) {
        if (what == "like") {
            adjacencies[lower.tri(adjacencies) | upper.tri(adjacencies)] <- NA
        }

        if (what == "unlike") {
            adjacencies[!lower.tri(adjacencies, diag = FALSE)] <- NA
        }

        if (what == "triangle") {
            adjacencies[!lower.tri(adjacencies, diag = TRUE)] <- NA
        }
    } else{
        if (what == "like") {
            adjacencies[lower.tri(adjacencies) | upper.tri(adjacencies)] <- NA
        }

        if (what == "unlike") {
            adjacencies[!upper.tri(adjacencies, diag = FALSE)] <- NA
        }

        if (what == "triangle") {
            adjacencies[!upper.tri(adjacencies, diag = TRUE)] <- NA
        }
    }
    return(adjacencies)
}
