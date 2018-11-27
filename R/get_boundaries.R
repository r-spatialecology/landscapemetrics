#' get_boundaries
#'
#' @description Streamlined version of raster::boundaries that takes a matrix as input
#'
#' @param matrix matrix with values.
#' @param directions 4 (rooks case), 8 (queens case)
#'
#' @details
#' Stripped down version of raster::boundaries so that we internally can parse
#' a matrix.
#'
#' @references Robert J. Hijmans (2018). raster: Geographic Data Analysis and Modeling. R package version 2.8-4.
#' https://CRAN.R-project.org/package=raster
#'
#' @return matrix
#'
#' @examples
#' test_matrix <- get_patches(landscape, class = 1, return_raster = FALSE)[[1]]
#' get_boundaries(test_matrix, directions = 4)
#'
#' @aliases get_boundaries
#' @rdname get_boundaries
#'
#' @keywords internal
#'
#' @export

get_boundaries <- function(landscape, directions = 4){

    landscape <- cbind(landscape[, 1], landscape, landscape[, ncol(landscape)])
    landscape <- rbind(landscape[1,], landscape, landscape[nrow(landscape),])
    paddim <- as.integer(dim(landscape))
    classes <- 0L
    type <- 0L
    directions <- as.integer(directions)

    x <- .Call('_edge', as.integer(t(landscape)), paddim, classes, type, directions, NAOK=TRUE, PACKAGE='raster')
    x <- matrix(x, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
    x <- x[2:(nrow(x)-1), 2:(ncol(x)-1)]
    return(x)
}
