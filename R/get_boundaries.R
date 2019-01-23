#' get_boundaries
#'
#' @description Get boundary cells of patches
#'
#' @param landscape RasterLayer or matrix.
#' @param directions Rook's case (4 neighbours) or queen's case (8 neighbours) should be used as neighbourhood rule
#' @param as_NA If true, non-boundary cells area labeld NA
#' @param return_raster If false, matrix is returned
#'
#' @details
#' All boundary/edge cells are labeled 1, all non-boundary cells 0. NA values are
#' not changed. Boundary cells are defined as cells that neighbour either a NA
#' cell or a cell with a different value than itself. Non-boundary cells only
#' neighbour cells with the same value than themself.
#'
#' @return RasterLayer or matrix
#'
#' @examples
#' class_1 <- get_patches(landscape, class = 1)[[1]]
#'
#' get_boundaries(class_1)
#' get_boundaries(class_1, return_raster = FALSE)
#'
#' class_1_matrix <- raster::as.matrix(class_1)
#' get_boundaries(class_1_matrix, return_raster = FALSE)
#'
#' @aliases get_boundaries
#' @rdname get_boundaries
#'
#' @export
get_boundaries <- function(landscape,
                           directions = 4,
                           as_NA = FALSE,
                           return_raster = TRUE){

    # check if either raster or matrix is provided
    if(class(landscape) != "RasterLayer" && class(landscape) != "matrix") {
        stop("Please provide RasterLayer or matrix as input.", call. = FALSE)
    }

    # check if either directions are possible
    if(directions != 4 && directions != 8) {
        stop("Please specify 'directions = 4' or 'directions = 8'.", call. = FALSE)
    }

    # input is raster
    if(class(landscape) == "RasterLayer") {

        # get boundaries
        result <- rcpp_get_boundaries(raster::as.matrix(landscape),
                                      directions = directions)

        if(as_NA) {
            result[which(result == 0)] <- NA
        }

        # convert back to raster
        if(return_raster) {
            result <- matrix_to_raster(matrix = result,
                                       landscape = landscape)
        }

        return(result)
    }

    # input is matrix
    else{

        # get boundaries
        result <- rcpp_get_boundaries(landscape,
                                      directions = directions)

        if(as_NA) {
            result[which(result == 0)] <- NA
        }

        if(return_raster) {
            warning("return_raster = TRUE not able for matrix input")
        }

        return(result)
    }
}

# get_boundaries_old <- function(landscape, directions = 4){
#
#     if(class(landscape) == "RasterLayer") {
#         landscape <- raster::as.matrix(landscape)
#     }
#
#     if(class(landscape) != "matrix") {
#         stop("Please provide matrix as input.", call. = FALSE)
#     }
#
#     landscape <- cbind(landscape[, 1], landscape, landscape[, ncol(landscape)])
#     landscape <- rbind(landscape[1,], landscape, landscape[nrow(landscape),])
#     paddim <- as.integer(dim(landscape))
#     classes <- 0L
#     type <- 0L
#     directions <- as.integer(directions)
#
#     x <- .Call('_edge', as.integer(t(landscape)), paddim, classes, type, directions, NAOK=TRUE, PACKAGE='raster')
#     x <- matrix(x, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
#     x <- x[2:(nrow(x)-1), 2:(ncol(x)-1)]
#     return(x)
# }
