#' get_unique_values
#'
#' @description This function returns the unique values of an object.
#'
#' @param x vector, matrix or Raster* object
#' @param simplify If true, a vector will be returned instead of a list for
#' 1-dimensional input
#'
#' @details
#' Fast and memory friendly Rcpp implementation to find the unique values of an object.
#'
#' @examples
#' get_unique_values(landscape)
#'
#' landscape_stack <- raster::stack(landscape, landscape, landscape)
#' get_unique_values(landscape_stack)
#'
#' landscape_matrix <- raster::as.matrix(landscape)
#' get_unique_values(landscape_matrix)
#'
#' x_vec <- c(1, 2, 1, 1, 2, 2)
#' get_unique_values(x_vec)
#'
#' landscape_list <- list(landscape, landscape_matrix, x_vec)
#' get_unique_values(landscape_list)
#'
#' @aliases get_unique_values
#' @rdname get_unique_values
#'
#' @export

get_unique_values <- function(x, simplify) UseMethod("get_unique_values")

#' @name get_unique_values
#' @export
get_unique_values.numeric <- function(x, simplify = FALSE){

    if(typeof(x) != "integer") {
        warning("Double values will be converted to integer", call. = FALSE)
    }

    unique_values <- rcpp_get_unique_values(x)

    if(simplify) {
        return(unique_values)
    }

    else{
        return(list(unique_values))
    }
}

#' @name get_unique_values
#' @export
get_unique_values.matrix <- function(x, simplify = FALSE){

    return(get_unique_values.numeric(x, simplify = simplify))

}

#' @name get_unique_values
#' @export
get_unique_values.list <- function(x, simplify = FALSE){

    unique_values <- lapply(x, FUN = function(current_element) {

        # use simplify = TRUE here to avoid lists of lists
        if(class(current_element) == "RasterLayer") {

            return(get_unique_values.RasterLayer(current_element, simplify = TRUE))

        } else if(class(current_element) == "numeric" ||
                  class(current_element) == "double" ||
                  class(current_element) == "integer") {

            return(get_unique_values.numeric(current_element, simplify = TRUE))

        } else if(class(current_element) == "matrix") {

            get_unique_values.matrix(get_unique_values.matrix, simplify = TRUE)

        } else{

            stop("List elements must be a RasterLayer, matrix or vector.")

        }
    })

    if(simplify) {
        if(length(x) == 1) {
            return(unique_values[[1]])
        }

        else {
            warning("Not able to simply list with more than 1 element.", call. = FALSE)
        }
    }

    return(unique_values)
}

#' @name get_unique_values
#' @export
get_unique_values.RasterLayer <- function(x, simplify = FALSE){

    if (!raster::inMemory(x)) {

        if (raster::fromDisk(x)) {

            if (raster::canProcessInMemory(x, 2)) {

                x <- raster::readAll(x)
            }
        }

        else {
            stop('RasterLayer has no values.')
        }
    }

    if (raster::inMemory(x)) {
        unique_values <- rcpp_get_unique_values(x@data@values)
    }

    else {
        block_1 <- vector()
        block_2 <- vector()

        tr <- raster::blockSize(x, n = 2)

        for (i in 1:tr$n) {

            block_1 <- rcpp_get_unique_values(c(block_1,
                                                raster::getValuesBlock(x,
                                                                       row = tr$row[i],
                                                                       nrows = tr$nrows[i])))
            if (length(block_1) > 10000) {

                block_2 <- rcpp_get_unique_values(c(block_1, block_2))
                block_1 <- vector()
            }
        }

        unique_values <- rcpp_get_unique_values(c(block_1, block_2))
    }

    if(simplify) {
        return(unique_values)
    }

    else {
        return(list(unique_values))
    }
}

#' @name get_unique_values
#' @export
get_unique_values.RasterStack <- function(x, simplify = FALSE){

    x <- raster::as.list(x)
    unique_values <- get_unique_values.list(x)

    if(simplify) {
        warning("Not able to simplify RasterStack.", call. = FALSE)
    }

    return(unique_values)
}

#' @name get_unique_values
#' @export
get_unique_values.RasterBrick <- function(x, simplify = FALSE){

    x <- raster::as.list(x)
    unique_values <- get_unique_values.list(x)

    if(simplify) {
        warning("Not able to simplify RasterBrick.", call. = FALSE)
    }

    return(unique_values)
}
