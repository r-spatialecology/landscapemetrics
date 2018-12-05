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
#' x_vec <- c(1, 2, 3, 1, 1, 2, 2, 3)
#' get_unique_values(x_vec)
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

    return(list(unique_values))
}

#' @name get_unique_values
#' @export
get_unique_values.matrix <- function(x, simplify = FALSE){

    if(typeof(x) != "integer") {
        warning("Double values will be converted to integer", call. = FALSE)
    }

    unique_values <- rcpp_get_unique_values(x)

    return(list(unique_values))
}

#' @name get_unique_values
#' @export
get_unique_values.list <- function(x, simplify = FALSE){

    # if(typeof(x) != "integer") {
    #     warning("Double values will be converted to integer", call. = FALSE)
    # }
    #
    # unique_values <- rcpp_get_unique_values(x)
    #
    # return(list(unique_values))
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
            stop('RasterLayer has no values')
        }
    }

    if (raster::inMemory(x)) {

        unique_values <- rcpp_get_unique_values(x@data@values)

        return(list(unique_values))

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

        return(list(unique_values))
    }
}

#' @name get_unique_values
#' @export
get_unique_values.RasterStack <- function(x, simplify = FALSE){

    x <- raster::as.list(x)

    unique_values <- lapply(x, FUN = function(current_raster) {

        if (!raster::inMemory(current_raster)) {

            if (raster::fromDisk(current_raster)) {

                if (raster::canProcessInMemory(current_raster, 2)) {

                    current_raster <- raster::readAll(current_raster)
                }
            }

            else {
                stop('RasterLayer has no values')
            }
        }

        if (raster::inMemory(current_raster)) {

            unique_values <- rcpp_get_unique_values(current_raster@data@values)

            return(unique_values)

        }

        else {
            block_1 <- vector()
            block_2 <- vector()

            tr <- raster::blockSize(current_raster, n = 2)

            for (i in 1:tr$n) {

                block_1 <- rcpp_get_unique_values(c(block_1,
                                                   raster::getValuesBlock(current_raster,
                                                                          row = tr$row[i],
                                                                          nrows = tr$nrows[i])))
                if (length(block_1) > 10000) {

                    block_2 <- rcpp_get_unique_values(c(block_1, block_2))
                    block_1 <- vector()
                }
            }

            unique_values <- rcpp_get_unique_values(c(block_1, block_2))

            return(unique_values)
        }
    })

    return(unique_values)
}

#' @name get_unique_values
#' @export
get_unique_values.RasterBrick <- function(x, simplify = FALSE){

    x <- raster::as.list(x)

    unique_values <- lapply(x, FUN = function(current_raster) {

        if (!raster::inMemory(current_raster)) {

            if (raster::fromDisk(current_raster)) {

                if (raster::canProcessInMemory(current_raster, 2)) {

                    current_raster <- raster::readAll(current_raster)
                }
            }

            else {
                stop('RasterLayer has no values')
            }
        }

        if (raster::inMemory(current_raster)) {

            unique_values <- rcpp_get_unique_values(current_raster@data@values)

            return(unique_values)

        }

        else {
            block_1 <- vector()
            block_2 <- vector()

            tr <- raster::blockSize(current_raster, n = 2)

            for (i in 1:tr$n) {

                block_1 <- rcpp_get_unique_values(c(block_1,
                                                   raster::getValuesBlock(current_raster,
                                                                          row = tr$row[i],
                                                                          nrows = tr$nrows[i])))
                if (length(block_1) > 10000) {

                    block_2 <- rcpp_get_unique_values(c(block_1, block_2))
                    block_1 <- vector()
                }
            }

            unique_values <- rcpp_get_unique_values(c(block_1, block_2))

            return(unique_values)
        }
    })

    return(unique_values)
}

# get_unique_values_int <- function(x){
#     rcpp_get_unique_values(x)
# }
