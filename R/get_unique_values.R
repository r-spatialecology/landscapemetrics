#' get_unique_values
#'
#' @description This function returns the unique values of an object.
#'
#' @param x vector, matrix or Raster* object
#'
#' @details
#' Fast and memory friendly Rcpp implementation to find the unique values of an object.
#'
#' @examples
#' get_unique_values(landscape)
#'
#' @aliases get_unique_values
#' @rdname get_unique_values
#'
#' @export

get_unique_values <- function(x) UseMethod("get_unique_values")

#' @name get_unique_values
#' @export
get_unique_values.vector <- function(x){
    list(get_unique_values_int(x))
}

#' @name get_unique_values
#' @export
get_unique_values.matrix <- function(x){
    list(get_unique_values_int(x))
}

#' @name get_unique_values
#' @export
get_unique_values.RasterLayer <- function(x){
    if (!raster::inMemory(x)) {
        if (raster::fromDisk(x)) {
            if (raster::canProcessInMemory(x, 2)) {
                x <- raster::readAll(x)
            }
        } else {
            stop('RasterLayer has no values')
        }
    }

    if (raster::inMemory(x)) {
        return(list(get_unique_values_int(x@data@values)))
    } else {
        u1 <- vector()
        u2 <- vector()

        tr <- raster::blockSize(x, n = 2)
        for (i in 1:tr$n) {
            u1 <- get_unique_values_int(
                c(u1, raster::getValuesBlock(x, row = tr$row[i], nrows = tr$nrows[i])))
            if (length(u1) > 10000) {
                u2 <- get_unique_values_int(c(u1, u2))
                u1 <- vector()
            }
        }
        return(list(get_unique_values_int(c(u1, u2))))
    }
}

#' @name get_unique_values
#' @export
get_unique_values.RasterStack <- function(x){

    if (!raster::inMemory(x)) {
        if (raster::canProcessInMemory(x, 2)) {
            x <- raster::readAll(x)
        }
    }

    if (raster::inMemory(x)) {

        x <- get_unique_values_int(x@data@values)
        if (!is.list(x)) {
            xx <- vector(length = ncol(x), mode = 'list')
            for (i in 1:ncol(x)) {
                xx[[i]] <- x[,i]
            }
            x <- xx
        }
        return(x)
    } else {
        nl <- raster::nlayers(x)
        un <- list(length = nl, mode = 'list')
        tr <- raster::blockSize(x, n = 2)
        un <- NULL
        for (i in 1:tr$n) {
            v <- get_unique_values_int(
                raster::getValuesBlock(x, row = tr$row[i], nrows = tr$nrows[i]) )
            un <- get_unique_values_int(rbind(v, un))
        }
        return(un)
    }

}

#' @name get_unique_values
#' @export
get_unique_values.RasterBrick <- function(x){
    return(get_unique_values.RasterStack(x))
}

get_unique_values_int <- function(x){
    rcpp_get_unique_values(x)
}
