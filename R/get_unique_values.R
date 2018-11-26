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
    list(get_unique_values_int(raster::as.matrix(x)))
}

#' @name get_unique_values
#' @export
get_unique_values.RasterStack <- function(x){

    result <- lapply(X = raster::as.list(x),
           FUN = raster::as.matrix)

    result <- lapply(X =result,
                     FUN = get_unique_values_int)

    return(result)
}

#' @name get_unique_values
#' @export
get_unique_values.RasterBrick <- function(x){

    result <- lapply(X = raster::as.list(x),
                     FUN = raster::as.matrix)

    result <- lapply(X =result,
                     FUN = get_unique_values_int)

    return(result)
}

get_unique_values_int <- function(x){
    rcpp_get_unique_values(x)
}
