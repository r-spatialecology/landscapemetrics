#' get_unique_values
#'
#' @description This function returns the unique values of an object.
#'
#' @param x Vector, matrix, raster, stars, or terra object or list of previous.
#' @param simplify If true, a vector will be returned instead of a list for
#' 1-dimensional input
#' @param verbose If true, warning messages are printed
#'
#' @details
#' Fast and memory friendly Rcpp implementation to find the unique values of an object.
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#'
#' get_unique_values(landscape)
#'
#' landscape_stack <- c(landscape, landscape, landscape)
#' get_unique_values(landscape_stack)
#'
#' landscape_matrix <- terra::as.matrix(landscape, wide = TRUE)
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
get_unique_values <- function(x, simplify = FALSE, verbose = TRUE) {

    x <- landscape_as_list(landscape = x)

    result <- lapply(X = x, FUN = get_unique_values_int,
                     verbose = verbose)

    names(result) <- paste0("layer_", 1:length(x))

    if (simplify) {

        if (length(result) == 1) {

            return(result[[1]])

        } else if (verbose) {

                warning("Not able to simplify input with more than one layer.", call. = FALSE)

        }
    }

    return(result)
}

get_unique_values_int <- function(landscape, verbose) {

    if (inherits(x = landscape, what = "SpatRaster")) {

        landscape <- terra::as.matrix(landscape, wide = TRUE)

    } else if (!inherits(x = landscape, what = "matrix") &&
               !inherits(x = landscape, what = "integer") &&
               !inherits(x = landscape, what = "numeric")) {

        stop("Input must be vector, matrix, raster, stars, or terra object or list of previous.",
             call. = FALSE)

    }

    if (typeof(landscape) != "integer" && verbose) {

        warning("Double values will be converted to integer.", call. = FALSE)

    }

    sort(rcpp_get_unique_values(landscape))
}
