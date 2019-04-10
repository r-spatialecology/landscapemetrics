#' Check input landscape
#'
#' @description Check input landscape
#'
#' @param landscape Raster* Layer, Stack, Brick, Stars or a list of rasterLayers
#'
#' @details This function extracts basic information about
#' the input landscape.
#' It includes a type of coordinate reference system (crs) -
#' either "geographic", "projected", or NA,
#' units of the coordinate reference system, a class of the input landscape's
#' values and the number of classes found in the landscape.
#'
#' @return tibble
#'
#' @examples
#' check_landscape(augusta_nlcd)
#' check_landscape(podlasie_ccilc)
#' check_landscape(raster::stack(landscape, landscape))
#'
#' @aliases check_landscape
#' @rdname check_landscape
#'
#' @export
check_landscape <- function(landscape) UseMethod("check_landscape")

#' @name check_landscape
#' @export
check_landscape.RasterLayer <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = check_landscape_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name check_landscape
#' @export
check_landscape.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = check_landscape_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name check_landscape
#' @export
check_landscape.RasterBrick <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = check_landscape_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name check_landscape
#' @export
check_landscape.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = check_landscape_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name check_landscape
#' @export
check_landscape.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = check_landscape_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

check_landscape_calc <- function(landscape) {

    # get info about projection and class of values
    info <- cbind(proj_info(landscape), data_info(landscape))

    # test if all values are integer
    info$class_ok <- ifelse(test = info$class == "integer",
                            yes = "ok", no = "notok")

    # check if number of classes makes sense
    info$n_classes_ok <- ifelse(test = info$n_classes <= 30,
                                yes = "ok", no = "maybe")

    # check if units are degree
    info$units_ok <- ifelse(test = is.na(info$units),
                            yes = "maybe", no = ifelse(test = info$units != "degrees",
                                                       yes = "ok", no = "notok"))

    # set final OK
    info$OK <- ifelse(test = info$class_ok == "ok" && info$units_ok == "ok" && info$n_classes_ok == "ok",
                      yes = cli::symbol$tick,
                      no = ifelse(test = info$class_ok == "notok" || info$units_ok == "notok",
                                  yes = cli::symbol$cross,
                                  no = ifelse(test = info$class_ok == "maybe" || info$units_ok == "maybe" || info$n_classes_ok == "maybe",
                                              yes = cli::symbol$circle_question_mark,
                                              no = NA)))

    info <- info[, c("crs", "units", "class", "n_classes", "OK")]

    if (info$class != "integer") {
        message("> Caution: Land-cover classes must be decoded as integer values.")
    }

    if (is.na(info$units) || info$units == "degrees") {
        message("> Caution: Coordinate reference system not metric - Units of results based on cellsizes and/or distances may be incorrect.")
    }

    if (info$n_classes > 30) {
        message("> Caution: More than 30 land cover-classes - Please check if discrete land-cover classes are present.")
    }

    return(info)
}
