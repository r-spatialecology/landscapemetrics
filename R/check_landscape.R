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
#' check_landscape(landscape)
#' landscape_stack = raster::stack(landscape, landscape)
#' check_landscape(landscape_stack)
#'
#' @aliases check_landscape
#' @rdname check_landscape
#'
#' @export
check_landscape <- function(landscape) UseMethod("check_landscape")

#' @name check_landscape
#' @export
check_landscape.RasterLayer <- function(landscape) {

   check_landscape_calc(raster::as.list(landscape))

}

#' @name check_landscape
#' @export
check_landscape.RasterStack <- function(landscape) {

    check_landscape_calc(raster::as.list(landscape))
}

#' @name check_landscape
#' @export
check_landscape.RasterBrick <- function(landscape) {

    check_landscape_calc(raster::as.list(landscape))
}

#' @name check_landscape
#' @export
check_landscape.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    check_landscape_calc(raster::as.list(landscape))

}

#' @name check_landscape
#' @export
check_landscape.list <- function(landscape) {

    check_landscape_calc(landscape)

}

proj_info = function(landscape){
    landscape_proj <- raster::projection(landscape)
    if (!is.na(landscape_proj)){
        if(raster::isLonLat(landscape)){
            data.frame(crs = "geographic", units = "degrees")
        } else{
            proj_units <- strsplit(sub(".*units=", "", landscape_proj), " ", fixed = TRUE)[[1]][[1]]
            data.frame(crs = "projected", units = proj_units)
        }
    } else {
        data.frame(crs = "NA", units = "NA")
    }
}

data_info <- function(landscape){
    data.frame(class = class(raster::getValues(landscape)),
               n_classes = length(raster::unique(landscape)))
}

check_landscape_calc <- function(landscape){

    info <- lapply(X = landscape,
           FUN = function(x){
               x <- cbind(proj_info(x), data_info(x))
               invisible(x)
           })

    info <- dplyr::mutate(dplyr::bind_rows(info, .id = "layer"),
                  layer = as.integer(layer))

    info <- dplyr::mutate(
        info,
        class_ok = dplyr::case_when(class != "integer" ~ "notok",
                                    class == "integer" ~ "ok"),
        units_ok = dplyr::case_when(units != "degrees" ~ "ok",
                                    units == "degrees" ~ "notok"),
        OK = dplyr::case_when(class_ok == "ok" & units_ok == "ok" ~ cli::symbol$tick,
                              TRUE ~ cli::symbol$cross)
    )

    info <- dplyr::select(info, layer, crs, units, class, n_classes, OK)

    print(info)

    correct_class <- info$class
    correct_units <- info$units
    correct_nclasses <- info$n_classes

    if (any(correct_class != "integer")) {
        cat("\n")
        cat(crayon::yellow$bold("Caution:"))
        cat("\n Only integer values for your classes are allowed as input for landscapemetrics.")

    }

    if (any(correct_units == "degrees")) {
        cat("\n")
        cat(crayon::yellow$bold("Caution:"))
        cat(
            "\n Only metric coordinates make sense for landscapemetrics. You will still get results, but these are not comparable with other studies and the interpretation of metrics that use the cellsize as input becomes error prone."
        )

    }

    if (any(correct_nclasses > 30)) {
        cat("\n")
        cat(crayon::yellow$bold("Caution:"))
        cat(
            "\n Landscape metrics describe categorical landscape patterns.
            You have more than 30 land cover classes, which seems very high (but can make sense in some cases, if so ignore this message).
            However, if you did not think about classifying your landscapes before using landscapemetrics, we recommend reading our background vignette to familiarize yourself with
            the basic concepts behind the metrics: https://r-spatialecology.github.io/landscapemetrics/articles/articles/general-background.html"
        )

    }

    invisible(info)
}
