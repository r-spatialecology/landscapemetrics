#' get_lsm
#'
#' @description Get landscape metric values
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param level Level of metrics to calculate (e.g. 'landscape').
#' @param metric Abbreviation of metrics to calculate (e.g. 'area').
#' @param name Full name of metrics to calculate (e.g. 'core area').
#' @param type Metric types to calculate according to FRAGSTATS grouping (e.g. 'aggregation metric').
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param count_boundary Include landscape boundary in edge length
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#' @param classes_max Potential maximum number of present classes
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured. The default is "log2",
#' which compute entropy in "bits". "log" and "log10" can be also used.
#' @param verbose Print warning messages
#'
#' @details
#' The functions returns a nested list with \code{RasterLayer}s. The first level
#' contains each input layer (only one element if \code{RasterLayer} was provided).
#' The second level contains a \code{RasterLayer} for each selected metric
#' (see \code{list_lsm} for details) where each cell has the landscape metric
#' value of the patch it belongs to. Only patch level metrics are allowed.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{show_lsm}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' get_lsm(landscape, what = "lsm_p_area")
#' get_lsm(landscape_stack, what = c("lsm_p_area", "lsm_p_perim"))
#' }
#'
#' @aliases get_lsm
#'
#' @rdname get_lsm
#'
#' @export
get_lsm <- function(landscape,
                    what,
                    level,
                    metric,
                    name,
                    type,
                    directions,
                    count_boundary,
                    consider_boundary,
                    edge_depth,
                    classes_max,
                    neighbourhood,
                    ordered,
                    base,
                    verbose) UseMethod("get_lsm")

#' @name get_lsm
#' @export
get_lsm.RasterLayer <- function(landscape,
                                what = NULL,
                                level = "patch",
                                metric = NULL,
                                name = NULL,
                                type = NULL,
                                directions = 8,
                                count_boundary = FALSE,
                                consider_boundary = FALSE,
                                edge_depth = 1,
                                classes_max = NULL,
                                neighbourhood = 4,
                                ordered = TRUE,
                                base = "log2",
                                verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_lsm_internal,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     verbose = verbose)

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.RasterStack <- function(landscape,
                                what = NULL,
                                level = "patch",
                                metric = NULL,
                                name = NULL,
                                type = NULL,
                                directions = 8,
                                count_boundary = FALSE,
                                consider_boundary = FALSE,
                                edge_depth = 1,
                                classes_max = NULL,
                                neighbourhood = 4,
                                ordered = TRUE,
                                base = "log2",
                                verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_lsm_internal,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     verbose = verbose)

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.RasterBrick <- function(landscape,
                                what = NULL,
                                level = "patch",
                                metric = NULL,
                                name = NULL,
                                type = NULL,
                                directions = 8,
                                count_boundary = FALSE,
                                consider_boundary = FALSE,
                                edge_depth = 1,
                                classes_max = NULL,
                                neighbourhood = 4,
                                ordered = TRUE,
                                base = "log2",
                                verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_lsm_internal,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     verbose = verbose)

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.stars <- function(landscape,
                          what = NULL,
                          level = "patch",
                          metric = NULL,
                          name = NULL,
                          type = NULL,
                          directions = 8,
                          count_boundary = FALSE,
                          consider_boundary = FALSE,
                          edge_depth = 1,
                          classes_max = NULL,
                          neighbourhood = 4,
                          ordered = TRUE,
                          base = "log2",
                          verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_lsm_internal,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     verbose = verbose)

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.list <- function(landscape,
                         what = NULL,
                         level = "patch",
                         metric = NULL,
                         name = NULL,
                         type = NULL,
                         directions = 8,
                         count_boundary = FALSE,
                         consider_boundary = FALSE,
                         edge_depth = 1,
                         classes_max = NULL,
                         neighbourhood = 4,
                         ordered = TRUE,
                         base = "log2",
                         verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = get_lsm_internal,
                     what = what,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     verbose = verbose)

    return(result)
}

get_lsm_internal <- function(landscape,
                             what,
                             level,
                             metric,
                             name,
                             type,
                             directions,
                             count_boundary,
                             consider_boundary,
                             edge_depth,
                             classes_max,
                             neighbourhood,
                             ordered,
                             base,
                             verbose) {

    # get all patch level metrics to check
    patch_metrics <- landscapemetrics::list_lsm(level = "patch", simplify = TRUE)

    # get name of metrics
    metrics <- landscapemetrics::list_lsm(level = level,
                                          metric = metric,
                                          name = name,
                                          type = type,
                                          what = what,
                                          simplify = TRUE,
                                          verbose = verbose)

    # error if no patch level metrics are provided
    if(!all(metrics %in% patch_metrics) || length(metrics) == 0){
        stop("Please provide (at least one) patch level metrics only. To list available metrics, run list_lsm(level = 'patch').",
             call. = FALSE)
    }

    # get CRS of input
    crs_input <- raster::crs(landscape)

    # get patches
    landscape_labeled <- get_patches(landscape, directions = directions)

    # continious, unique patch id
    for(i in seq_len(length(landscape_labeled) - 1)){

        max_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_id
    }

    # get dataframe with patch ID and coordinates to merge with result of metric
    patches_tibble <- raster::as.data.frame(sum(raster::stack(landscape_labeled),
                                                na.rm = TRUE),
                                            xy = TRUE)

    # modify names
    names(patches_tibble) <- c("x", "y", "id")

    # replace all 0 values for NA
    patches_tibble$id <- replace(patches_tibble$id,
                                 patches_tibble$id == 0,
                                 NA)

    # loop through metrics and return raster with value for each patch
    result <- lapply(metrics, function(x) {

        # get metric value
        fill_value <- calculate_lsm(landscape, what = x,
                                    directions = directions,
                                    count_boundary = count_boundary,
                                    consider_boundary = consider_boundary,
                                    edge_depth = edge_depth,
                                    classes_max = classes_max,
                                    neighbourhood = neighbourhood,
                                    ordered = ordered,
                                    base = base,
                                    verbose = verbose)

        # merge with coords data frame
        fill_value <-  merge(x = patches_tibble,
                             y = fill_value,
                             by = "id",
                             all.x = TRUE)

        # convert to raster
        raster::rasterFromXYZ(fill_value[, c(2,3, 8)], crs = crs_input)
    })

    # using metrics to name list
    names(result) <- metrics

    return(result)
}
