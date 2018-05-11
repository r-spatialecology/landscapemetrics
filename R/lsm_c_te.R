#' Total Edge  (TE, class scale)
#'
#' @description Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param class Which class
#'
#' @return tibble
#'
#' @examples
#' lsm_c_te(landscape, 0.5)
#'
#' @aliases lsm_c_te
#' @rdname lsm_c_te
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#'
#'
lsm_c_te <- function(landscape,
                     class){

    if (raster::nlayers(landscape) == 1) {
        # calculate total edge length by mapping over every class

        # cclabel class
        cclabeled_raster <- cclabel(landscape, class)

        # set background to calculate number of neighbors next to cells with
        # values of -999
        cclabeled_raster[is.na(cclabeled_raster)] <- -999

        # compute neighborhood matrix
        adjacent_cells <- raster::adjacent(cclabeled_raster,
                                           1:raster::ncell(cclabeled_raster),
                                           4,
                                           pairs=TRUE)
        # count whos neighbor of who
        tb <- table(cclabeled_raster[adjacent_cells[,1]],
                    cclabeled_raster[adjacent_cells[,2]])


        # return first row with counts of adjents sites between patches and
        # cells with -999


        total_edge <- tibble::tibble(
            layer = as.numeric(1),
            level = 'landscape',
            id = as.numeric(NA),
            metric = 'total edge',
            value = (sum(tb[2:ncol(tb),1])/2) * prod(raster::res(landscape))
        )
    } else {
        ### stack
    }


    return(total_edge)

}
