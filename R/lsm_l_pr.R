#' PR (landscape level)
#'
#' @description Patch richness (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#'
#' @details
#' \deqn{PR = m}
#' where \eqn{m} is the number of classes
#'
#' PR is a 'Diversity metric'. It is one of the simplest diversity and composition measures.
#' However, because of its absolute nature, it is not comparable among landscapes with
#' different total areas.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PR >= 1}
#' \subsection{Behaviour}{Equals PR = 1 when only one patch is present and increases, without
#' limit, as the number of classes increases}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_pr(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_pr <- function(landscape){
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_pr_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_pr_calc <- function(landscape, extras = NULL){

    if (!is.null(extras)){
        classes <- extras$classes
    } else {
        classes <- get_unique_values_int(landscape, verbose = FALSE)
    }
    richness <- length(classes)

    # all values NA
    if (richness == 0) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pr",
                              value = as.double(NA))))
    }

    return(tibble::new_tibble(list(level = rep("landscape", length(richness)),
                          class = rep(as.integer(NA), length(richness)),
                          id = rep(as.integer(NA), length(richness)),
                          metric = rep("pr", length(richness)),
                          value = as.double(richness))))
}
