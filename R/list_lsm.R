#' List landscape metrics
#'
#' @description List landscape metrics
#'
#' @param level Level of metrics. Either 'patch', 'class' or 'landscape' (or vector with combination).
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param simplify If true, function names are returned as vector.
#' @param verbose Print warning messages
#'
#' @details List all available landscape metrics depending on the provided filter
#' arguments. If an argument is not provided, automatically all possibilities are
#' selected. Therefore, to get **all** available metrics, use simply `list_lsm()`.
#' For all arguments with exception of the \code{what} argument, it is also possible to
#' use a negative subset, i.e. all metrics  **but** the selected ones. Therefore,
#' simply use e.g. \code{level = "-patch".} Furthermore, it is possible to only
#' get a vector with all function names instead of the full tibble.
#'
#' @return tibble
#'
#' @examples
#' list_lsm(level = c("patch", "landscape"), type = "aggregation metric")
#' list_lsm(level = "-patch", type = "area and edge metric")
#' list_lsm(metric = "area", simplify = TRUE)
#'
#' list_lsm(metric = "area", what = "lsm_p_shape")
#' list_lsm(metric = "area", what = c("patch", "lsm_l_ta"))
#' list_lsm(what = c("lsm_c_tca", "lsm_l_ta"))
#'
#' @aliases list_lsm
#' @rdname list_lsm
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
list_lsm <- function(level = NULL,
                     metric = NULL,
                     name = NULL,
                     type = NULL,
                     what = NULL,
                     simplify = FALSE,
                     verbose = TRUE) {

    lsm_abbreviations_names_modified <- landscapemetrics::lsm_abbreviations_names

    lsm_abbreviations_names_modified$metric_new <- vapply(X = strsplit(lsm_abbreviations_names_modified$metric,
                                                                       split = "_"),
                                                          FUN = function(x) x[1],
                                                          FUN.VALUE = character(1))

    if(!is.null(what)) {

        if(any(grepl(pattern = "[-]", x = what))) {
            stop("Negative strings not allowed for 'what' argument. Please use other arguments for negative subsets.",
                 call. = FALSE)
        }

        if(!is.null(c(level, metric, name, type))) {

            level  <- NULL
            metric <- NULL
            name <- NULL
            type <- NULL

            if(verbose) {warning("Only using 'what' argument.", call. = FALSE)}
        }

        if(any(what %in% c("patch", "class", "landscape"))){
            level <- what[what %in% c("patch", "class", "landscape")]
            what <- what[!what %in% c("patch", "class", "landscape")]
        }

        which_rows <- which(lsm_abbreviations_names_modified$function_name %in% what |
                                lsm_abbreviations_names_modified$level %in% level)

        result <- lsm_abbreviations_names_modified[which_rows, ]
    }

    else{

        if(is.null(level)) {

            level <- unique(lsm_abbreviations_names_modified$level)

        } else {

            if(any(grepl(pattern = "[-]", x = level))) {

                if(!all(pattern = grepl("[-]", x = level))) {
                    stop("Mixing of positive and negative strings as subset not allowed for the same argument.")
                }

                level_neg <- gsub(pattern = "[-]", replacement = "", x = level)
                level_neg_i <- which(lsm_abbreviations_names_modified$level %in% level_neg)
                lsm_abbreviations_names_modified <- lsm_abbreviations_names_modified[-level_neg_i, ]

                level <- unique(lsm_abbreviations_names_modified$level)
                level_i <- which(!(level %in% level_neg))
                level <- level[level_i]

            }
        }

        if(is.null(metric)){

            metric <- unique(lsm_abbreviations_names_modified$metric_new)

        } else {

            if(any(grepl(pattern = "[-]", x = metric))) {

                if(!all(pattern = grepl("[-]", x = metric))) {
                    stop("Mixing of positive and negative strings as subset not allowed for the same argument.")
                }

                metric_neg <- gsub(pattern = "[-]", replacement = "", x = metric)
                metric_neg_i <- which(lsm_abbreviations_names_modified$metric_new %in% metric_neg)
                lsm_abbreviations_names_modified <- lsm_abbreviations_names_modified[-metric_neg_i, ]

                metric <- unique(lsm_abbreviations_names_modified$metric_new)
                metric_i <- which(!(metric %in% metric_neg))
                metric <- metric[metric_i]
            }
        }

        if(is.null(name)){

            name <- unique(lsm_abbreviations_names_modified$name)

        } else {

            if(any(grepl(pattern = "[-]", x = name))) {

                if(!all(pattern = grepl("[-]", x = name))) {
                    stop("Mixing of positive and negative strings as subset not allowed for the same argument.")
                }

                name_neg <- gsub(pattern = "[-]", replacement = "", x = name)
                name_neg_i <- which(lsm_abbreviations_names_modified$name %in% name_neg)
                lsm_abbreviations_names_modified <- lsm_abbreviations_names_modified[-name_neg_i, ]

                name <- unique(lsm_abbreviations_names_modified$name)
                name_i <- which(!(name %in% name_neg))
                name <- name[name_i]
            }
        }

        if(is.null(type)){

            type <- unique(lsm_abbreviations_names_modified$type)

        } else {
            if(any(grepl(pattern = "[-]", x = type))) {

                if(!all(pattern = grepl("[-]", x = type))) {
                    stop("Mixing of positive and negative strings as subset not allowed for the same argument.")
                }

                type_neg <- gsub(pattern = "[-]", replacement = "", x = type)
                type_neg_i <- which(lsm_abbreviations_names_modified$type %in% type_neg)
                lsm_abbreviations_names_modified <- lsm_abbreviations_names_modified[-type_neg_i, ]

                type <- unique(lsm_abbreviations_names_modified$type)
                type_i <- which(!(type %in% type_neg))
                type <- type[type_i]
            }
        }

        which_rows <- which(lsm_abbreviations_names_modified$level %in% level &
                                lsm_abbreviations_names_modified$metric_new %in% metric &
                                lsm_abbreviations_names_modified$name %in% name &
                                lsm_abbreviations_names_modified$type %in% type)

        result <- lsm_abbreviations_names_modified[which_rows, ]
    }

    result <- result[, -6]

    if(simplify) {
        result <- result$function_name
    }

    return(result)
}

