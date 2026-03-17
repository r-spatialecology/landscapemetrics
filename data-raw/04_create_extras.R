library(purrr)
library(stringr)
library(dplyr)

# NEW: Extract dependencies from function parameter signatures
get_deps_from_signature <- function(metric_name, lsm_dir) {
    file_path <- paste0(lsm_dir, "R/", metric_name, ".R")
    if (!file.exists(file_path)) return(NULL)
    
    lines <- readLines(file_path)
    calc_func_name <- paste0(metric_name, "_calc")
    
    # Find the _calc function definition
    calc_start <- grep(paste0("^", calc_func_name, "\\s*<-\\s*function\\("), lines)
    if (length(calc_start) == 0) return(NULL)
    
    # Get function signature (may span multiple lines)
    sig_lines <- lines[calc_start[1]:(calc_start[1] + 20)]
    sig_text <- paste(sig_lines, collapse = " ")
    
    # Extract parameter names
    deps <- character(0)
    if (grepl("\\bclasses\\b", sig_text)) deps <- c(deps, "classes")
    if (grepl("\\bclass_patches\\b", sig_text)) deps <- c(deps, "class_patches")
    if (grepl("\\barea_patches\\b", sig_text)) deps <- c(deps, "area_patches")
    if (grepl("\\bperimeter_patch\\b", sig_text)) deps <- c(deps, "perimeter_patch")
    if (grepl("\\bpoints\\b", sig_text)) deps <- c(deps, "points")
    if (grepl("\\benn_patch\\b", sig_text)) deps <- c(deps, "enn_patch")
    if (grepl("\\bcomposition_vector\\b", sig_text)) deps <- c(deps, "composition_vector")
    if (grepl("\\bcomp\\b", sig_text)) deps <- c(deps, "comp")
    if (grepl("\\bcplx\\b", sig_text)) deps <- c(deps, "cplx")
    if (grepl("\\bneighbor_matrix\\b", sig_text)) deps <- c(deps, "neighbor_matrix")
    if (grepl("\\bcore_patch\\b", sig_text)) deps <- c(deps, "core_patch")
    
    if (length(deps) > 0) {
        return(data.frame(metric = metric_name, extras = deps))
    }
    return(NULL)
}

all_lsms <- landscapemetrics::list_lsm()

# Extract from function signatures (new pattern)
db_extras_new <- map_df(all_lsms$function_name, get_deps_from_signature, lsm_dir = "~/Software/landscapemetrics/")

# Combine all sources
extras_df <- db_extras_new |>
    distinct(metric, extras) |>
    arrange(metric, extras)

# create new environment
my_new_env <- new.env(hash = FALSE)

# load current internal data into this new environment
load("R/sysdata.rda", envir = my_new_env)

# add or replace some objects
my_new_env$extras_df <- extras_df

# save the environment as internal package data
save(list = names(my_new_env),
     file = "R/sysdata.rda",
     envir = my_new_env,
     compress = "xz")

cat("Generated extras_df with", nrow(extras_df), "rows\n")
cat("Unique metrics:", length(unique(extras_df$metric)), "\n")
