library(purrr)
library(stringr)
library(dplyr)
get_extras_per_file <- function(input_fun, lsm_dir, all_extras){
    my_files = paste0(lsm_dir, "R/", input_fun, ".R")
    t1 = readLines(my_files)
    new_df = data.frame(metric = input_fun,
               extras = stringr::str_replace_all(all_extras, "extras\\\\\\$", ""))
    new_df$used = unlist(lapply(paste0("\\b", all_extras, "\\b\\)?"),  # Add word boundaries
                                 \(x) any(stringr::str_detect(t1, x))))
    new_df = subset(new_df, used)
    return(new_df)
}

all_lsms <- landscapemetrics::list_lsm()

# 1. check which functions are using extras directly
all_extras <- c("extras\\$points", "extras\\$classes",
               "extras\\$class_patches", "extras\\$area_patches", "extras\\$neighbor_matrix",
               "extras\\$composition_vector", "extras\\$comp", "extras\\$cplx",
               "extras\\$enn_patch", "extras\\$perimeter_patch")
db_extras1 <- map_df(all_lsms$function_name,
    get_extras_per_file,
    lsm_dir = "~/Software/landscapemetrics/",
    all_extras = all_extras)

# 2. check which functions are using extras indirectly
get_int_functions <- function(function_name1){
    function_name2 = paste0(function_name1, "_calc")
    int_functions2 = codetools::findGlobals(eval(parse(text = paste0("landscapemetrics:::", function_name2))), merge = FALSE)$functions
    int_functions2 = stringr::str_replace_all(int_functions2, "\\_calc", "")
    data.frame(mainmetric = function_name1, usedmetric = int_functions2)
}

sel_intfuns_1degree <- map_df(all_lsms$function_name, get_int_functions) |>
    filter(str_detect(usedmetric, "^lsm"))
sel_intfuns_2degree = map_df(unique(sel_intfuns_1degree$usedmetric), get_int_functions) |>
    filter(str_detect(usedmetric, "^lsm"))
sel_intfuns_2degree = left_join(sel_intfuns_1degree, sel_intfuns_2degree, by = c("usedmetric" = "mainmetric"),
    relationship = "many-to-many") |>
    select(mainmetric, usedmetric = usedmetric.y) |>
    filter(!is.na(usedmetric))

sel_intfuns <- rbind(sel_intfuns_1degree, sel_intfuns_2degree)

# 3. join them
db_extras2 <- left_join(sel_intfuns, db_extras1, by = c("usedmetric" = "metric"),
    relationship = "many-to-many") |>
    filter(!is.na(used)) |>
    select(metric = mainmetric, extras, used)

extras_df <- rbind(db_extras1, db_extras2) |> select(-used) |>
    distinct(metric, extras) |>
    arrange(metric)

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
