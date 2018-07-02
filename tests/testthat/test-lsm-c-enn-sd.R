context("class level enn_sd metric")

fragstats_class_landscape_enn_sd <- fragstats_patch_landscape %>%
    group_by(TYPE) %>%
    summarise(metric = sd(ENN)) %>%
    pull(metric) %>%
    round(.,4)
landscapemetrics_class_landscape_enn_sd <- lsm_c_enn_sd(landscape)

test_that("lsm_c_enn_sd results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_enn_sd %in%
                        round(landscapemetrics_class_landscape_enn_sd$value, 4)))
})

test_that("lsm_c_enn_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_enn_sd, "tbl_df")
    expect_is(lsm_c_enn_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_enn_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_enn_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_enn_sd), 6)
})

test_that("lsm_c_enn_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_enn_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_enn_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_enn_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_enn_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_enn_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_enn_sd$value, "double")
})
