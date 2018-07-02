context("landscape level para_sd metric")

fragstats_landscape_landscape_para_sd <- fragstats_patch_landscape %>%
    summarise(metric = sd(PARA)) %>%
    pull(metric) %>%
    round(.,2)
landscapemetrics_landscape_landscape_para_sd <- lsm_l_para_sd(landscape)

test_that("lsm_l_para_sd results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_para_sd %in%
                        round(landscapemetrics_landscape_landscape_para_sd$value * 10000, 2)))
})

test_that("lsm_l_para_sd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_para_sd, "tbl_df")
    expect_is(lsm_l_para_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_para_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_para_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_para_sd), 6)
})

test_that("lsm_l_para_sd returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_para_sd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_para_sd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_para_sd$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_para_sd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_para_sd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_para_sd$value, "double")
})


