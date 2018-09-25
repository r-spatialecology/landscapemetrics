# #### Class level ####
#
# context("class level lsm_c_ai cv metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, AI)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_ai(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_ai results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_area_cv cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(AREA))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_area_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_area_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_area_mn metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, AREA_MN)
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_area_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_area_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_area_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(AREA))
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_area_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_area_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_ca metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, CA)
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_ca(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_ca results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_cai_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(CAI))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_cai_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_cai_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(CAI))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_cai_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_cai_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(CAI))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_cai_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_circle_cv metric")
#
# landscapemetrics_class_landscape_value <- lsm_c_circle_cv(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test missing
#
#
# context("class level lsm_c_circle_mn metric")
#
# landscapemetrics_class_landscape_value <- lsm_c_circle_mn(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test missing
#
#
# context("class level lsm_c_circle_mn metric")
#
# landscapemetrics_class_landscape_value <- lsm_c_circle_sd(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test missing
#
#
# context("class level lsm_c_clumpy metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, CLUMPY)
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_clumpy(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_clumpy results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_cohesion metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, COHESION)
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_cohesion(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_cohesion results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_contig_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(CONTIG))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_contig_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_contig_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_contig_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(CONTIG))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_contig_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_contig_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_contig_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(CONTIG))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_contig_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_contig_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_core_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(CORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_core_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_core_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(CORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_core_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_core_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(CORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_core_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_cpland metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, CPLAND)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_cpland(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_dcad metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, DCAD)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_dcad(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_cdore_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(NCORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_dcore_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_dore_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(NCORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_dcore_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_dore_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(NCORE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_dcore_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_cai_cv results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_division metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, DIVISION)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_division(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_division results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_ed metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, ED)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_ed(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_ed results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_enn_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(ENN))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_enn_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_enn_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_enn_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(ENN))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_enn_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_enn_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_enn_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(ENN))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_enn_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_enn_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_lsm_c_frac_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(FRAC))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_frac_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_frac_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_frac_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(FRAC))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_frac_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_frac_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level frac_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(FRAC))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_frac_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_frac_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_gyrate_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(GYRATE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_gyrate_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_gyrate_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_gyrate_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(GYRATE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_gyrate_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_gyrate_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_gyrate_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(GYRATE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_gyrate_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_gyrate_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_iji metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, IJI)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_iji(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_iji results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_lpi metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, LPI)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_lpi(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_lpi results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_lsi metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, LSI)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_lsi(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_lsi results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_mesh metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, MESH)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_mesh(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_mesh results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_ndca metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, NDCA)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_ndca(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_c_ndca results are equal to fragstats", {
# #     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# # })
#
#
# context("class level lsm_c_nlsi metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, NLSI)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_nlsi(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_nlsi results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_np metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, NP)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_np(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_np results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_pafrac metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, PAFRAC)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_pafrac(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_pafrac results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_para_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(PARA))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_para_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_para_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_para_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(PARA))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_para_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_para_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_para_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(PARA))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_para_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_para_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_pd metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, PD)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_pd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_pd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_pladj metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, PLADJ)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_pladj(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_pladj results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_pland metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, PLAND)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_pland(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_pland results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_shape_cv metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = raster::cv(SHAPE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_shape_cv(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_shape_cv results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_shape_mn metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = mean(SHAPE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_shape_mn(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_shape_mn results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_shape_sd metric")
#
# fragstats_class_landscape_value <- fragstats_patch_landscape %>%
#     dplyr::group_by(TYPE) %>%
#     dplyr::summarize(metric = sd(SHAPE))
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_shape_sd(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_shape_sd results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_split metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, SPLIT)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_split(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_split results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_tca metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, TCA)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_tca(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_tca results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# context("class level lsm_c_te metric")
#
# fragstats_class_landscape_value <- dplyr::select(fragstats_class_landscape,
#                                                  TYPE, TE)
#
# names(fragstats_class_landscape_value) <- c("class", "value")
#
# landscapemetrics_class_landscape_value <- lsm_c_te(landscape)
#
# comparison <- dplyr::full_join(x = fragstats_class_landscape_value,
#                                y = landscapemetrics_class_landscape_value,
#                                by = "class",
#                                suffix = c(".fs", ".lsm"))
#
# test_that("lsm_c_te results are equal to fragstats", {
#     expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
# })
#
#
# #### Landscape level ####
#
#
# context("landscape level lsm_l_ai metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$AI
# landscapemetrics_landscape_landscape_value <- lsm_l_ai(landscape)
#
# test_that("lsm_l_ai results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level area_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(AREA))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_area_cv(landscape)
#
# test_that("lsm_l_area_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level area_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(AREA))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_area_cv(landscape)
#
# test_that("lsm_l_area_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level area_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(AREA))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_area_sd(landscape)
#
# test_that("lsm_l_area_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_cai_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(CAI))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_cai_cv(landscape)
#
# # Different core algorithm
# # test_that("lsm_l_cai_cv results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_cai_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(CAI))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_cai_mn(landscape)
#
# # Different core algorithm
# # test_that("lsm_l_cai_mn results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_cai_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(CAI))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_cai_sd(landscape)
#
# # Different core algorithm
# # test_that("lsm_l_cai_sd results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_circle_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(CIRCLE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_circle_cv(landscape)
#
# # see https://r-spatialecology.github.io/landscapemetrics/ for more information
# # test_that("lsm_l_circle_cv results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_circle_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(CIRCLE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_circle_mn(landscape)
#
# # see https://r-spatialecology.github.io/landscapemetrics/ for more information
# # test_that("lsm_l_circle_mn results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_circle_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(CIRCLE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_circle_sd(landscape)
#
# # see https://r-spatialecology.github.io/landscapemetrics/ for more information
# # test_that("lsm_l_circle_sd results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_cohesion metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$COHESION
# landscapemetrics_landscape_landscape_value <- lsm_l_cohesion(landscape)
#
# test_that("lsm_l_cohesion results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_contag metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$CONTAG
# landscapemetrics_landscape_landscape_value <- lsm_l_contag(landscape)
#
# test_that("lsm_l_contag results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_contig_cv metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(CONTIG))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_contig(landscape), value = round(value, 4)), value = raster::cv(value))
#
# test_that("lsm_l_contig_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_contig_mn metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(CONTIG))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_contig(landscape), value = round(value, 4)), value = mean(value))
#
# test_that("lsm_l_contig_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_contig_sd metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(CONTIG))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_contig(landscape), value = round(value, 4)), value = sd(value))
#
# test_that("lsm_l_contig_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_core_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(CORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_core_cv(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_core_cv results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_core_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(CORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_core_mn(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_core_mn results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_core_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(CORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_core_sd(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_core_sd results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level _lsm_l_dcad metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$DCAD
# landscapemetrics_landscape_landscape_value <- lsm_l_dcad(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_dcad results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_dcore_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(NCORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_dcore_cv(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_dcore_cv results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_dcore_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(NCORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_dcore_mn(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_dcore_mn results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_dcore_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(NCORE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_dcore_sd(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/
# # test_that("lsm_l_dcore_sd results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_division metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$DIVISION
# landscapemetrics_landscape_landscape_value <- lsm_l_division(landscape)
#
# test_that("lsm_l_division results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_ed metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$ED
# landscapemetrics_landscape_landscape_value <- lsm_l_ed(landscape)
#
# test_that("lsm_l_ed results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_enn_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(ENN))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_enn_cv(landscape)
#
# test_that("lsm_l_enn_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_enn_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(ENN))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_enn_mn(landscape)
#
# test_that("lsm_l_enn_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_enn_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(ENN))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_enn_sd(landscape)
#
# test_that("lsm_l_enn_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_frac_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(FRAC))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_frac_cv(landscape)
#
# test_that("lsm_l_frac_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_frac_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(FRAC))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_frac_mn(landscape)
#
# test_that("lsm_l_frac_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_frac_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(FRAC))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_frac_sd(landscape)
#
# test_that("lsm_l_frac_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_gyrate_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(GYRATE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_gyrate_cv(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_l_gyrate_cv results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_gyrate_mn metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(GYRATE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_gyrate_mn(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_l_gyrate_mn results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_gyrate_sd metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(GYRATE))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_gyrate_sd(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_l_gyrate_sd results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_lpi metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$LPI
# landscapemetrics_landscape_landscape_value <- lsm_l_lpi(landscape)
#
# test_that("lsm_l_lpi results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_lsi metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$LSI
# landscapemetrics_landscape_landscape_value <- lsm_l_lsi(landscape)
#
# test_that("lsm_l_lsi results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_mesh metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$MESH
# landscapemetrics_landscape_landscape_value <- lsm_l_mesh(landscape)
#
# test_that("lsm_l_mesh results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_msidi metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$MSIDI
# landscapemetrics_landscape_landscape_value <- lsm_l_msidi(landscape)
#
# test_that("lsm_l_msidi results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_msiei metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$MSIEI
# landscapemetrics_landscape_landscape_value <- lsm_l_msiei(landscape)
#
# test_that("lsm_l_msiei results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_ndca metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$NDCA
# landscapemetrics_landscape_landscape_value <- lsm_l_ndca(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_l_ndca results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_np metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$NP
# landscapemetrics_landscape_landscape_value <- lsm_l_np(landscape)
#
# test_that("lsm_l_np results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_pafrac metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$PAFRAC
# landscapemetrics_landscape_landscape_value <- lsm_l_pafrac(landscape)
#
# test_that("lsm_l_pafrac results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_para_cv metric")
#
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(PARA))
#
# landscapemetrics_landscape_landscape_value <- lsm_l_para_cv(landscape)
#
# test_that("lsm_l_para_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_para_mn metric")
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(PARA) / 10000)
#
# landscapemetrics_landscape_landscape_value <- lsm_l_para_mn(landscape)
#
# test_that("lsm_l_para_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_para_sd metric")
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(PARA) / 10000)
#
# landscapemetrics_landscape_landscape_value <- lsm_l_para_sd(landscape)
#
# test_that("lsm_l_para_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_pd metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$PD
# landscapemetrics_landscape_landscape_value <- lsm_l_pd(landscape)
#
# test_that("lsm_l_pd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_pladj metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$PLADJ
# landscapemetrics_landscape_landscape_value <- lsm_l_pladj(landscape)
#
# test_that("lsm_l_pladj results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_pr metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$PR
# landscapemetrics_landscape_landscape_value <- lsm_l_pr(landscape)
#
# test_that("lsm_l_pr results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_prd metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$NP
# landscapemetrics_landscape_landscape_value <- lsm_l_prd(landscape)
#
# test_that("lsm_l_prd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_rpr metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$RPR
# landscapemetrics_landscape_landscape_value <- lsm_l_rpr(landscape, classes_max = 5)
#
# test_that("lsm_l_rpr results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_shape_cv metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = raster::cv(SHAPE))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_shape(landscape), value = round(value, 4)), value = raster::cv(value))
#
# test_that("lsm_l_shape_cv results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_shape_mn metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = mean(SHAPE))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_shape(landscape), value = round(value, 4)), value = mean(value))
#
# test_that("lsm_l_shape_mn results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_shape_sd metric")
#
# # FRAGSTATS already rounds on patch level
# fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
#     dplyr::summarize(value = sd(SHAPE))
#
# landscapemetrics_landscape_landscape_value <- dplyr::summarize(dplyr::dplyr::mutate(lsm_p_shape(landscape), value = round(value, 4)), value = sd(value))
#
# test_that("lsm_l_shape_sd results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_shdi metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SHDI
# landscapemetrics_landscape_landscape_value <- lsm_l_shdi(landscape)
#
# test_that("lsm_l_shdi results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_shei metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SHEI
# landscapemetrics_landscape_landscape_value <- lsm_l_shei(landscape)
#
# test_that("lsm_l_shei results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_sidi metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SIDI
# landscapemetrics_landscape_landscape_value <- lsm_l_sidi(landscape)
#
# test_that("lsm_l_sidi results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_siei metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SIEI
# landscapemetrics_landscape_landscape_value <- lsm_l_siei(landscape)
#
# test_that("lsm_l_siei results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_split metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SPLIT
# landscapemetrics_landscape_landscape_value <- lsm_l_split(landscape)
#
# test_that("lsm_l_split results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_ta metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$TA
# landscapemetrics_landscape_landscape_value <- lsm_l_ta(landscape)
#
# test_that("lsm_l_ta results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# context("landscape level lsm_l_tca metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$TCA
# landscapemetrics_landscape_landscape_value <- lsm_l_tca(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_l_tca results are equal to fragstats", {
# #     expect_true(round(fragstats_landscape_landscape_value, 4) ==
# #                     round(landscapemetrics_landscape_landscape_value$value, 4))
# # })
#
#
# context("landscape level lsm_l_te metric")
#
# fragstats_landscape_landscape_value <- fragstats_landscape_landscape$TE
# landscapemetrics_landscape_landscape_value <- lsm_l_te(landscape)
#
# test_that("lsm_l_te results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })
#
#
# #### Patch level ####
#
#
# context("patch level lsm_p_area metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$AREA
# landscapemetrics_patch_landscape_value <- lsm_p_area(landscape)
#
# test_that("lsm_p_area results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_cai metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$CAI
# landscapemetrics_patch_landscape_value <- lsm_p_cai(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_p_cai results are equal to fragstats", {
# #     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
# #                         round(landscapemetrics_patch_landscape_value$value, 4)))
# # })
#
#
# context("patch level lsm_p_circle metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$CIRCLE
# landscapemetrics_patch_landscape_value <- lsm_p_circle(landscape)
#
# test_that("lsm_p_circle results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_contig metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$CONTIG
# landscapemetrics_patch_landscape_value <- lsm_p_contig(landscape)
#
# test_that("lsm_p_contig results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_core metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$CORE
# landscapemetrics_patch_landscape_value <- lsm_p_core(landscape)
#
# # See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# # test_that("lsm_p_core results are equal to fragstats", {
# #     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
# #                         round(landscapemetrics_patch_landscape_value$value, 4)))
# # })
#
#
# context("patch level lsm_p_enn metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$ENN
# landscapemetrics_patch_landscape_value <- lsm_p_enn(landscape)
#
# test_that("lsm_p_enn results are comparable to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_frac metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$FRAC
# landscapemetrics_patch_landscape_value <- lsm_p_frac(landscape)
#
# test_that("lsm_p_frac results are comparable to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_gyrate metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$GYRATE
# landscapemetrics_patch_landscape_value <- lsm_p_gyrate(landscape)
#
# test_that("lsm_p_gyrate results are comparable to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_nca metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$NCORE
# landscapemetrics_patch_landscape_value <- lsm_p_ncore(landscape)
#
# test_that("lsm_p_ncore results are comparable to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_para metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$PARA
# landscapemetrics_patch_landscape_value <- lsm_p_para(landscape)
#
# test_that("lsm_p_para results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_perim metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$PERIM
# landscapemetrics_patch_landscape_value <- lsm_p_perim(landscape)
#
# test_that("lsm_p_perim results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
#
# context("patch level lsm_p_shape metric")
#
# fragstats_patch_landscape_value <- fragstats_patch_landscape$SHAPE
# landscapemetrics_patch_landscape_value <- lsm_p_shape(landscape)
#
# test_that("lsm_p_shape results are equal to fragstats", {
#     expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
#                         round(landscapemetrics_patch_landscape_value$value, 4)))
# })
#
# #### END ####
