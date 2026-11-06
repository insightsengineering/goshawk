# Requirement: Function to create a spaghetti plot.
adlb <- make_adlb()

test_that("g_spaghettiplot returns a ggplot object", {
  p <- g_spaghettiplot(
    data = adlb,
    subj_id = "USUBJID",
    biomarker_var = "PARAMCD",
    biomarker = "CRP",
    value_var = "AVAL",
    trt_group = "ARM",
    time = "AVISITCDN"
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_spaghettiplot works with continuous time and group statistics", {
  p <- g_spaghettiplot(
    data = adlb,
    biomarker = "CRP",
    trt_group = "ARM",
    time = "AVISITCDN",
    color_manual = arm_colors,
    color_comb = "#39ff14",
    alpha = 0.2,
    xtick = c(0, 1, 5),
    xlabel = c("Baseline", "Week 1", "Week 5"),
    rotate_xlab = TRUE,
    group_stats = "MEDIAN",
    hline_arb = c(0.5, 0.7),
    hline_arb_color = c("blue", "red"),
    hline_arb_label = c("Arb A", "Arb B"),
    hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
    hline_vars_colors = c("pink", "brown", "purple", "gray"),
    dot_size = 3
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_spaghettiplot supports mean statistics and discrete time", {
  p <- g_spaghettiplot(
    data = adlb,
    biomarker = "ALT",
    trt_group = "ARM",
    trt_group_level = c("C: Combination", "B: Placebo", "A: Drug X"),
    time = "AVISITCD",
    time_level = c("BL", "W 1", "W 5"),
    xtick = c("BL", "W 1", "W 5"),
    xlabel = c("Baseline", "Week 1", "Week 5"),
    group_stats = "MEAN",
    ylim = c(0, 100),
    facet_ncol = 3,
    font_size = 10
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_spaghettiplot supports all facet scales and no group statistics", {
  for (scales in c("fixed", "free", "free_x", "free_y")) {
    p <- g_spaghettiplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      time = "AVISITCD",
      facet_scales = scales,
      group_stats = "NONE",
      font_size = NULL
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("g_spaghettiplot uses the unit and handles an empty unit", {
  adlb_nounit <- adlb
  adlb_nounit$AVALU <- ""
  p <- g_spaghettiplot(data = adlb_nounit, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_match(p$labels$title, "mg/L")

  p_unit <- g_spaghettiplot(data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN")
  expect_match(p_unit$labels$title, "mg/L")
  expect_no_error(ggplot2::ggplot_build(p_unit))
})

test_that("g_spaghettiplot validates its arguments", {
  expect_error(
    g_spaghettiplot(data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN", ylim = 1)
  )
  expect_error(
    g_spaghettiplot(
      data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN", facet_scales = "other"
    )
  )
})
