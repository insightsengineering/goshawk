# Requirement: Function to create a density distribution plot.
adlb <- make_adlb()

test_that("g_density_distribution_plot returns a ggplot object", {
  p <- g_density_distribution_plot(
    data = adlb,
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL",
    trt_group = "ARM",
    unit = "AVALU"
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p), regexp = "Removed 2 rows", fixed = TRUE))
})

test_that("g_density_distribution_plot works with all options", {
  p <- g_density_distribution_plot(
    label = "Density",
    data = adlb,
    param_var = "PARAMCD",
    param = "ALT",
    xaxis_var = "AVAL",
    trt_group = "ARM",
    unit = "AVALU",
    loq_flag_var = "LOQFL",
    xlim = c(0, 200),
    ylim = c(0, 1),
    color_manual = arm_colors,
    color_comb = "#39ff14",
    comb_line = TRUE,
    facet_var = "AVISITCD",
    hline_arb = c(0.1, 0.2),
    hline_arb_color = c("red", "blue"),
    hline_arb_label = c("a", "b"),
    facet_ncol = 3,
    rotate_xlab = TRUE,
    font_size = 10,
    line_size = 1,
    rug_plot = TRUE
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_density_distribution_plot works without units and combined line", {
  p <- g_density_distribution_plot(
    data = adlb,
    param = "CRP",
    unit = NULL,
    comb_line = FALSE,
    font_size = NULL
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p), regexp = "Removed 1 row", fixed = TRUE))

  adlb_nounit <- adlb
  adlb_nounit$AVALU <- ""
  p_empty <- g_density_distribution_plot(data = adlb_nounit, param = "CRP", unit = "AVALU")
  expect_s3_class(p_empty, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p), regexp = "Removed 1 row", fixed = TRUE))
})

test_that("g_density_distribution_plot handles a manual color without the combined entry", {
  p <- g_density_distribution_plot(
    data = adlb,
    param = "CRP",
    color_manual = arm_colors,
    color_comb = NULL
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p), regexp = "Removed 2 rows", fixed = TRUE))

  p_with_comb <- g_density_distribution_plot(
    data = adlb,
    param = "CRP",
    color_manual = c(arm_colors, "Combined Dose" = "black"),
    color_comb = NULL
  )
  expect_s3_class(p_with_comb, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p_with_comb), regexp = "Removed 2 rows", fixed = TRUE))
})

test_that("g_density_distribution_plot uses the ggplot2.discrete.colour option", {
  withr::local_options(ggplot2.discrete.colour = c("red", "green", "blue", "black"))
  p <- g_density_distribution_plot(data = adlb, param = "CRP")
  expect_s3_class(p, "ggplot")
  expect_no_error(expect_warning(ggplot2::ggplot_build(p), regexp = "Removed 2 rows", fixed = TRUE))
})

test_that("g_density_distribution_plot validates its arguments", {
  expect_error(g_density_distribution_plot(data = adlb, param = "CRP", xlim = 1))
  expect_error(g_density_distribution_plot(data = adlb, param = "CRP", ylim = c(1, 2, 3)))
})
