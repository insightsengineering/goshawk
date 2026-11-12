# Requirement: Function to create line plot of summary statistics over time.
describe("g_lineplot", {
  adlb <- make_adlb()
  line_types <- c("A: Drug X" = "solid", "B: Placebo" = "dashed", "C: Combination" = "dotted")

  it("returns a combined plot with the mean table", {
    p <- g_lineplot(
      data = adlb,
      biomarker_var = "PARAMCD",
      biomarker = "CRP",
      value_var = "AVAL",
      trt_group = "ARM",
      time = "AVISITCDN"
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("works with median, manual styling and horizontal lines", {
    p <- g_lineplot(
      label = "Line Plot",
      data = adlb,
      biomarker = "ALT",
      value_var = "AVAL",
      trt_group = "ARM",
      trt_group_level = c("C: Combination", "B: Placebo", "A: Drug X"),
      time = "AVISITCDN",
      color_manual = arm_colors,
      line_type = line_types,
      median = TRUE,
      hline_arb = c(40, 60),
      hline_arb_color = c("green", "red"),
      hline_arb_label = c("A", "B"),
      xtick = c(0, 1, 5),
      xlabel = c("Baseline", "Week 1", "Week 5"),
      rotate_xlab = TRUE,
      ylim = c(0, 100),
      plot_font_size = 10,
      dot_size = 2,
      dodge = 0.2,
      plot_height = 800,
      table_font_size = 10
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("supports discrete time and hides the central value table", {
    p <- g_lineplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      time = "AVISITCD",
      time_level = c("BL", "W 1", "W 5"),
      xtick = c("BL", "W 1", "W 5"),
      xlabel = c("Baseline", "Week 1", "Week 5"),
      display_center_tbl = FALSE
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))

    p_chr <- g_lineplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      time = "AVISITCD",
      rotate_xlab = TRUE
    )
    expect_s3_class(p_chr, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("supports a shape variable", {
    p <- g_lineplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      shape = "SEX",
      time = "AVISITCDN",
      color_manual = arm_colors,
      line_type = line_types,
      plot_height = 3000,
      dot_size = 1
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))

    p_shape_type <- g_lineplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      shape = "SEX",
      shape_type = c("F" = 16, "M" = 17),
      time = "AVISITCDN",
      median = TRUE,
      plot_height = 3000
    )
    expect_s3_class(p_shape_type, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p_shape_type))
  })

  it("repeats default shapes when there are many levels", {
    adlb_many <- adlb
    adlb_many$GRP <- factor(rep(sprintf("g%02d", 1:20), length.out = nrow(adlb_many)))
    suppressWarnings(p <- g_lineplot(
      data = adlb_many,
      biomarker = "CRP",
      trt_group = "ARM",
      shape = "GRP",
      time = "AVISITCDN",
      plot_height = 20000
    ))
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("marks cells below the count threshold", {
    p <- g_lineplot(
      data = adlb,
      biomarker = "CRP",
      trt_group = "ARM",
      time = "AVISITCDN",
      count_threshold = 4
    )
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("uses the ggplot2.discrete.colour option", {
    withr::local_options(ggplot2.discrete.colour = c("red", "green", "blue", "black"))
    p <- g_lineplot(data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN")
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  })

  it("validates its arguments", {
    expect_error(
      g_lineplot(data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN", ylim = 1)
    )
    expect_error(
      g_lineplot(
        data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN",
        color_manual = c("A: Drug X" = "red")
      )
    )
    expect_error(
      g_lineplot(
        data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN",
        line_type = c("A: Drug X" = "solid")
      )
    )
    expect_error(
      g_lineplot(
        data = adlb, biomarker = "CRP", trt_group = "ARM", shape = "SEX", time = "AVISITCDN",
        shape_type = c("F" = 16), plot_height = 3000
      )
    )
    expect_error(
      g_lineplot(data = adlb, biomarker = "CRP", trt_group = "ARM", time = "AVISITCDN", plot_height = 100),
      "not sufficient"
    )
  })
})
