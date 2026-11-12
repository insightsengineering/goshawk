# Requirement: Function to create a boxplot.
describe("g_boxplot", {
  adlb <- make_adlb()

  it("returns a ggplot object", {
    p <- g_boxplot(
      adlb,
      biomarker = "CRP",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID"
    )
    expect_s3_class(p, "ggplot")
  })

  it("works with units, facets, colors, shapes and fonts", {
    p <- g_boxplot(
      adlb,
      biomarker = "CRP",
      param_var = "PARAMCD",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      loq_flag_var = "LOQFL",
      loq_legend = FALSE,
      unit = "AVALU",
      color_manual = arm_colors,
      shape_manual = c("N" = 1, "Y" = 2),
      facet_var = "AVISITCD",
      facet_ncol = 2,
      alpha = 0.5,
      dot_size = 3,
      font_size = 10,
      rotate_xlab = TRUE
    )
    expect_s3_class(p, "ggplot")
    expect_match(p$labels$title, "mg/L")
    expect_match(p$labels$y, "mg/L")
  })

  it("handles facet_ncol not set, no box and an empty unit", {
    p <- g_boxplot(
      adlb,
      biomarker = "ALT",
      yaxis_var = "BASE",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      box = FALSE,
      facet_var = "AVISITCD",
      facet_ncol = NULL,
      ylim = c(0, 100)
    )
    expect_s3_class(p, "ggplot")

    adlb_nounit <- adlb
    adlb_nounit$AVALU <- ""
    p_nounit <- g_boxplot(
      adlb_nounit,
      biomarker = "CRP",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      unit = "AVALU"
    )
    expect_s3_class(p_nounit, "ggplot")
    expect_false(grepl("\\(", p_nounit$labels$y))
  })

  it("skips facetting for facet_var = 'None'", {
    p <- g_boxplot(
      adlb,
      biomarker = "CRP",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      facet_var = "None"
    )
    expect_s3_class(p$facet, "FacetNull")
  })

  it("takes the most common unit of a factor unit variable", {
    adlb_f <- adlb
    adlb_f$AVALU <- factor(adlb_f$AVALU)
    p <- g_boxplot(
      adlb_f,
      biomarker = "CRP",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      unit = "AVALU"
    )
    expect_match(p$labels$y, "mg/L")
  })

  it("adds horizontal lines from values and variables", {
    p <- g_boxplot(
      adlb,
      biomarker = "CRP",
      yaxis_var = "AVAL",
      trt_group = "ARM",
      xaxis_var = "STUDYID",
      hline_arb = c(0.9, 1.2),
      hline_arb_color = "blue",
      hline_arb_label = "Arbitrary",
      hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
      hline_vars_colors = c("pink", "brown", "purple", "gray"),
      hline_vars_labels = c("A", "B", "C", "D")
    )
    expect_s3_class(p, "ggplot")
    n_hline <- sum(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
    expect_equal(n_hline, 2)
  })

  it("validates its arguments", {
    expect_error(
      g_boxplot(
        adlb,
        biomarker = "CRP", param_var = "NOPE", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID"
      ),
      "is not in data"
    )
    expect_error(
      g_boxplot(adlb, biomarker = "XYZ", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID"),
      "is not found in param_var"
    )
    expect_error(
      g_boxplot(
        adlb,
        biomarker = "CRP", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID", unit = "NOPE"
      ),
      "is not in data"
    )
    expect_error(
      g_boxplot(
        adlb,
        biomarker = "CRP", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID", loq_legend = "yes"
      )
    )
    expect_error(
      g_boxplot(
        adlb,
        biomarker = "CRP", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID", dot_size = "big"
      )
    )
    expect_error(
      g_boxplot(
        adlb,
        biomarker = "CRP", yaxis_var = "AVAL", trt_group = "ARM", xaxis_var = "STUDYID", ylim = c(1, 2, 3)
      )
    )
  })
})
