# Requirement: Function to create a correlation plot.
corr <- make_corr_data()

corr_args <- function(...) {
  args <- list(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT"
  )
  utils::modifyList(args, list(...))
}

test_that("g_correlationplot returns a ggplot object", {
  p <- g_correlationplot(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT"
  )
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Correlation of ALT to CRP")
})

test_that("g_correlationplot supports the different facetting options", {
  p_visit_arm <- g_correlationplot(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT",
    visit_facet = TRUE,
    facet = TRUE,
    facet_var = "ARM")
  expect_s3_class(p_visit_arm, "ggplot")
  expect_s3_class(p_visit_arm$facet, "Facet")

  p_visit <- g_correlationplot(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT",
    visit_facet = TRUE,
    facet = TRUE,
    facet_var = "ARM")
  expect_s3_class(p_visit$facet, "Facet")

  p_arm <- g_correlationplot(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT",
    facet = TRUE,
    facet_var = "ARM",
    visit_facet = FALSE)
  expect_s3_class(p_arm$facet, "Facet")

  p_none <- g_correlationplot(
    data = corr,
    param_var = "PARAMCD",
    xaxis_param = "CRP",
    xaxis_var = "AVAL",
    xvar = "AVAL",
    yaxis_param = "ALT",
    yaxis_var = "BASE",
    yvar = "BASE",
    trt_group = "ARM",
    visit = "AVISITCD",
    loq_flag_var = "LOQFL_COMB",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT",
    facet = FALSE,
    visit_facet = FALSE,
    facet_var = "ARM")

  expect_s3_class(p_none$facet, "FacetNull")
})

test_that("Example", {
  skip_if_not_installed("tidyr")
  library(stringr)
  library(tidyr)

  # original ARM value = dose value
  arm_mapping <- list(
    "A: Drug X" = "150mg QD",
    "B: Placebo" = "Placebo",
    "C: Combination" = "Combination"
  )
  color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
  # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
  shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)

  ADLB <- rADLB
  var_labels <- lapply(ADLB, function(x) attributes(x)$label)
  ADLB <- ADLB |>
    mutate(AVISITCD = case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~
        paste(
          "W",
          trimws(
            substr(
              AVISIT,
              start = 6,
              stop = str_locate(AVISIT, "DAY") - 1
            )
          )
        ),
      TRUE ~ NA_character_
    )) |>
    mutate(AVISITCDN = case_when(
      AVISITCD == "SCR" ~ -2,
      AVISITCD == "BL" ~ 0,
      grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
      TRUE ~ NA_real_
    )) |>
    # use ARMCD values to order treatment in visualization legend
    mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
      ifelse(grepl("B", ARMCD), 2,
        ifelse(grepl("A", ARMCD), 3, NA)
      )
    )) |>
    mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) |>
    mutate(ARM = factor(ARM) |>
      reorder(TRTORD)) |>
    mutate(
      ANRHI = case_when(
        PARAMCD == "ALT" ~ 60,
        PARAMCD == "CRP" ~ 70,
        PARAMCD == "IGA" ~ 80,
        TRUE ~ NA_real_
      ),
      ANRLO = case_when(
        PARAMCD == "ALT" ~ 20,
        PARAMCD == "CRP" ~ 30,
        PARAMCD == "IGA" ~ 40,
        TRUE ~ NA_real_
      )
    ) |>
    rowwise() |>
    group_by(PARAMCD) |>
    mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
    )) |>
    mutate(LBSTRESC = ifelse(
      USUBJID %in% sample(USUBJID, 1, replace = TRUE),
      paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
    )) |>
    ungroup()
  attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
  attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
  attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"

  # add LLOQ and ULOQ variables
  ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB, flag_var = "LOQFL")
  ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")

  # given the 2 param and 2 analysis vars we need to transform the data
  plot_data_t1 <- suppressWarnings(ADLB |>
    gather(
      ANLVARS, ANLVALS, PARAM, LBSTRESC, BASE2, BASE, AVAL, BASE, LOQFL,
      ANRHI, ANRLO, ULOQN, LLOQN
    ) |>
    mutate(ANL.PARAM = ifelse(ANLVARS %in% c("PARAM", "LBSTRESC", "LOQFL"),
      paste0(ANLVARS, "_", PARAMCD),
      paste0(ANLVARS, ".", PARAMCD)
    )) |>
    select(USUBJID, ARM, ARMCD, AVISITN, AVISITCD, ANL.PARAM, ANLVALS) |>
    spread(ANL.PARAM, ANLVALS))

  # the transformed analysis value variables are character and need to be converted to numeric for
  # ggplot
  # remove records where either of the analysis variables are NA since they will not appear on the
  # plot and will ensure that LOQFL = NA level is removed
  plot_data_t2 <- plot_data_t1 |>
    filter(!is.na(BASE.CRP) & !is.na(AVAL.ALT)) |>
    mutate_at(vars(contains(".")), as.numeric) |>
    mutate(
      LOQFL_COMB = ifelse(LOQFL_CRP == "Y" | LOQFL_ALT == "Y", "Y", "N")
    )

  expect_warning(p <- g_correlationplot(
    label = "Correlation Plot",
    data = plot_data_t2,
    param_var = "PARAMCD",
    xaxis_param = c("CRP"),
    xaxis_var = "AVAL",
    xvar = "AVAL.CRP",
    yaxis_param = c("ALT"),
    yaxis_var = "BASE",
    yvar = "BASE.ALT",
    trt_group = "ARM",
    visit = "AVISITCD",
    visit_facet = TRUE,
    loq_legend = TRUE,
    unit = "AVALU",
    title_text = "Correlation of ALT to CRP",
    xaxis_lab = "CRP",
    yaxis_lab = "ALT",
    color_manual = color_manual,
    shape_manual = shape_manual,
    facet_ncol = 4,
    facet = FALSE,
    facet_var = "ARM",
    reg_line = FALSE,
    hline_arb = c(15, 25),
    hline_arb_color = c("gray", "green"),
    hline_arb_label = "Hori_line_label",
    vline_arb = c(.5, 1),
    vline_arb_color = c("red", "black"),
    vline_arb_label = c("Vertical Line A", "Vertical Line B"),
    hline_vars = c("ANRHI.ALT", "ANRLO.ALT", "ULOQN.ALT", "LLOQN.ALT"),
    hline_vars_colors = c("green", "blue", "purple", "cyan"),
    hline_vars_labels = c("ANRHI ALT Label", "ANRLO ALT Label", "ULOQN ALT Label", "LLOQN ALT Label"),
    vline_vars = c("ANRHI.CRP", "ANRLO.CRP", "ULOQN.CRP", "LLOQN.CRP"),
    vline_vars_colors = c("yellow", "orange", "brown", "gold"),
    vline_vars_labels = c("ANRHI CRP Label", "ANRLO CRP Label", "ULOQN CRP Label", "LLOQN CRP Label"),
    rotate_xlab = FALSE,
    font_size = 14,
    dot_size = 2,
    reg_text_size = 3
  ), "First value is taken from variable", fixed = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})
